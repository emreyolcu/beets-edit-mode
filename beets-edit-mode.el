;;; beets-edit-mode.el --- Major mode for beets edit buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emre Yolcu

;; Author: Emre Yolcu <mail@emreyolcu.com>
;; URL: https://github.com/emreyolcu/beets-edit-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data

;; beets-edit-mode is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; beets-edit-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with beets-edit-mode.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; beets-edit-mode is a major mode for the metadata editing buffers created by
;; the beets `edit' plugin.  It makes beets' own metadata editing more pleasant
;; inside Emacs: the ids that beets needs are guarded, stray typography can be
;; normalized, and `beets-edit-compact-mode' renders the documents as a table.
;; The mode never touches your library: it only shapes the buffer text, which
;; beets reads back and applies as usual when the edit ends.
;;
;; The plugin's buffers have random temporary file names, so nothing associates
;; them with this mode automatically.  To opt in to content-based detection, add
;; to your init file:
;;
;;   (add-to-list 'magic-mode-alist '(beets-edit-buffer-p . beets-edit-mode))

;;; Code:

(require 'ucs-normalize)

(autoload 'beets-edit-compact-mode "beets-edit-compact"
  "Toggle a compact, one-row-per-document view of a beets edit buffer." t)

(defgroup beets-edit nil
  "Major mode for beets edit buffers."
  :group 'tools
  :prefix "beets-edit-")

(defcustom beets-edit-normalize-rules
  '(("[‐‑‒–—―−]" . "-")
    ("…" . "...")
    ("[  ]" . " ")
    ("[­​﻿]" . "")
    ("\\(\\w\\)'\\(\\w\\)" . "\\1’\\2")
    ("[Ⅰ-ⅿﬁﬂ]" . ucs-normalize-NFKC-string)
    ("⁄" . "/")
    ("[ \t]+$" . ""))
  "Rules applied by `beets-edit-normalize'.

Each element is a cons (REGEXP . REPLACEMENT), applied in order
with `replace-match'.  REPLACEMENT is either a string, which may
use backreferences, or a function called with the matched text
whose result is inserted literally.

The defaults normalize characters that MusicBrainz editors commonly
introduce but an ASCII keyboard cannot type, following the character
table behind Picard's option for converting Unicode punctuation: hyphen,
dash, and minus variants become \"-\", the horizontal ellipsis becomes
\"...\", the no-break spaces become plain spaces, and the invisible soft
hyphen, zero width space, and byte order mark are removed.  The
apostrophe rule follows the MusicBrainz style preference for the
typographic apostrophe, rewriting a straight apostrophe between word
characters to \"’\".  Roman numeral codepoints and the fi and fl
ligatures decompose to ordinary letters, the fraction slash becomes
\"/\", and trailing whitespace is deleted.  Curly quotation marks,
letter apostrophes such as the Hawaiian okina, and CJK punctuation are
deliberately left alone."
  :type '(alist :key-type regexp :value-type (choice string function)))

(defconst beets-edit-font-lock-keywords
  '(("^id: -?[0-9]+\n"
     0 '(face nil invisible beets-edit-id rear-nonsticky t) t)
    ("^\\([a-z0-9_]+\\):" 1 'font-lock-variable-name-face)
    ("^---$" 0 'font-lock-comment-face)
    ("^ *\\(\t+\\)" 1 'error))
  "Font-lock keywords for `beets-edit-mode'.

Field names start in column zero, so indented continuation lines
of long values are intentionally not matched.  The `id' line is
made invisible, newline included, so that the row does not
compete visually with the `---' separators; the id is instead
shown by `beets-edit--annotate-region' as a dim annotation at the
end of the first line of its document, and the raw lines can be
revealed with `beets-edit-toggle-ids'.  Tabs in indentation are
highlighted as errors because YAML forbids them there.")

(defconst beets-edit--id-regexp "^id: \\(-?[0-9]+\\)$"
  "Regexp matching an id line, with the id in group one.")

(defvar-local beets-edit--inhibit-id-annotations nil
  "Non-nil when another display owns the ids, suppressing annotations.
The compact view sets this for its lifetime: it carries each id inside
its row.")

(defconst beets-edit--value-block-regexp
  "\\(?:\n\\(?:[ \t]+\\|- \\).*\\)*"
  "Regexp matching a value's continuation lines and list items.")

(defun beets-edit--field-line-regexp (field)
  "Return a regexp matching FIELD's line, its value in group one."
  (concat "^" (regexp-quote field) ": ?\\(.*\\)$"))

(defun beets-edit--fold-value (value end)
  "Fold the continuation lines after point into VALUE, up to END.
Point is just past a field line holding VALUE; wrapped lines fold in
with spaces and list items join with \"; \", matching the compact view's
display.  Return the folded value, point after the last folded line."
  (let ((case-fold-search nil))
    (while (and (< (point) (or end (point-max)))
                (looking-at "\\(?:[ \t]+\\(.*\\)\\|- \\(.*\\)\\)$"))
      (let ((continuation (match-string-no-properties 1))
            (item (match-string-no-properties 2)))
        (setq value (cond ((null item) (concat value " " continuation))
                          ((string-empty-p value) item)
                          (t (concat value "; " item)))))
      (forward-line 1)))
  value)

(defun beets-edit--current-field ()
  "Return the name of the field at or above point, or nil.
On an indented continuation line, the field is the one whose value the
line continues."
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not (bobp)) (looking-at "\\(?:[ \t]\\|- \\)"))
        (forward-line -1))
      (and (looking-at "\\([a-z0-9_]+\\):")
           (match-string-no-properties 1)))))

(defun beets-edit--document-start ()
  "Return the start position of the document around point."
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward "^---$" nil t)
        (progn (forward-line 1) (point))
      (point-min))))

(defun beets-edit--previous-document-start (&optional n)
  "Return the start of the Nth previous document, without moving point.
N defaults to 1.  Signal a `user-error' when the count overshoots the
first document, so callers moving by count fail in place."
  (save-excursion
    (dotimes (_ (or n 1))
      (let ((start (beets-edit--document-start)))
        (when (= start (point-min))
          (user-error "No previous document"))
        (goto-char (1- start))))
    (beets-edit--document-start)))

(defun beets-edit--goto-field (field)
  "Move point to the value of FIELD in the document at point.
Assume point is at the start of the document.  Return non-nil if FIELD
was found; otherwise leave point in place."
  (let ((case-fold-search nil))
    (re-search-forward (concat "^" (regexp-quote field) ": ?")
                       (beets-edit--document-end (point)) t)))

(defun beets-edit--document-end (start)
  "Return the end of the document starting at START.
The end is the separator's beginning, or the end of the buffer."
  (save-excursion
    (goto-char start)
    (if (re-search-forward "^---$" nil t)
        (match-beginning 0)
      (point-max))))

(defun beets-edit--map-documents (function)
  "Call FUNCTION with the START and END of every document, in order.
END excludes the separator.  Return the list of results."
  (let (results)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point))
              (end (beets-edit--document-end (point))))
          (push (funcall function start end) results)
          (goto-char end)
          (forward-line 1))))
    (nreverse results)))

(defun beets-edit--field-names ()
  "Return the field names present in the current buffer.
The id is not among them: it names documents, and no command should
offer to rewrite it."
  (let ((case-fold-search nil)
        names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-z0-9_]+\\):" nil t)
        (let ((name (match-string-no-properties 1)))
          (unless (or (equal name "id") (member name names))
            (push name names)))))
    (nreverse names)))

(defun beets-edit--write-field (field value &optional bound)
  "Replace the next occurrence of FIELD with VALUE, before BOUND.
Search from point; the old value's continuation lines and list items go
with it.  A value that was a YAML list is written back as one, with
VALUE split on \"; \", mirroring how `beets-edit--field-value' folds
list items for display; a scalar stays a scalar.  Return non-nil when a
replacement was made."
  (let ((case-fold-search nil))
    (when (re-search-forward (concat "^" (regexp-quote field) ": ?.*"
                                     beets-edit--value-block-regexp)
                             bound t)
      (replace-match
       ;; `split-string' clobbers the match data replace-match needs.
       (save-match-data
         (if (string-match-p "\n- " (match-string 0))
             (concat field ":\n"
                     (mapconcat (lambda (item) (concat "- " item))
                                (split-string value "; ") "\n"))
           (concat field ": " value)))
       t t)
      t)))

(defun beets-edit--set-field (field value)
  "Set FIELD to VALUE in every document in the current buffer.
A document that lacks FIELD gains it at its end.  Return the number of
documents changed."
  (let ((case-fold-search nil)
        (count 0)
        (more t))
    (save-excursion
      (goto-char (point-min))
      (while more
        (let ((end (beets-edit--document-end (point))))
          (unless (beets-edit--write-field field value end)
            (goto-char end)
            ;; The buffer may lack a final newline.
            (unless (bolp) (insert "\n"))
            (insert field ": " value "\n")))
        (setq count (1+ count))
        ;; Not `eobp': a separator ending the buffer, left by a deleted last
        ;; document, starts no further document.
        (setq more (and (re-search-forward "^---$" nil t)
                        (zerop (forward-line 1))
                        (not (eobp))))))
    count))

(defun beets-edit--read-field (prompt table)
  "Read a field name with completion over TABLE, prompting with PROMPT.
A name outside TABLE is allowed after a confirming RET, so new fields
can be added; it must still look like a beets field, and the id, which
beets owns, is refused."
  (let ((field (completing-read prompt table nil 'confirm)))
    (unless (let ((case-fold-search nil))
              (string-match-p "\\`[a-z0-9_]+\\'" field))
      (user-error "Field names are lowercase letters, digits, and underscores"))
    (when (equal field "id")
      (user-error "The id names the document; beets owns it"))
    field))

(defun beets-edit--field-value (field &optional start end)
  "Return FIELD's folded value between START and END, or nil.
Continuation lines fold in with spaces and list items join with \"; \",
matching the compact view's display of the value."
  (save-excursion
    (goto-char (or start (point-min)))
    (when (let ((case-fold-search nil))
            (re-search-forward (beets-edit--field-line-regexp field) end t))
      (let ((value (match-string-no-properties 1)))
        (forward-line 1)
        (beets-edit--fold-value value end)))))

(defun beets-edit--clean-value (value)
  "Return VALUE without a trailing newline, rejecting inner ones.
Field values are single lines."
  (setq value (string-trim-right value "\n+"))
  (when (string-match-p "\n" value)
    (user-error "Field values are single lines"))
  value)

(defun beets-edit--read-value (field current)
  "Edit FIELD's value CURRENT in a dedicated buffer and return it.
Uses `string-edit'; its edit buffer derives from Text mode, so minor
modes such as `electric-quote-local-mode' work there in full, unlike in
the minibuffer.  The buffer's `electric-quote-mode' state is carried
over.  Signal `user-error' on abort or when the value is not a single
line.

Calls `string-edit' directly rather than through
`read-string-from-buffer' because the field name belongs on the edit
buffer's header line, which `string-edit' overwrites after running the
mode hooks."
  (let ((electric (bound-and-true-p electric-quote-mode))
        (value nil)
        (aborted nil))
    (string-edit nil (or current "")
                 (lambda (edited)
                   (setq value edited)
                   (exit-recursive-edit))
                 :abort-callback (lambda ()
                                   (setq aborted t)
                                   (exit-recursive-edit)))
    ;; `string-edit' has selected the edit buffer's window and installed its own
    ;; header line; replace it with ours.
    (with-current-buffer (window-buffer (selected-window))
      (electric-quote-local-mode (if electric 1 -1))
      ;; Point after the initial contents, as in the minibuffer.
      (goto-char (point-max))
      (setq header-line-format
            (substitute-command-keys
             (format " Edit %s: \\[string-edit-done] to apply, \\[string-edit-abort] to cancel"
                     field))))
    (let ((edit-buffer (window-buffer (selected-window))))
      (unwind-protect
          (recursive-edit)
        ;; Escapes such as C-M-c or abort-recursive-edit bypass both string-edit
        ;; callbacks and would leak the edit buffer.
        (when (buffer-live-p edit-buffer)
          (quit-windows-on edit-buffer t))))
    (when (or aborted (null value))
      (user-error "Edit cancelled"))
    (beets-edit--clean-value value)))

(defun beets-edit-set-field (field value)
  "Set FIELD to VALUE in every document in the buffer.

Interactively, complete FIELD from the fields present in the
buffer; a name outside them, confirmed, adds a new field.  VALUE is
read with the field's first value in the buffer as the default, and
inserted verbatim; supplying any YAML quoting it may need is the
caller's responsibility."
  (interactive
   (let* ((field (beets-edit--read-field "Field: "
                                         (beets-edit--field-names)))
          (current (beets-edit--field-value field))
          (value (beets-edit--read-value field current)))
     (list field value)))
  (let ((count (beets-edit--set-field field value)))
    (message (ngettext "Set %s in %d document" "Set %s in %d documents" count)
             field count)))

(defun beets-edit--normalize (beg end)
  "Normalize the text between BEG and END.
Compose the region to Unicode normalization form C, then apply
`beets-edit-normalize-rules'.  Return the number of rule replacements
made."
  (let ((count 0)
        (bound (copy-marker end)))
    (save-excursion
      (ucs-normalize-NFC-region beg bound)
      (dolist (rule beets-edit-normalize-rules)
        (goto-char beg)
        (while (re-search-forward (car rule) bound t)
          (let ((from (match-beginning 0))
                (rep (cdr rule)))
            (if (stringp rep)
                (replace-match rep t)
              (replace-match (save-match-data (funcall rep (match-string 0)))
                             t t))
            ;; Resume one character back so a match may reuse the last character
            ;; of this one, as consecutive apostrophes sharing a word character
            ;; require.
            (goto-char (max from (1- (point)))))
          (setq count (1+ count)))))
    (set-marker bound nil)
    count))

(defun beets-edit-normalize (beg end)
  "Normalize typography between BEG and END.
Compose the text to Unicode normalization form C and apply the rules in
`beets-edit-normalize-rules'.  Interactively, operate on the active
region, or on the whole buffer when no region is active."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((tick (buffer-chars-modified-tick))
         (count (beets-edit--normalize beg end)))
    (cond ((> count 0)
           (message "Made %d replacement%s" count (if (= count 1) "" "s")))
          ((/= tick (buffer-chars-modified-tick))
           (message "Composed Unicode characters"))
          (t
           (message "Nothing to normalize")))))

(defun beets-edit--swap-positional-fields (a b)
  "Exchange the disc and track values between document strings A and B.
Return the results as a cons.  A field absent from either document is
left alone."
  (let ((case-fold-search nil))
    (dolist (field '("disc" "track"))
      (let* ((regexp (beets-edit--field-line-regexp field))
             (va (and (string-match regexp a) (match-string 1 a)))
             (vb (and (string-match regexp b) (match-string 1 b))))
        (when (and va vb (not (equal va vb)))
          (setq a (replace-regexp-in-string regexp (concat field ": " vb)
                                            a t t))
          (setq b (replace-regexp-in-string regexp (concat field ": " va)
                                            b t t))))))
  (cons a b))

(defun beets-edit--transpose-with-next (start)
  "Transpose the document at START with the following one.
The two documents also exchange their disc and track numbers, so the
transposition reorders the tracklist rather than merely the text: each
id moves with its document, and beets applies the new numbers to the
same items.  Return the new start position of the moved document."
  (let ((end (beets-edit--document-end start)))
    (when (= end (point-max))
      (user-error "No next document"))
    (let* ((next-start (save-excursion
                         (goto-char end)
                         (forward-line 1)
                         (point)))
           (next-end (beets-edit--document-end next-start))
           (moved (buffer-substring start end))
           (other (buffer-substring next-start next-end)))
      (when (= next-start next-end)
        (user-error "No next document"))
      (unless (string-suffix-p "\n" other)
        (setq other (concat other "\n")))
      (let ((swapped (beets-edit--swap-positional-fields moved other)))
        (setq moved (car swapped))
        (setq other (cdr swapped)))
      (delete-region start next-end)
      (goto-char start)
      (insert other "---\n" moved)
      (+ start (length other) 4))))

(defun beets-edit--goto-moved (start offset)
  "Move point to OFFSET within the document at START, clamped.
The document may have shrunk when its track number changed width."
  (goto-char (min (+ start offset)
                  (max start (1- (beets-edit--document-end start))))))

(defun beets-edit-move-document-down (&optional n)
  "Move the document at point N places down the tracklist.
Its disc and track numbers are exchanged with each document it passes,
and point goes along.  A negative N moves up."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-move-document-up (- n))
    (dotimes (_ n)
      (let* ((start (beets-edit--document-start))
             (offset (- (point) start)))
        (beets-edit--goto-moved (beets-edit--transpose-with-next start)
                                offset)))))

(defun beets-edit-move-document-up (&optional n)
  "Move the document at point N places up the tracklist.
Its disc and track numbers are exchanged with each document it passes,
and point goes along.  A negative N moves down."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-move-document-down (- n))
    (dotimes (_ n)
      (let ((offset (- (point) (beets-edit--document-start)))
            (previous (beets-edit--previous-document-start)))
        (beets-edit--transpose-with-next previous)
        (beets-edit--goto-moved previous offset)))))

(defun beets-edit-next-document (&optional n)
  "Move to the Nth next document, keeping the field when possible.
N defaults to 1; a negative N moves backward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-previous-document (- n))
    (let ((field (beets-edit--current-field))
          (target (save-excursion
                    ;; A separator ending the buffer, left by a deleted last
                    ;; document, starts no further document.
                    (unless (and (re-search-forward "^---$" nil t n)
                                 (zerop (forward-line 1))
                                 (not (eobp)))
                      (user-error "No next document"))
                    (point))))
      (goto-char target)
      (when field
        (beets-edit--goto-field field)))))

(defun beets-edit-previous-document (&optional n)
  "Move to the Nth previous document, keeping the field when possible.
N defaults to 1; a negative N moves forward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-next-document (- n))
    (let ((field (beets-edit--current-field))
          (target (beets-edit--previous-document-start n)))
      (goto-char target)
      (when field
        (beets-edit--goto-field field)))))

(defun beets-edit--annotate-region (beg end)
  "Refresh the id annotations of the documents intersecting BEG..END.
Registered with `jit-lock-register'.  Each document's id is shown as a
dim annotation right-aligned at the end of the document's first line,
standing in for the invisible `id' line."
  (save-excursion
    (let ((case-fold-search nil)
          (from (progn (goto-char beg) (beets-edit--document-start)))
          (to (beets-edit--document-end end)))
      (remove-overlays from to 'beets-edit--annotation t)
      ;; Raw id lines revealed by `beets-edit-toggle-ids' make the annotations
      ;; redundant; then, as under a suppressing display, only the cleanup above
      ;; is wanted.
      (goto-char (if (or beets-edit--inhibit-id-annotations
                         (not (memq 'beets-edit-id
                                    buffer-invisibility-spec)))
                     to
                   from))
      (while (re-search-forward beets-edit--id-regexp to t)
        (let* ((text (concat "id " (match-string-no-properties 1)))
               (anchor (save-excursion
                         (goto-char (match-beginning 0))
                         (goto-char (beets-edit--document-start))
                         ;; An id on the document's first line is itself
                         ;; invisible; anchor on the next one.
                         (when (looking-at beets-edit--id-regexp)
                           (forward-line 1))
                         (line-end-position)))
               (ov (make-overlay anchor anchor)))
          (overlay-put ov 'beets-edit--annotation t)
          (overlay-put ov 'after-string
                       (concat
                        (propertize " " 'cursor t 'display
                                    ;; One reserved column: on text terminals a
                                    ;; line reaching the window edge wraps its
                                    ;; last character.
                                    `(space :align-to
                                            (- right ,(1+ (string-width text)))))
                        (propertize text 'face 'shadow))))))))

(defun beets-edit-toggle-ids ()
  "Toggle visibility of the raw id lines."
  (interactive)
  (when (bound-and-true-p beets-edit-compact-mode)
    (user-error "The raw id lines are managed by the compact view"))
  (if (memq 'beets-edit-id buffer-invisibility-spec)
      (remove-from-invisibility-spec 'beets-edit-id)
    (add-to-invisibility-spec 'beets-edit-id))
  ;; A spec change is not a buffer change; redisplay would not repaint the lines
  ;; on its own.
  (force-window-update (current-buffer))
  ;; The annotations stand in for the hidden lines, so they follow the toggle.
  (save-restriction
    (widen)
    (beets-edit--annotate-region (point-min) (point-max)))
  (message "Raw id lines %s"
           (if (memq 'beets-edit-id buffer-invisibility-spec)
               "hidden"
             "visible")))

(defun beets-edit--imenu-index ()
  "Return an imenu index with one entry per document.
Documents are indexed by their title, prefixed with the track number
when present, or by their album when they have no title.  Entries point
at the start of the document, which the compact view renders as the row."
  (let ((case-fold-search nil))
    (delq nil
          (beets-edit--map-documents
           (lambda (start end)
             (let* ((value (lambda (field)
                             (save-excursion
                               (goto-char start)
                               (and (re-search-forward
                                     (beets-edit--field-line-regexp field) end t)
                                    (match-string-no-properties 1)))))
                    (title (funcall value "title"))
                    (track (and title (funcall value "track")))
                    (name (cond ((and title track)
                                 (concat track "  " title))
                                (title title)
                                (t (funcall value "album")))))
               (and name (cons name start))))))))

(defvar-local beets-edit--original-ids nil
  "Ids present when the mode was enabled, as a list of strings.")

(defun beets-edit--ids ()
  "Return the ids present in the current buffer, as a list of strings."
  (let ((case-fold-search nil)
        ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward beets-edit--id-regexp nil t)
        (push (match-string-no-properties 1) ids)))
    (nreverse ids)))

(defun beets-edit--bad-ids (original ids)
  "Return the ids in IDS that are new or duplicated against ORIGINAL.
A missing id is not reported: deleting a whole document is a legitimate
way to leave that item unchanged."
  (seq-uniq
   (append (seq-difference ids original)
           (seq-filter (lambda (id)
                         (> (seq-count (lambda (other) (equal other id)) ids)
                            1))
                       ids))))

(defun beets-edit--count-documents-without-id ()
  "Return the number of documents that lack an id field.
A document deleted in its entirety does not count; this reports
documents whose id line alone went missing, which is easy to do
unnoticed while the id lines are invisible."
  (let ((case-fold-search nil))
    (seq-count #'null
               (beets-edit--map-documents
                (lambda (start end)
                  (save-excursion
                    (goto-char start)
                    (re-search-forward beets-edit--id-regexp end t)))))))

(defun beets-edit--check-ids ()
  "Ask for confirmation before saving a buffer with edited ids.
beets matches documents to library items by id and silently ignores
documents whose id is unknown, ambiguous, or absent.  Meant for the
local value of `write-file-functions'; always return nil so that saving
proceeds normally."
  (when beets-edit--original-ids
    (let ((bad (beets-edit--bad-ids beets-edit--original-ids
                                    (beets-edit--ids)))
          (missing (beets-edit--count-documents-without-id)))
      (when bad
        (unless (yes-or-no-p
                 (format (ngettext
                          "The document with unknown or ambiguous id %s will be ignored by beets; save anyway? "
                          "Documents with unknown or ambiguous ids %s will be ignored by beets; save anyway? "
                          (length bad))
                         (string-join bad ", ")))
          (user-error "Save cancelled")))
      (when (> missing 0)
        (unless (yes-or-no-p
                 (format (ngettext
                          "%d document without an id will be ignored by beets; save anyway? "
                          "%d documents without an id will be ignored by beets; save anyway? "
                          missing)
                         missing))
          (user-error "Save cancelled")))))
  nil)

(defun beets-edit--malformed-lines ()
  "Return the numbers of lines that fit no element of the format.
Every line of a beets edit buffer is a \"field: value\" line, a
continuation or list item belonging to a preceding field, a `---'
separator, or blank."
  (let ((case-fold-search nil)
        (line 1)
        (in-field nil)
        bad)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "[a-z0-9_]+:\\(?: \\|$\\)")
               (setq in-field t))
              ((looking-at "\\(?:[ \t]+\\S-\\|- \\)")
               (unless in-field
                 (push line bad)))
              ((looking-at "---$")
               (setq in-field nil))
              ((looking-at "[ \t]*$"))
              (t
               (push line bad)))
        (forward-line 1)
        (setq line (1+ line))))
    (nreverse bad)))

(defun beets-edit--check-structure ()
  "Ask for confirmation before saving a structurally damaged buffer.
A line that is no longer a field, a continuation, or a separator usually
means a label was edited by accident.  Meant for the local value of
`write-file-functions'; always return nil so that saving proceeds
normally."
  (let ((bad (beets-edit--malformed-lines)))
    (when bad
      (unless (yes-or-no-p
               (format (ngettext
                        "Line %s no longer looks like beets fields; save anyway? "
                        "Lines %s no longer look like beets fields; save anyway? "
                        (length bad))
                       (mapconcat #'number-to-string bad ", ")))
        (user-error "Save cancelled"))))
  nil)

;;;###autoload
(defun beets-edit-buffer-p ()
  "Return non-nil if the current buffer resembles a beets edit buffer.

A beets edit buffer starts with a column-zero \"field: value\"
line and carries a numeric id field in its first document, as
every document beets emits does.  The id is negative in buffers
created during an import session.  This function is meant to be
used as the predicate of a `magic-mode-alist' entry; see the
Commentary for the snippet to add to an init file."
  (let ((case-fold-search nil))
    (save-restriction
      ;; `set-auto-mode' narrows to the first few thousand bytes for its regexp
      ;; entries; this predicate is about content, so look at all of it.
      (widen)
      (save-excursion
        (goto-char (point-min))
        (and (looking-at "[a-z0-9_]+:\\(?: \\|$\\)")
             ;; Bounding the search to the first document keeps unrelated YAML
             ;; with an id further down from being claimed.
             (re-search-forward beets-edit--id-regexp
                                (beets-edit--document-end (point-min))
                                t)
             t)))))

(defvar-keymap beets-edit-mode-map
  :doc "Keymap for `beets-edit-mode'."
  "C-c C-f" #'beets-edit-set-field
  "C-c C-q" #'beets-edit-normalize
  "C-c C-t" #'beets-edit-toggle-ids
  "C-c C-v" #'beets-edit-compact-mode
  "M-n" #'beets-edit-next-document
  "M-p" #'beets-edit-previous-document
  "M-<up>" #'beets-edit-move-document-up
  "M-<down>" #'beets-edit-move-document-down)

;;;###autoload
(define-derived-mode beets-edit-mode text-mode "Beets"
  "Major mode for beets metadata editing buffers.

These buffers are created by the beets `edit' plugin: a stream of
flat YAML documents, one per track or album, separated by `---'
lines.  The documents are also pages, so the page commands
\\[forward-page], \\[backward-page], and \\[narrow-to-page]
operate on them.

The `id' lines, which beets owns and the user should never edit, are
hidden and shown instead as dim annotations at the end of each
document's first line; \\[beets-edit-toggle-ids] reveals the raw lines.
Saving warns when an id was edited, duplicated, or deleted without its
document.

\\{beets-edit-mode-map}"
  (setq font-lock-defaults '(beets-edit-font-lock-keywords))
  (setq-local font-lock-extra-managed-props '(invisible rear-nonsticky))
  ;; Not `add-to-invisibility-spec': that keeps the default t member, which
  ;; hides any non-nil `invisible' property, so removing the symbol in
  ;; `beets-edit-toggle-ids' would reveal nothing.
  (setq-local buffer-invisibility-spec '(beets-edit-id))
  (jit-lock-register #'beets-edit--annotate-region)
  (setq-local page-delimiter "^---$")
  (setq-local imenu-create-index-function #'beets-edit--imenu-index)
  (setq beets-edit--original-ids (save-restriction
                                   (widen)
                                   (beets-edit--ids)))
  (add-hook 'write-file-functions #'beets-edit--check-ids nil t)
  (add-hook 'write-file-functions #'beets-edit--check-structure nil t)
  (add-hook 'change-major-mode-hook #'beets-edit--teardown nil t))

(defun beets-edit--teardown ()
  "Remove the mode's traces before a change of major mode.
The save guards need explicit removal because `write-file-functions' is
permanent-local, and the id annotation overlays because overlays are not
local variables at all."
  (remove-hook 'change-major-mode-hook #'beets-edit--teardown t)
  (remove-hook 'write-file-functions #'beets-edit--check-ids t)
  (remove-hook 'write-file-functions #'beets-edit--check-structure t)
  (jit-lock-unregister #'beets-edit--annotate-region)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'beets-edit--annotation t)))

(provide 'beets-edit-mode)

;;; beets-edit-mode.el ends here
