;;; beets-edit-compact.el --- Compact view for beets edit buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emre Yolcu

;; Author: Emre Yolcu <mail@emreyolcu.com>

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

;; A compact view of beets edit buffers, in the style of Org's column view:
;; fields identical across all documents move to a caption on the tab line, and
;; each document renders as a single row of the fields that differ, under column
;; labels on the header line.  The buffer text underneath stays untouched.  Each
;; cell is an overlay over one character of the document's first line, so point
;; motion is cell motion.  The view is read-only and editing goes through
;; commands; toggling it off restores the raw buffer exactly.

;;; Code:

(require 'beets-edit-mode)
(require 'hl-line)
(require 'cursor-sensor)
(require 'face-remap)

(defcustom beets-edit-compact-max-cell-width 65
  "Maximum display width of a row cell before truncation.
Column widths are computed from the data: each column is as wide as its
widest value or its label, capped at this many columns.  Truncated cells
expand while point is inside them.  The default keeps a typical row
within 80 columns."
  :type 'natnum
  :group 'beets-edit)

(defcustom beets-edit-compact-field-order
  '("disc" "track" "title" "artist" "album" "albumartist" "year"
    "genres" "label" "catalognum")
  "Canonical order of fields in the compact view.

Applies to the caption of shared fields, the row columns, and
field completion.  It follows the beets edit plugin's own field order,
extended with the disc number and common album-level fields.
Fields not listed here keep their buffer order, which beets emits
alphabetically."
  :type '(repeat string)
  :group 'beets-edit)

(defcustom beets-edit-compact-never-shared-fields
  '("disc" "track" "title" "year" "genres")
  "Fields that stay in the rows even when identical in every document.
A field identical across all documents normally moves to the caption;
these are exempt.  The defaults are position and identity, which repeat
across documents only by coincidence of the query, as when every match
is a first track or a cover of one song.  The year and genres are
included too, since separate albums often share them by coincidence."
  :type '(repeat string)
  :group 'beets-edit)

(defvar beets-edit-compact-mode)

(defconst beets-edit-compact--positional-fields '("disc" "track")
  "Fields that place a document rather than describe it.
The cursor prefers to rest on the first column that is not one of them.")

(defvar-local beets-edit-compact--saved nil
  "Plist of raw-view settings restored on exit; non-nil while active.")
(defvar-local beets-edit-compact--caption-remap nil
  "Cookie of the buffer-local tab line face remapping.")
(defvar-local beets-edit-compact--expanded nil
  "Cons of the currently expanded truncated cell and its short display.")
(defvar-local beets-edit-compact--highlighted nil
  "Cell overlays of the currently highlighted row.")
(defvar-local beets-edit-compact--highlight nil
  "Cons of the highlighted row's start and its inherited faces.")
(defvar-local beets-edit-compact--title nil
  "The column title row, before horizontal scroll adjustment.")
(defvar-local beets-edit-compact--caption-text nil
  "The caption text, before the chrome prefix is attached.")

(defun beets-edit-compact--order (names)
  "Return NAMES sorted by `beets-edit-compact-field-order'.
Unlisted names keep their relative order after the listed ones."
  (sort (copy-sequence names)
        (lambda (a b)
          (< (or (seq-position beets-edit-compact-field-order a)
                 most-positive-fixnum)
             (or (seq-position beets-edit-compact-field-order b)
                 most-positive-fixnum)))))

(defun beets-edit-compact--documents ()
  "Return a list of (START END FIELDS), one per document.
START and END delimit the document, excluding its separator.  FIELDS is
an alist of (NAME . VALUE) in buffer order, with the continuation lines
of wrapped values folded in and the items of list-valued fields joined
with \"; \"."
  (beets-edit--map-documents #'beets-edit-compact--parse-document))

(defun beets-edit-compact--parse-document (start end)
  "Parse the document between START and END into (START END FIELDS)."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search nil)
          fields)
      (while (< (point) end)
        (if (looking-at "\\([a-z0-9_]+\\): ?\\(.*\\)$")
            (let ((name (match-string-no-properties 1))
                  (value (match-string-no-properties 2)))
              (forward-line 1)
              (push (cons name (beets-edit--fold-value value end)) fields))
          (forward-line 1)))
      (list start end (nreverse fields)))))

(defun beets-edit-compact--shared-fields (docs)
  "Return the fields of DOCS with identical values everywhere.
A field counts as shared only when it is present in every document with
byte-identical value text, and there are at least two documents.  The id
field and the fields in `beets-edit-compact-never-shared-fields' are
never included.  The result is in canonical order."
  (when (cdr docs)
    (let (shared)
      (dolist (fv (nth 2 (car docs)))
        (let ((name (car fv))
              (value (cdr fv)))
          (when (and (not (equal name "id"))
                     (not (member name
                                  beets-edit-compact-never-shared-fields))
                     (seq-every-p
                      (lambda (doc)
                        (let ((cell (assoc name (nth 2 doc))))
                          (and cell (equal (cdr cell) value))))
                      (cdr docs)))
            (push fv shared))))
      (setq shared (nreverse shared))
      (mapcar (lambda (name) (assoc name shared))
              (beets-edit-compact--order (mapcar #'car shared))))))

(defun beets-edit-compact--row-fields (docs shared)
  "Return the names of the fields DOCS rows should display.
Excludes the fields in SHARED and the id field; the result is in
canonical order."
  (let (names)
    (dolist (doc docs)
      (dolist (fv (nth 2 doc))
        (let ((name (car fv)))
          (unless (or (equal name "id")
                      (assoc name shared)
                      (member name names))
            (push name names)))))
    (beets-edit-compact--order (nreverse names))))

(defun beets-edit-compact--widths (docs row-fields)
  "Return an alist of display widths for ROW-FIELDS across DOCS.
Columns are always at least as wide as their labels."
  (mapcar (lambda (name)
            (let ((width (string-width name)))
              (dolist (doc docs)
                (let ((cell (assoc name (nth 2 doc))))
                  (when cell
                    (setq width (max width (string-width (cdr cell)))))))
              (cons name (min width beets-edit-compact-max-cell-width))))
          row-fields))

(defun beets-edit-compact--title-row (row-fields widths)
  "Return the labeled column title row for ROW-FIELDS with WIDTHS."
  (propertize (mapconcat (lambda (name)
                           (truncate-string-to-width
                            name (cdr (assoc name widths)) nil ?\s "…"))
                         row-fields "  ")
              'face 'font-lock-variable-name-face))

(defun beets-edit-compact--caption (docs shared)
  "Return the tab line caption summarizing SHARED fields of DOCS."
  (if shared
      (mapconcat (lambda (fv)
                   (concat (propertize
                            (car fv)
                            'face 'font-lock-variable-name-face)
                           ;; The tab line is a mode-line format, where % starts
                           ;; a construct.
                           ": " (string-replace
                                 "%" "%%" (cdr fv))))
                 shared "    ")
    (format (ngettext "%d document" "%d documents" (length docs))
            (length docs))))

(defun beets-edit-compact--remove-overlays ()
  "Delete all compact view overlays."
  (setq beets-edit-compact--expanded nil
        beets-edit-compact--highlighted nil
        beets-edit-compact--highlight nil)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'beets-edit-compact t)))

(defun beets-edit-compact--face (faces)
  "Return an explicit, fully specified face inheriting FACES, if any."
  (if faces `(:inherit (,@faces default)) 'default))

(defun beets-edit-compact--set-cell-face (cell faces)
  "Set CELL's strings to the row face, inheriting FACES when non-nil.
Cell display strings need an explicit face: without one they inherit the
fontification of the character they replace.  Faces of overlays covering
the row, such as the highlight of the row at point or a preview overlay
from another package, cannot paint the display strings either, so they
are folded into the strings' own face here.  The id suffix takes the row
faces too, under its dim foreground, and the cell keeps its own base
face, such as a changed field's highlight, underneath.  Callers
repainting a suspended row refresh its overlay-string copy afterwards
with `beets-edit-compact--row-copy'."
  (let* ((base (overlay-get cell 'beets-edit-compact-base))
         (face (beets-edit-compact--face (append base faces)))
         (suffix (or (overlay-get cell 'beets-edit-compact-suffix) ""))
         (suffixed (not (string-empty-p suffix)))
         (gap-face (and suffixed (beets-edit-compact--face faces)))
         (id-face (and suffixed
                       (beets-edit-compact--face (cons 'shadow faces)))))
    (dolist (prop '(display beets-edit-compact-full
                            beets-edit-compact-suspended))
      (let ((string (overlay-get cell prop)))
        (when string
          (setq string (copy-sequence string))
          (let* ((len (length string))
                 (suffix-len (if (string-suffix-p suffix string)
                                 (length suffix)
                               0)))
            (put-text-property 0 (- len suffix-len) 'face face string)
            (when (> suffix-len 0)
              ;; The two spacer columns, then the "id NNN" text.
              (put-text-property (- len suffix-len) (+ (- len suffix-len) 2)
                                 'face gap-face string)
              (put-text-property (+ (- len suffix-len) 2) len
                                 'face id-face string)))
          (overlay-put cell prop string))))))

(defun beets-edit-compact--changed-fields (start end)
  "Return the fields changed between START and END.
A field counts as changed when its lines carry the highlight-changes
property, which cannot show through the cells; the cells render the
change face themselves."
  (when (bound-and-true-p highlight-changes-mode)
    (save-excursion
      (goto-char start)
      (let ((case-fold-search nil)
            field fields)
        (while (< (point) end)
          (when (looking-at "\\([a-z0-9_]+\\):")
            (setq field (match-string-no-properties 1)))
          (when (and field
                     (text-property-not-all (point) (line-end-position)
                                            'hilit-chg nil))
            (push field fields))
          (forward-line 1))
        (seq-uniq fields)))))

(defun beets-edit-compact--refresh ()
  "Build the compact rendering of the current buffer.
Every displayed cell is an overlay over one character of its document's
first line, in the style of Org's column view, so that point motion is
cell motion."
  (beets-edit-compact--remove-overlays)
  (save-restriction
    (widen)
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs))
           (row-fields (beets-edit-compact--row-fields docs shared))
           (widths (beets-edit-compact--widths docs row-fields)))
      (setq beets-edit-compact--caption-text
            (beets-edit-compact--caption docs shared))
      (setq tab-line-format '(:eval (beets-edit-compact--tab-line)))
      (setq beets-edit-compact--title
            (and row-fields
                 (beets-edit-compact--title-row row-fields widths)))
      (setq header-line-format
            (and beets-edit-compact--title
                 '(:eval (beets-edit-compact--header))))
      (dolist (doc docs)
        (beets-edit-compact--render-row doc row-fields widths)))))

(defun beets-edit-compact--render-row (doc row-fields widths)
  "Render the document DOC as one row of ROW-FIELDS cells.
WIDTHS is the column width alist.  Creates the cell overlays over the
document's first line and the fold overlays hiding the rest."
  (pcase-let* ((`(,start ,end ,fields) doc)
               (doc-id (cdr (assoc "id" fields)))
               (eol (save-excursion (goto-char start) (line-end-position)))
               (shown (min (length row-fields) (- eol start)))
               (lump (> (length row-fields) shown))
               (changed (beets-edit-compact--changed-fields start end))
               (hide-end (save-excursion
                           (goto-char end)
                           ;; Each fold stops short of a newline that ends the
                           ;; row's screen line: the separator's, or for the
                           ;; last document the buffer's final newline, if any.
                           (cond ((looking-at "---\n")
                                  (1- (match-end 0)))
                                 ((eq (char-before (point-max)) ?\n)
                                  (1- (point-max)))
                                 (t (point-max))))))
    (dotimes (i shown)
      (let* ((lastp (= i (1- shown)))
             ;; A first line shorter than the column count cannot give every
             ;; field a cell; the last cell renders all the remaining fields
             ;; instead.
             (names (if (and lastp lump)
                        (nthcdr i row-fields)
                      (list (nth i row-fields))))
             (name (car names))
             (value (or (cdr (assoc name fields)) ""))
             (text (mapconcat
                    (lambda (field)
                      (truncate-string-to-width
                       (or (cdr (assoc field fields)) "")
                       (cdr (assoc field widths)) nil ?\s "…"))
                    names "  "))
             ;; The raw view's id annotations sit inside text this view hides,
             ;; so the last cell carries the row's id itself, immune to the face
             ;; repaints and present in the expanded rendering too.  A collapsed
             ;; stretch space still renders one column wide, so one literal
             ;; space makes the gap before the id match the two-column separator
             ;; when a long value overruns the alignment target.  Fully
             ;; specified faces: with a bare shadow or a faceless stretch, faces
             ;; of the hidden text underneath, such as a lazy highlight, would
             ;; bleed through.
             (suffix (if (and lastp doc-id)
                         (concat
                          (propertize " " 'face 'default)
                          (propertize " " 'face 'default
                                      'display
                                      `(space :align-to
                                              (- right
                                                 ,(+ 4 (length doc-id)))))
                          (propertize
                           (concat "id " doc-id)
                           'face (beets-edit-compact--face '(shadow))))
                       ""))
             (base (and (seq-intersection names changed)
                        '(highlight-changes)))
             (content (propertize (concat text (if lastp "" "  "))
                                  'face (beets-edit-compact--face base)))
             (ov (make-overlay (+ start i) (+ start i 1))))
        (overlay-put ov 'beets-edit-compact t)
        (overlay-put ov 'beets-edit-compact-field name)
        (overlay-put ov 'beets-edit-compact-base base)
        (overlay-put ov 'beets-edit-compact-suffix suffix)
        (overlay-put ov 'display (concat content suffix))
        (overlay-put ov 'help-echo (concat name ": " value))
        (when (seq-some (lambda (field)
                          (> (string-width (or (cdr (assoc field fields)) ""))
                             (cdr (assoc field widths))))
                        names)
          (overlay-put ov 'beets-edit-compact-full
                       (concat (propertize
                                (concat (mapconcat
                                         (lambda (field)
                                           (or (cdr (assoc field fields)) ""))
                                         names "  ")
                                        (if lastp "" "  "))
                                'face (beets-edit-compact--face base))
                               suffix))
          (overlay-put ov 'cursor-sensor-functions
                       (list #'beets-edit-compact--sensor)))))
    (when (< (+ start shown) eol)
      (beets-edit-compact--hide (+ start shown) eol 'searchable))
    (let ((body-end (min end hide-end)))
      (when (< eol body-end)
        (beets-edit-compact--hide eol body-end 'searchable))
      (when (< body-end hide-end)
        (beets-edit-compact--hide body-end hide-end)))))

(defun beets-edit-compact--hide (start end &optional searchable)
  "Hide START to END behind an overlay.
When SEARCHABLE, Isearch may unfold it, just as it opens folded Org
headings; unfolding reveals the raw document whole, so the first-line
remainder is openable like the body, and only the separator is not."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'beets-edit-compact t)
    (overlay-put overlay 'invisible 'beets-edit-compact)
    (when searchable
      (overlay-put overlay 'isearch-open-invisible
                   #'beets-edit-compact--isearch-open)
      (overlay-put overlay 'isearch-open-invisible-temporary
                   #'beets-edit-compact--isearch-temporary))))

(defun beets-edit-compact--row-copy (start &optional create)
  "Refresh the row shown above the unfolded document at START.
While a document is unfolded its cells suspend their rendering, and
their suspended strings, joined, stand in for the row as an overlay
string above the raw text; the row itself is synthetic presentation, so
nothing searchable is lost in the copy.  Only refreshes an existing copy
unless CREATE is non-nil."
  (let* ((cells (beets-edit-compact--cells start))
         (first (car cells)))
    (when (and first (or create (overlay-get first 'before-string)))
      (let ((copy (concat (mapconcat
                           (lambda (c)
                             (overlay-get c 'beets-edit-compact-suspended))
                           cells)
                          "\n")))
        ;; The stretch bounding the id aligns against the window edge inside an
        ;; overlay string, unlike inside the cells' display strings; without it,
        ;; the literal spaces reproduce the row's own gap.
        (remove-text-properties 0 (length copy) '(display nil) copy)
        (overlay-put first 'before-string copy)))))

(defun beets-edit-compact--isearch-temporary (overlay hide)
  "Unfold OVERLAY's document while a search inspects it, refold when HIDE.
The unfolded document shows its whole raw source beneath its row: the
cells suspend their rendering so the real first line shows through, with
the row standing above it as an overlay string, the id line becomes
visible in place, and the following separator disappears together with
its newline, so no blank line stands in for it.  OVERLAY may be any of
the document's openable overlays.  Isearch calls this mid-search and
reads the match data right after, so the searches here must not leak
into it.

Preview-style revealers, such as Consult's, call the handler for every
openable overlay on the target line, including ones the target is not
inside; outside Isearch, the document unfolds only when point actually
lies in OVERLAY's hidden text, so previewing a row leaves it rendered."
  (save-match-data
    (let* ((start (save-excursion
                    (goto-char (overlay-start overlay))
                    (line-beginning-position)))
           (end (beets-edit--document-end start)))
      (when (or hide
                isearch-mode
                (and (>= (point) (overlay-start overlay))
                     (< (point) (overlay-end overlay))))
        (with-silent-modifications
          ;; Fontify first: the id line's `invisible' property is
          ;; font-lock-managed, so jit-lock's first pass over a document that
          ;; was folded before ever being displayed would otherwise re-hide the
          ;; id line right after it is revealed here.
          (unless hide
            (font-lock-ensure start end))
          (dolist (o (overlays-in start end))
            (when (overlay-get o 'isearch-open-invisible)
              (overlay-put o 'invisible (and hide 'beets-edit-compact))))
          (dolist (cell (beets-edit-compact--cells start))
            (if hide
                (progn
                  (when (overlay-get cell 'beets-edit-compact-suspended)
                    (overlay-put cell 'display
                                 (overlay-get cell
                                              'beets-edit-compact-suspended))
                    (overlay-put cell 'beets-edit-compact-suspended nil))
                  (overlay-put cell 'before-string nil))
              (when (overlay-get cell 'display)
                (overlay-put cell 'beets-edit-compact-suspended
                             (overlay-get cell 'display))
                (overlay-put cell 'display nil))))
          (unless hide
            (beets-edit-compact--row-copy start 'create))
          (save-excursion
            (goto-char start)
            (when (let ((case-fold-search nil))
                    (re-search-forward beets-edit--id-regexp end t))
              (if hide
                  (put-text-property (match-beginning 0) (1+ (match-end 0))
                                     'invisible 'beets-edit-id)
                (remove-text-properties (match-beginning 0)
                                        (1+ (match-end 0))
                                        '(invisible nil))))))
        (let ((separator (seq-find (lambda (o)
                                     (and (overlay-get o 'beets-edit-compact)
                                          (overlay-get o 'invisible)
                                          (not (overlay-get
                                                o 'isearch-open-invisible))))
                                   (overlays-at end))))
          (when separator
            ;; Absolute bounds keep repeated opens idempotent: the separator is
            ;; the three "---" characters, plus its newline while the document
            ;; above is unfolded.
            (move-overlay separator (overlay-start separator)
                          (+ (overlay-start separator)
                             (if hide 3 4)))))))))

(defun beets-edit-compact--isearch-content-p (beg end)
  "Return nil for a match on text the view never displays.
A match, BEG to END, is a phantom when it lies entirely under a row's
cells, or when it starts inside a field label anywhere in the document;
the view shows values, never labels.  Text under cells is never a stop,
even while its document is unfolded: the cells' scaffolding renders
again on refold, and a stop there could not stay visible."
  (not (or (and (get-char-property beg 'beets-edit-compact-field)
                (get-char-property (max beg (1- end))
                                   'beets-edit-compact-field))
           (save-excursion
             (save-match-data
               (goto-char beg)
               (forward-line 0)
               (let ((case-fold-search nil))
                 (and (looking-at "[a-z0-9_]+:\\(?: \\|$\\)")
                      (< beg (match-end 0)))))))))

(defun beets-edit-compact--goto-field-cell ()
  "Move point from inside a document to the cell of its field.
When the field at point has no column of its own, land on the row's home
cell."
  (let* ((field (beets-edit--current-field))
         (start (beets-edit--document-start))
         (column (or (and field
                          (seq-position
                           (mapcar (lambda (cell)
                                     (overlay-get
                                      cell 'beets-edit-compact-field))
                                   (beets-edit-compact--cells start))
                           field))
                     (beets-edit-compact--home-column start))))
    (beets-edit-compact--goto-column start column)))

(defun beets-edit-compact--isearch-open (overlay)
  "Land a finished search on the matched field's cell.
Isearch calls this to make the match at point visible; instead of
leaving raw text open inside the view, move point to the cell rendering
the matched field and refold OVERLAY."
  (save-match-data
    (beets-edit-compact--isearch-temporary overlay t)
    (beets-edit-compact--goto-field-cell)))

(defun beets-edit-compact--reveal-occurrence ()
  "Land an occur jump on the cell of the matched field.
Meant for the local value of `occur-mode-find-occurrence-hook': the
occurrence itself lies in a folded document body, where point cannot
usefully stay."
  (when beets-edit-compact-mode
    (beets-edit-compact--goto-field-cell)))

(defun beets-edit-compact--cells (start)
  "Return the cell overlays of the row starting at START, in order.
Cells cover consecutive characters from START, so no sorting is needed."
  (let ((position start)
        cells cell)
    (while (setq cell (beets-edit-compact--cell-at position))
      (push cell cells)
      (setq position (1+ position)))
    (nreverse cells)))

(defun beets-edit-compact--cell-at (pos)
  "Return the cell overlay at POS, or nil."
  (seq-find (lambda (o) (overlay-get o 'beets-edit-compact-field))
            (overlays-at pos)))

(defun beets-edit-compact--chrome-prefix ()
  "Return a prefix aligning a chrome line with the buffer's columns.
Chrome lines have no line number display; align past the window's, when
it has one."
  (propertize " " 'display
              `(space :align-to
                      ,(if display-line-numbers
                           `(,(line-number-display-width t))
                         0))))

(defun beets-edit-compact--tab-line ()
  "Return the caption for the window being drawn.
Evaluated by the tab line so that its origin matches the header line's
and the buffer's columns in any window configuration."
  (concat (beets-edit-compact--chrome-prefix)
          beets-edit-compact--caption-text))

(defun beets-edit-compact--header ()
  "Return the column labels for the window being drawn.
Evaluated by the header line with that window selected, so the labels
follow the window's own horizontal scroll, which header lines ignore,
and clear its line number display when one is on."
  (concat (beets-edit-compact--chrome-prefix)
          (substring beets-edit-compact--title
                     (min (window-hscroll)
                          (length beets-edit-compact--title)))))

(defun beets-edit-compact--update-highlight (&optional window)
  "Highlight the row at point.
Runs from `pre-redisplay-functions'; only the selected WINDOW is
considered, so several windows on the buffer do not fight over the
single highlight.  Faces of foreign overlays at the row start, such as a
completion preview, take precedence over the `hl-line' highlight.  Only
overlays spanning the whole anchor line qualify: fragment-level marks,
such as a spell checker's on one word, must not tint the whole row.
Overlays belonging to another window, the region, whose extent the row
cannot render faithfully, and highlight-changes marks, which the cells
render per field themselves, are left out too."
  (when (and beets-edit-compact-mode
             (or (null window)
                 (eq window (selected-window))
                 (eq window (minibuffer-selected-window))))
    (let* ((pos (if window (window-point window) (point)))
           (start (save-excursion
                    (goto-char pos)
                    (beets-edit--document-start)))
           (eol (save-excursion (goto-char start) (line-end-position)))
           (foreign (delete-dups
                     (delq nil
                           (mapcar (lambda (o)
                                     (and (not (overlay-get
                                                o 'beets-edit-compact))
                                          (not (overlay-get o 'hilit-chg))
                                          (>= (overlay-end o) eol)
                                          (let ((w (overlay-get o 'window)))
                                            (or (null w) (eq w window)))
                                          (let ((face (overlay-get o 'face)))
                                            (and (not (eq face 'region))
                                                 face))))
                                   (overlays-at start)))))
           (faces (append foreign '(hl-line))))
      (unless (and (eql start (car beets-edit-compact--highlight))
                   (equal faces (cdr beets-edit-compact--highlight)))
        (dolist (cell beets-edit-compact--highlighted)
          (when (overlay-buffer cell)
            (beets-edit-compact--set-cell-face cell nil)))
        (when (car beets-edit-compact--highlight)
          (beets-edit-compact--row-copy (car beets-edit-compact--highlight)))
        (let ((cells (beets-edit-compact--cells start)))
          (dolist (cell cells)
            (beets-edit-compact--set-cell-face cell faces))
          (beets-edit-compact--row-copy start)
          (setq beets-edit-compact--highlighted cells))
        (setq beets-edit-compact--highlight (cons start faces))))))

(defun beets-edit-compact--sensor (window _oldpos dir)
  "Expand a truncated cell while point is inside it.
DIR is the motion direction symbol from `cursor-sensor-mode'; only the
selected WINDOW expands cells, since the expansion is a single
buffer-wide display change.  A row whose rendering is suspended while
its document is unfolded is left alone; an expansion collapsing on such
a row updates the suspended rendering instead."
  (when beets-edit-compact--expanded
    (let ((cell (car beets-edit-compact--expanded)))
      (when (overlay-buffer cell)
        (cond ((overlay-get cell 'display)
               (overlay-put cell 'display (cdr beets-edit-compact--expanded))
               (beets-edit-compact--set-cell-face
                cell (and (memq cell beets-edit-compact--highlighted)
                          (cdr beets-edit-compact--highlight))))
              ((overlay-get cell 'beets-edit-compact-suspended)
               (overlay-put cell 'beets-edit-compact-suspended
                            (cdr beets-edit-compact--expanded))
               (beets-edit-compact--row-copy
                (save-excursion (goto-char (overlay-start cell))
                                (beets-edit--document-start)))))))
    (setq beets-edit-compact--expanded nil))
  (when (and (eq dir 'entered)
             (or (null window) (eq window (selected-window))))
    (let ((cell (beets-edit-compact--cell-at (point))))
      (when (and cell
                 (overlay-get cell 'display)
                 (overlay-get cell 'beets-edit-compact-full))
        (setq beets-edit-compact--expanded
              (cons cell (overlay-get cell 'display)))
        (overlay-put cell 'display
                     (overlay-get cell 'beets-edit-compact-full))
        (beets-edit-compact--set-cell-face
         cell (and (memq cell beets-edit-compact--highlighted)
                   (cdr beets-edit-compact--highlight)))))))

(defun beets-edit-compact--goto-column (start column)
  "Move to cell COLUMN of the row starting at START, clamped."
  (let ((cells (beets-edit-compact--cells start)))
    (goto-char (+ start (max 0 (min column
                                    (1- (max 1 (length cells)))))))))

(defun beets-edit-compact-next (&optional n)
  "Move to the Nth next document, staying on the same column.
A negative N moves to a previous row."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-compact-previous (- n))
    (let ((column (- (point) (beets-edit--document-start)))
          ;; Overshooting must not move point, as in
          ;; `beets-edit-compact-previous'.
          (target (save-excursion
                    (dotimes (_ n)
                      (unless (re-search-forward "^---$" nil t)
                        (user-error "No next document"))
                      (forward-line 1))
                    (point))))
      (beets-edit-compact--goto-column target column))))

(defun beets-edit-compact-previous (&optional n)
  "Move to the Nth previous document, staying on the same column.
A negative N moves to a following row."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-compact-next (- n))
    (let ((column (- (point) (beets-edit--document-start)))
          (target (beets-edit--previous-document-start n)))
      (beets-edit-compact--goto-column target column))))

(defun beets-edit-compact-forward-cell (&optional n)
  "Move N cells forward, flowing onto the next row past the last cell.
A negative N moves backward.  The first and last cell of the table
clamp."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-compact-backward-cell (- n))
    (dotimes (_ n)
      (let* ((start (beets-edit--document-start))
             (cells (beets-edit-compact--cells start)))
        (if (and cells (< (point) (+ start (1- (length cells)))))
            (goto-char (1+ (point)))
          (let ((next (save-excursion
                        (goto-char start)
                        (and (re-search-forward "^---$" nil t)
                             (progn (forward-line 1) (point))))))
            (when next (goto-char next))))))))

(defun beets-edit-compact-backward-cell (&optional n)
  "Move N cells backward, flowing onto the previous row before the first.
A negative N moves forward.  The first and last cell of the table clamp."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (beets-edit-compact-forward-cell (- n))
    (dotimes (_ n)
      (let ((start (beets-edit--document-start)))
        (if (> (point) start)
            (goto-char (1- (point)))
          (when (> start (point-min))
            (let* ((previous (beets-edit--previous-document-start))
                   (cells (beets-edit-compact--cells previous)))
              (goto-char (+ previous
                            (max 0 (1- (length cells))))))))))))

(defun beets-edit-compact--home-column (start)
  "Return the home column of the row at START.
Home is the first column that is not positional; a row of nothing but
positional columns homes to its first cell."
  (let ((cells (beets-edit-compact--cells start))
        (column 0))
    (while (and cells
                (member (overlay-get (car cells)
                                     'beets-edit-compact-field)
                        beets-edit-compact--positional-fields))
      (setq cells (cdr cells))
      (setq column (1+ column)))
    (if cells column 0)))

(defun beets-edit-compact--goto-home (start)
  "Move to the home cell of the row starting at START."
  (beets-edit-compact--goto-column
   start (beets-edit-compact--home-column start)))

(defun beets-edit-compact-first-row ()
  "Move to the home cell of the first row, pushing the mark."
  (interactive)
  (push-mark)
  (beets-edit-compact--goto-home (point-min)))

(defun beets-edit-compact-last-row ()
  "Move to the home cell of the last row, pushing the mark."
  (interactive)
  (push-mark)
  (beets-edit-compact--goto-home
   (save-excursion (goto-char (point-max))
                   (beets-edit--document-start))))

(defun beets-edit-compact-scroll-up (&optional arg)
  "Scroll upward by ARG lines, keeping the cell column."
  (interactive "^P")
  (let ((column (- (point) (beets-edit--document-start))))
    (scroll-up-command arg)
    (beets-edit-compact--goto-column (beets-edit--document-start) column)))

(defun beets-edit-compact-scroll-down (&optional arg)
  "Scroll downward by ARG lines, keeping the cell column."
  (interactive "^P")
  (let ((column (- (point) (beets-edit--document-start))))
    (scroll-down-command arg)
    (beets-edit-compact--goto-column (beets-edit--document-start) column)))

(defun beets-edit-compact--imenu-jump ()
  "Land an imenu jump on the row's home cell.
Meant for the local value of `imenu-after-jump-hook'; the index entries
point at document starts, which are positional cells."
  (when beets-edit-compact-mode
    (beets-edit-compact--goto-home (beets-edit--document-start))))

(defun beets-edit-compact-first-cell ()
  "Move to the home cell of the current row, its first content column."
  (interactive)
  (beets-edit-compact--goto-home (beets-edit--document-start)))

(defun beets-edit-compact-last-cell ()
  "Move to the last cell of the current row."
  (interactive)
  (let* ((start (beets-edit--document-start))
         (cells (beets-edit-compact--cells start)))
    (when cells
      (goto-char (overlay-start (car (last cells)))))))

(defun beets-edit-compact--write (start end field value)
  "Set FIELD to VALUE in the document between START and END.
A field the document does not have yet is added at its end."
  (save-excursion
    (goto-char start)
    (let ((inhibit-read-only t))
      (unless (beets-edit--write-field field value end)
        (goto-char end)
        ;; The buffer may lack a final newline.
        (unless (bolp) (insert "\n"))
        (insert field ": " value "\n")))))

(defun beets-edit-compact--completion-table (names)
  "Return a completion table over NAMES that preserves their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action names string pred))))

(defun beets-edit-compact-edit (&optional prompt)
  "Edit a field of the document at point.
Without a prefix argument, edit the field of the cell at point.  With a
prefix argument PROMPT, or when point is not on a cell, ask for the
field, offering all of the document's fields including the shared ones
shown in the caption; editing a shared field here changes only this
document, after which the field is no longer shared and moves into the
rows.

The document is relocated by its id after the value editor returns, so
edits made meanwhile cannot misdirect the write."
  (interactive "P")
  (let* ((start (beets-edit--document-start))
         (column (- (point) start))
         (doc-fields (nth 2 (beets-edit-compact--parse-document
                             start (beets-edit--document-end start))))
         (cell (beets-edit-compact--cell-at (point)))
         (fields (seq-remove (lambda (fv) (equal (car fv) "id"))
                             doc-fields))
         (id (cdr (assoc "id" doc-fields)))
         (field (if (and cell (not prompt))
                    (overlay-get cell 'beets-edit-compact-field)
                  (beets-edit--read-field
                   "Field: "
                   (beets-edit-compact--completion-table
                    (beets-edit-compact--order (mapcar #'car fields))))))
         (current (cdr (assoc field fields)))
         (value (beets-edit--read-value field current)))
    (if (equal value current)
        (message "No change")
      (when id
        (save-excursion
          (goto-char (point-min))
          (unless (re-search-forward (concat "^id: " id "$") nil t)
            (user-error "The document disappeared while editing"))
          (setq start (beets-edit--document-start))))
      (beets-edit-compact--write start (beets-edit--document-end start)
                                 field value)
      (beets-edit-compact--refresh)
      (beets-edit-compact--goto-column start column))))

(defun beets-edit-compact-edit-all ()
  "Set a field to the same value in every document."
  (interactive)
  (let* ((start (beets-edit--document-start))
         (column (- (point) start))
         (id (cdr (assoc "id" (nth 2 (beets-edit-compact--parse-document
                                      start
                                      (beets-edit--document-end start))))))
         (names (beets-edit-compact--order (beets-edit--field-names)))
         (field (beets-edit--read-field
                 "Field (all documents): "
                 (beets-edit-compact--completion-table names)))
         (current (beets-edit--field-value field))
         (value (beets-edit--read-value field current))
         (count (let ((inhibit-read-only t))
                  (beets-edit--set-field field value))))
    (beets-edit-compact--refresh)
    ;; Re-anchor point: the row may have lost the column it stood on when the
    ;; field became shared and moved to the caption.
    (when id
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^id: " id "$") nil t)
          (setq start (beets-edit--document-start)))))
    (beets-edit-compact--goto-column start column)
    (message (ngettext "Set %s in %d document" "Set %s in %d documents" count)
             field count)))

(defun beets-edit-compact-normalize ()
  "Normalize the whole buffer's typography and refresh the view.
Runs `beets-edit-normalize' on the real text under the view."
  (interactive)
  (let ((column (- (point) (beets-edit--document-start))))
    (let ((inhibit-read-only t))
      (beets-edit-normalize (point-min) (point-max)))
    (beets-edit-compact--refresh)
    (beets-edit-compact--goto-column (beets-edit--document-start) column)))

(defun beets-edit-compact-move-down (&optional n)
  "Move the row at point N rows down, point going along.
The refresh is unconditional: a count that overshoots the last row
errors after some documents have already moved."
  (interactive "p")
  (unwind-protect
      (let ((inhibit-read-only t))
        (beets-edit-move-document-down n))
    (beets-edit-compact--refresh)))

(defun beets-edit-compact-move-up (&optional n)
  "Move the row at point N rows up, point going along.
The refresh is unconditional: a count that overshoots the first row
errors after some documents have already moved."
  (interactive "p")
  (unwind-protect
      (let ((inhibit-read-only t))
        (beets-edit-move-document-up n))
    (beets-edit-compact--refresh)))

(defun beets-edit-compact--refuse-edit ()
  "Explain that the compact view edits through commands.
Bound in place of `self-insert-command'.  A refused edit must not even
begin: an insertion stopped only by a read-only text property would
still mark the buffer as modified, whose redisplay wipes the error
message from the echo area."
  (interactive)
  (user-error
   (substitute-command-keys
    "Compact view: \\[beets-edit-compact-edit] edits this cell, \\[universal-argument] \\[beets-edit-compact-edit] asks, \\[beets-edit-compact-edit-all] edits all, \\[beets-edit-compact-quit] raw view")))

(defun beets-edit-compact-undo (&optional arg)
  "Undo in the compact view and refresh it.
ARG is passed to `undo', which the view's read-only state would
otherwise refuse."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg))
  (beets-edit-compact--refresh))

(defun beets-edit-compact-redo (&optional arg)
  "Redo in the compact view and refresh it.
ARG is passed to `undo-redo', which the view's read-only state would
otherwise refuse."
  (interactive "p")
  (let ((inhibit-read-only t))
    (undo-redo arg))
  (beets-edit-compact--refresh))

(defun beets-edit-compact-quit ()
  "Return to the raw view."
  (interactive)
  (beets-edit-compact-mode -1))

(defvar-keymap beets-edit-compact-mode-map
  :doc "Keymap for `beets-edit-compact-mode'."
  "n" #'beets-edit-compact-next
  "p" #'beets-edit-compact-previous
  "f" #'beets-edit-compact-forward-cell
  "b" #'beets-edit-compact-backward-cell
  "TAB" #'beets-edit-compact-forward-cell
  "<backtab>" #'beets-edit-compact-backward-cell
  "RET" #'beets-edit-compact-edit
  "e" #'beets-edit-compact-edit
  "E" #'beets-edit-compact-edit-all
  "q" #'beets-edit-compact-quit
  "<remap> <self-insert-command>" #'beets-edit-compact--refuse-edit
  "<remap> <delete-char>" #'beets-edit-compact--refuse-edit
  "<remap> <delete-backward-char>" #'beets-edit-compact--refuse-edit
  "<remap> <delete-forward-char>" #'beets-edit-compact--refuse-edit
  "<remap> <kill-line>" #'beets-edit-compact--refuse-edit
  "<remap> <kill-whole-line>" #'beets-edit-compact--refuse-edit
  "<remap> <kill-region>" #'beets-edit-compact--refuse-edit
  "<remap> <yank>" #'beets-edit-compact--refuse-edit
  "<remap> <open-line>" #'beets-edit-compact--refuse-edit
  "<remap> <undo>" #'beets-edit-compact-undo
  "<remap> <undo-redo>" #'beets-edit-compact-redo
  "<remap> <beets-edit-move-document-up>" #'beets-edit-compact-move-up
  "<remap> <beets-edit-move-document-down>" #'beets-edit-compact-move-down
  "<remap> <next-line>" #'beets-edit-compact-next
  "<remap> <previous-line>" #'beets-edit-compact-previous
  "<remap> <beets-edit-next-document>" #'beets-edit-compact-next
  "<remap> <beets-edit-previous-document>" #'beets-edit-compact-previous
  "<remap> <forward-char>" #'beets-edit-compact-forward-cell
  "<remap> <backward-char>" #'beets-edit-compact-backward-cell
  "<remap> <right-char>" #'beets-edit-compact-forward-cell
  "<remap> <left-char>" #'beets-edit-compact-backward-cell
  "<remap> <move-beginning-of-line>" #'beets-edit-compact-first-cell
  "<remap> <move-end-of-line>" #'beets-edit-compact-last-cell
  "<remap> <beginning-of-buffer>" #'beets-edit-compact-first-row
  "<remap> <end-of-buffer>" #'beets-edit-compact-last-row
  "<remap> <forward-page>" #'beets-edit-compact-next
  "<remap> <backward-page>" #'beets-edit-compact-previous
  "<remap> <scroll-up-command>" #'beets-edit-compact-scroll-up
  "<remap> <scroll-down-command>" #'beets-edit-compact-scroll-down
  "<remap> <beets-edit-normalize>" #'beets-edit-compact-normalize
  "<remap> <beets-edit-set-field>" #'beets-edit-compact-edit-all)

;;;###autoload
(define-minor-mode beets-edit-compact-mode
  "Toggle a compact, one-row-per-document view of a beets edit buffer.

Fields whose values are identical across all documents move to a
caption on the tab line; each document renders as a single row of
the remaining fields, under column labels on the header line.
The buffer text is not changed in any way, and the view is
read-only; editing goes through commands, and disabling the mode
restores the raw buffer exactly."
  :lighter nil
  (cond (beets-edit-compact--saved
         (unless beets-edit-compact-mode
           (beets-edit-compact--teardown)))
        (beets-edit-compact-mode
         (unless (derived-mode-p 'beets-edit-mode)
           (setq beets-edit-compact-mode nil)
           (user-error "Not a beets edit buffer"))
         (beets-edit-compact--setup))))

(defun beets-edit-compact--setup ()
  "Enter the compact view.
Point moves to the cell of the field it was on in the raw view; fields
without a cell, such as the id or a shared field, fall back to the row's
home cell."
  (setq beets-edit-compact--saved
        (list :header header-line-format
              :tab-line tab-line-format
              :truncate truncate-lines
              :cursor-sensor cursor-sensor-mode
              :read-only buffer-read-only))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq beets-edit--inhibit-id-annotations t)
  (widen)
  (add-hook 'change-major-mode-hook #'beets-edit-compact--teardown nil t)
  (add-to-invisibility-spec 'beets-edit-compact)
  (add-hook 'occur-mode-find-occurrence-hook
            #'beets-edit-compact--reveal-occurrence nil t)
  (add-hook 'imenu-after-jump-hook
            #'beets-edit-compact--imenu-jump nil t)
  (cursor-sensor-mode 1)
  ;; The caption and the column labels are one visual unit, but themes style the
  ;; tab and header lines differently; render the caption as a header line,
  ;; resetting the tab line face underneath.
  (setq beets-edit-compact--caption-remap
        (face-remap-add-relative 'tab-line 'header-line 'default))
  (let ((start (beets-edit--document-start)))
    (beets-edit-compact--refresh)
    (when (beets-edit-compact--cells start)
      (beets-edit-compact--goto-field-cell)))
  (beets-edit--annotate-region (point-min) (point-max))
  ;; :before-while, so phantom matches are rejected before
  ;; `isearch-filter-visible' temporarily opens the fold around them; a search
  ;; failing on labels alone would otherwise leave the last examined document
  ;; spliced open.
  (add-function :before-while (local 'isearch-filter-predicate)
                #'beets-edit-compact--isearch-content-p)
  (add-hook 'pre-redisplay-functions
            #'beets-edit-compact--update-highlight nil t)
  (beets-edit-compact--update-highlight))

(defun beets-edit-compact--teardown ()
  "Leave the compact view, restoring the raw buffer display.
Point lands on the field of the cell it was on, mirroring how entering
the view lands on the cell of the field at point."
  (let ((field (let ((cell (beets-edit-compact--cell-at (point))))
                 (and cell
                      (overlay-get cell 'beets-edit-compact-field)))))
    (beets-edit-compact--teardown-1)
    (when field
      (goto-char (beets-edit--document-start))
      (beets-edit--goto-field field))))

(defun beets-edit-compact--teardown-1 ()
  "Restore the raw buffer display."
  (remove-function (local 'isearch-filter-predicate)
                   #'beets-edit-compact--isearch-content-p)
  (remove-hook 'change-major-mode-hook #'beets-edit-compact--teardown t)
  (remove-hook 'occur-mode-find-occurrence-hook
               #'beets-edit-compact--reveal-occurrence t)
  (remove-hook 'imenu-after-jump-hook
               #'beets-edit-compact--imenu-jump t)
  (remove-hook 'pre-redisplay-functions
               #'beets-edit-compact--update-highlight t)
  (remove-from-invisibility-spec 'beets-edit-compact)
  (beets-edit-compact--remove-overlays)
  (setq beets-edit--inhibit-id-annotations nil)
  (let ((saved beets-edit-compact--saved))
    (setq beets-edit-compact--saved nil)
    (setq header-line-format (plist-get saved :header))
    (setq tab-line-format (plist-get saved :tab-line))
    (setq truncate-lines (plist-get saved :truncate))
    (setq buffer-read-only (plist-get saved :read-only))
    (cursor-sensor-mode (if (plist-get saved :cursor-sensor) 1 -1)))
  (when beets-edit-compact--caption-remap
    (face-remap-remove-relative beets-edit-compact--caption-remap)
    (setq beets-edit-compact--caption-remap nil))
  (unless beets-edit-compact-mode
    (save-restriction
      (widen)
      (beets-edit--annotate-region (point-min) (point-max)))))

(provide 'beets-edit-compact)

;;; beets-edit-compact.el ends here
