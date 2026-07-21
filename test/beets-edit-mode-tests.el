;;; beets-edit-mode-tests.el --- Tests for beets-edit-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests run against fixtures in the exact shape the beets `edit' plugin emits;
;; see the fixtures directory.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'imenu)
(require 'beets-edit-mode)
(require 'beets-edit-compact)

(defconst beets-edit-tests--fixture-dir
  (expand-file-name "fixtures"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing captured beets edit buffers.")

(defmacro beets-edit-tests--with-fixture (name &rest body)
  "Run BODY in a temporary buffer containing fixture NAME."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents
      (expand-file-name ,name beets-edit-tests--fixture-dir))
     ,@body))

(ert-deftest beets-edit-set-field-repeated ()
  "Setting a field rewrites it in every document."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (should (= (beets-edit--set-field "album" "New Album") 3))
    (should (= (how-many "^album: New Album$" (point-min) (point-max)) 3))
    (should (= (how-many "Test Album" (point-min) (point-max)) 0))
    (should (= (how-many "^artist: Test Artist$" (point-min) (point-max)) 3))))

(ert-deftest beets-edit-set-field-absent ()
  "Setting an absent field adds it to every document."
  (beets-edit-tests--with-fixture "edit-album-default.yaml"
    (let ((docs (length (beets-edit--map-documents #'cons))))
      (should (= (beets-edit--set-field "composer" "X") docs))
      (should (= (how-many "^composer: X$" (point-min) (point-max)) docs))
      (should-not (beets-edit--malformed-lines))))
  ;; Mixed presence: existing values rewrite, missing ones appear.
  (with-temp-buffer
    (insert "album: A\nid: 1\n---\nalbum: B\ncomposer: Old\nid: 2\n")
    (should (= (beets-edit--set-field "composer" "New") 2))
    (should (equal (buffer-string)
                   (concat "album: A\nid: 1\ncomposer: New\n"
                           "---\nalbum: B\ncomposer: New\nid: 2\n"))))
  ;; A buffer without a final newline still gains a whole line.
  (with-temp-buffer
    (insert "album: A\nid: 1")
    (should (= (beets-edit--set-field "composer" "X") 1))
    (should (equal (buffer-string) "album: A\nid: 1\ncomposer: X\n")))
  ;; A separator ending the buffer, left by a deleted last document, starts no
  ;; further document.
  (with-temp-buffer
    (insert "album: A\nid: 1\n---\n")
    (should (= (beets-edit--set-field "composer" "X") 1))
    (should (equal (buffer-string)
                   "album: A\nid: 1\ncomposer: X\n---\n"))))

(ert-deftest beets-edit-compact-write-no-final-newline ()
  "The compact write adds a missing field on its own line at EOF."
  (with-temp-buffer
    (insert "album: A\nid: 1\ntitle: T\n---\nalbum: B\nid: 2\ntitle: U")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-max))
    (let ((start (beets-edit--document-start)))
      (beets-edit-compact--write start (beets-edit--document-end start)
                                 "composer" "X"))
    (should (string-suffix-p "title: U\ncomposer: X\n" (buffer-string)))
    (should-not (beets-edit--malformed-lines))))

(ert-deftest beets-edit-read-field ()
  "The field reader allows confirmed new names, rejecting bad ones."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "composer")))
    (should (equal (beets-edit--read-field "Field: " '("album")) "composer")))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Bad Name!")))
    (should-error (beets-edit--read-field "Field: " '("album"))
                  :type 'user-error))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "id")))
    (should-error (beets-edit--read-field "Field: " '("album"))
                  :type 'user-error)))

(ert-deftest beets-edit-compact-edit-all-new-field ()
  "E with a new field name adds a column to every document."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "composer"))
              ((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "Test Composer")))
      (beets-edit-compact-edit-all))
    (should (= (how-many "^composer: Test Composer$" (point-min) (point-max)) 3))
    ;; Identical everywhere, the new field lands in the caption.
    (should (string-match "composer: Test Composer" beets-edit-compact--caption-text))
    (should-not (beets-edit--malformed-lines))))

(ert-deftest beets-edit-set-field-wrapped-value ()
  "Setting a field with a wrapped value removes its continuation lines."
  (beets-edit-tests--with-fixture "edit-track-wrapped.yaml"
    (should (= (beets-edit--set-field "path" "/new/path.flac") 2))
    (should (= (how-many "^path: /new/path\\.flac$" (point-min) (point-max)) 2))
    (should (= (how-many "^[ \t]" (point-min) (point-max)) 0))))

(ert-deftest beets-edit-set-field-preserves-other-continuations ()
  "Setting one field leaves another field's continuation lines alone."
  (beets-edit-tests--with-fixture "edit-track-wrapped.yaml"
    (let ((continuations (how-many "^[ \t]" (point-min) (point-max))))
      (should (> continuations 0))
      (beets-edit--set-field "album" "New Album")
      (should (= (how-many "^[ \t]" (point-min) (point-max)) continuations)))))

(ert-deftest beets-edit-font-lock-id-hidden ()
  "The id line is invisible, newline included; text is unchanged."
  (dolist (fixture '("edit-track-default.yaml" "edit-import-track.yaml"))
    (beets-edit-tests--with-fixture fixture
      (beets-edit-mode)
      (let ((before (buffer-string)))
        (font-lock-ensure)
        (should (equal (buffer-string) before)))
      (goto-char (point-min))
      (re-search-forward "^id: -?[0-9]+\n")
      (should (eq (get-text-property (match-beginning 0) 'invisible)
                  'beets-edit-id))
      (should (eq (get-text-property (1- (match-end 0)) 'invisible)
                  'beets-edit-id))
      (should (memq 'beets-edit-id buffer-invisibility-spec))
      (goto-char (point-min))
      (re-search-forward "^album:")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'font-lock-variable-name-face)))))

(defun beets-edit-tests--annotations ()
  "Return the id annotation overlays in the current buffer, in order."
  (seq-sort-by #'overlay-start #'<
               (seq-filter (lambda (ov)
                             (overlay-get ov 'beets-edit--annotation))
                           (overlays-in (point-min) (point-max)))))

(ert-deftest beets-edit-id-annotations ()
  "Each document's first line is annotated with its id."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit--annotate-region (point-min) (point-max))
    (let ((annotations (beets-edit-tests--annotations)))
      (should (= (length annotations) 3))
      (should (= (overlay-start (car annotations)) (line-end-position)))
      (let ((str (overlay-get (car annotations) 'after-string)))
        (should (string-match "id 1" str))
        (should (get-text-property 0 'cursor str))
        (should (equal (get-text-property 0 'display str)
                       '(space :align-to (- right 5))))))))

(ert-deftest beets-edit-id-annotations-reorder ()
  "Annotations survive killing and yanking a whole document."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit--annotate-region (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward "^---$")
    (forward-line 1)
    (let* ((beg (point))
           (end (save-excursion
                  (re-search-forward "^---$")
                  (1+ (match-end 0))))
           (doc (delete-and-extract-region beg end)))
      (goto-char (point-max))
      (insert "---\n" (substring doc 0 -4)))
    (beets-edit--annotate-region (point-min) (point-max))
    (let ((annotations (beets-edit-tests--annotations)))
      (should (= (length annotations) 3))
      (should (string-match "id 2"
                            (overlay-get (car (last annotations))
                                         'after-string))))))

(ert-deftest beets-edit-count-documents-without-id ()
  "A deleted id line is counted; deleting the whole document is not."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (should (= (beets-edit--count-documents-without-id) 0))
    (goto-char (point-min))
    (re-search-forward "^id: 2\n")
    (delete-region (match-beginning 0) (match-end 0))
    (should (= (beets-edit--count-documents-without-id) 1))
    ;; Deleting a whole document, id and all, is not counted.
    (goto-char (point-min))
    (delete-region (point-min) (progn (re-search-forward "^---$")
                                      (forward-line 1) (point)))
    (should (= (beets-edit--count-documents-without-id) 1))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (beets-edit--check-ids) :type 'user-error))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (should-not (beets-edit--check-ids)))))

(ert-deftest beets-edit-mode-teardown ()
  "A major mode change removes guards, annotations, and snapshots."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit--annotate-region (point-min) (point-max))
    (text-mode)
    (should-not beets-edit--original-ids)
    (should-not (memq #'beets-edit--check-structure write-file-functions))
    (should-not (memq #'beets-edit--check-ids write-file-functions))
    (should-not (beets-edit-tests--annotations))))

(ert-deftest beets-edit-narrowed-enable ()
  "Enabling the mode narrowed still snapshots every id."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (narrow-to-region (point-min) 30)
    (beets-edit-mode)
    (widen)
    (should (equal beets-edit--original-ids '("1" "2" "3")))))

(ert-deftest beets-edit-toggle-ids ()
  "Toggling reveals and re-hides the raw id lines."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (font-lock-ensure)
    (let ((id-pos (progn (goto-char (point-min))
                         (re-search-forward "^id: ")
                         (match-beginning 0))))
      (should (invisible-p id-pos))
      (beets-edit-toggle-ids)
      ;; Not just spec membership: a leftover t member would keep hiding any
      ;; non-nil `invisible' property.
      (should-not (invisible-p id-pos))
      ;; The annotations stand in for the hidden lines; while the raw lines
      ;; show, they would say the same thing twice.
      (should-not (beets-edit-tests--annotations))
      (beets-edit-toggle-ids)
      (should (invisible-p id-pos))
      (should (beets-edit-tests--annotations)))
    (beets-edit-compact-mode 1)
    (should-error (beets-edit-toggle-ids) :type 'user-error)))

(ert-deftest beets-edit-mode-map-bindings ()
  "The mode map binds the field command."
  (should (eq (lookup-key beets-edit-mode-map (kbd "C-c C-f"))
              #'beets-edit-set-field)))

(ert-deftest beets-edit-buffer-p-fixtures ()
  "The detection predicate accepts all captured edit buffers."
  (dolist (name '("edit-track-default.yaml" "edit-album-default.yaml"
                  "edit-track-wrapped.yaml"
                  "edit-import-track.yaml" "edit-track-big.yaml"
                  "edit-album-multi.yaml"))
    (beets-edit-tests--with-fixture name
      (should (beets-edit-buffer-p)))))

(ert-deftest beets-edit-buffer-p-plain-yaml ()
  "The detection predicate rejects ordinary YAML."
  (with-temp-buffer
    (insert "album: x\ntitle: y\n")
    (should-not (beets-edit-buffer-p)))
  (with-temp-buffer
    (insert "config:\n  id: 3\n")
    (should-not (beets-edit-buffer-p)))
  (with-temp-buffer
    (insert "kind: note\ntext: y\n---\nkind: item\nid: 3\n")
    (should-not (beets-edit-buffer-p))))

(ert-deftest beets-edit-next-document-keeps-field ()
  "Moving between documents lands on the same field."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (re-search-forward "^title: ")
    (should (looking-at "Track 1"))
    (beets-edit-next-document)
    (should (looking-at "Track 2"))
    (beets-edit-next-document)
    (should (looking-at "Track 3"))
    (should-error (beets-edit-next-document) :type 'user-error)
    (beets-edit-previous-document)
    (should (looking-at "Track 2"))
    (beets-edit-previous-document)
    (should (looking-at "Track 1"))
    (should-error (beets-edit-previous-document) :type 'user-error)
    ;; A trailing separator starts no further document, and a refused move
    ;; leaves point in place.
    (goto-char (point-max))
    (insert "---\n")
    (re-search-backward "^title: Track 3")
    (search-forward "title: ")
    (should-error (beets-edit-next-document) :type 'user-error)
    (should (looking-at "Track 3"))))

(ert-deftest beets-edit-next-document-field-absent ()
  "Moving lands at the document start when the field is absent there."
  (with-temp-buffer
    (insert "a: 1\n---\nb: 2\n")
    (goto-char (point-min))
    (search-forward "a: ")
    (beets-edit-next-document)
    (should (looking-at "b: 2"))))

(ert-deftest beets-edit-page-delimiter ()
  "Documents are pages."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (should (equal page-delimiter "^---$"))
    (forward-page)
    (forward-line 1)
    (should (looking-at "album:"))))

(ert-deftest beets-edit-normalize-defaults ()
  "Default rules fold dashes, ellipses, spaces, invisibles, apostrophes."
  (with-temp-buffer
    (insert "title: A – B — C ‐ D…\n"
            "artist: Test's Artist\n"
            "album: Test Album Name​­\n")
    (should (> (beets-edit--normalize (point-min) (point-max)) 0))
    (should (equal (buffer-string)
                   (concat "title: A - B - C - D...\n"
                           "artist: Test’s Artist\n"
                           "album: Test Album Name\n")))))

(ert-deftest beets-edit-normalize-leaves-intentional ()
  "Curly quotes, letter apostrophes, and CJK text are not touched."
  (with-temp-buffer
    (insert "artist: ʻOkina\n"
            "title: “Test” ‘test’\n"
            "album: テストー、「試」！\n")
    (let ((before (buffer-string)))
      (should (= (beets-edit--normalize (point-min) (point-max)) 0))
      (should (equal (buffer-string) before)))))

(ert-deftest beets-edit-normalize-nfc ()
  "Decomposed characters are composed to normalization form C."
  (with-temp-buffer
    (insert "artist: Tést\n")
    (beets-edit--normalize (point-min) (point-max))
    (should (equal (buffer-string) "artist: Tést\n"))))

(ert-deftest beets-edit-normalize-compat ()
  "Roman numerals, ligatures, fraction slash, trailing space fold."
  (with-temp-buffer
    (insert "title: Track Ⅳ ﬁle 1⁄2  \n")
    (should (> (beets-edit--normalize (point-min) (point-max)) 0))
    (should (equal (buffer-string) "title: Track IV file 1/2\n"))))

(ert-deftest beets-edit-normalize-overlapping-apostrophes ()
  "Adjacent apostrophes sharing a word character all curl."
  (with-temp-buffer
    (insert "title: a'b'c\n")
    (beets-edit--normalize (point-min) (point-max))
    (should (equal (buffer-string) "title: a\u2019b\u2019c\n"))))

(ert-deftest beets-edit-normalize-region-bound ()
  "Normalization stays within the given bounds."
  (with-temp-buffer
    (insert "a–b\nc–d\n")
    (beets-edit--normalize 1 4)
    (should (equal (buffer-string) "a-b\nc–d\n"))))

(ert-deftest beets-edit-bad-ids ()
  "New and duplicated ids are flagged; reorder and deletion are not."
  (should (equal (beets-edit--bad-ids '("1" "2" "3") '("1" "99" "3")) '("99")))
  (should-not (beets-edit--bad-ids '("1" "2" "3") '("2" "1" "3")))
  (should-not (beets-edit--bad-ids '("1" "2" "3") '("1" "3")))
  (should (equal (beets-edit--bad-ids '("1" "2" "3") '("1" "2" "2" "3"))
                 '("2"))))

(ert-deftest beets-edit-id-check ()
  "Enabling the mode snapshots ids; saving with an edited id queries."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (should (equal beets-edit--original-ids '("1" "2" "3")))
    (should (memq #'beets-edit--check-ids write-file-functions))
    (should-not (beets-edit--check-ids))
    (goto-char (point-min))
    (re-search-forward "^id: 2$")
    (replace-match "id: 99")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (beets-edit--check-ids) :type 'user-error))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (should-not (beets-edit--check-ids)))))

(ert-deftest beets-edit-imenu-index ()
  "Imenu indexes documents at their row start, titled per buffer kind."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index)
                     '("1  Track 1" "2  Track 2" "3  Track 3")))
      ;; Entries point at document starts, not at the title lines.
      (should (= (cdr (car index)) (point-min)))
      (should (= (cdr (cadr index))
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^---$")
                   (1+ (point)))))))
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (beets-edit-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index)
                     '("Test Album" "Other Album")))
      (should (= (cdr (car index)) (point-min))))))

(ert-deftest beets-edit-clean-value ()
  "Values lose a trailing newline; inner newlines are rejected."
  (should (equal (beets-edit--clean-value "One line\n") "One line"))
  (should-error (beets-edit--clean-value "Two\nlines") :type 'user-error))

(ert-deftest beets-edit-compact-via-mode-hook ()
  "Enabling the compact view from the mode hook works."
  (let ((beets-edit-mode-hook (list #'beets-edit-compact-mode)))
    (beets-edit-tests--with-fixture "edit-track-default.yaml"
      (beets-edit-mode)
      (should beets-edit-compact-mode)
      (should header-line-format)
      (should (string-match "track" (beets-edit-compact--header))))))

(ert-deftest beets-edit-font-lock-tab-indent ()
  "Tabs in indentation are highlighted as errors."
  (with-temp-buffer
    (insert "path: /a/long\n \tcontinuation\n")
    (beets-edit-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "\t")
    (should (eq (get-text-property (match-beginning 0) 'face) 'error))))

(ert-deftest beets-edit-field-names ()
  "Field names are collected uniquely from all documents."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (should (equal (beets-edit--field-names)
                   '("album" "artist" "title" "track")))))

(ert-deftest beets-edit-compact-documents ()
  "Documents parse with wrapped values folded."
  (beets-edit-tests--with-fixture "edit-track-wrapped.yaml"
    (let ((docs (beets-edit-compact--documents)))
      (should (= (length docs) 2))
      (should (string-suffix-p "01 Track 1.flac"
                               (cdr (assoc "path" (nth 2 (car docs)))))))))

(ert-deftest beets-edit-compact-shared-and-rows ()
  "Uniform fields are shared; a diverging field moves to the rows."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (equal (mapcar #'car shared) '("artist" "album")))
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("track" "title"))))
    (goto-char (point-min))
    (re-search-forward "^artist: Test Artist$")
    (replace-match "artist: Test Artist feat. Other")
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (equal (mapcar #'car shared) '("album")))
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("track" "title" "artist"))))))

(ert-deftest beets-edit-compact-single-document ()
  "A single document shares nothing and shows all fields in its row."
  (beets-edit-tests--with-fixture "edit-album-default.yaml"
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (= (length docs) 1))
      (should-not shared)
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("album" "albumartist"))))))

(ert-deftest beets-edit-compact-missing-field ()
  "A field absent from one document is not shared."
  (with-temp-buffer
    (insert "album: X\nid: 1\ntitle: A\n---\nalbum: X\nid: 2\n")
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (equal (mapcar #'car shared) '("album")))
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("title"))))))

(ert-deftest beets-edit-compact-toggle-roundtrip ()
  "The compact view changes nothing and tears down byte-identically."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (set-buffer-modified-p nil)
    (let ((before (buffer-string)))
      (beets-edit-compact-mode 1)
      (should (equal (buffer-string) before))
      (should-not (buffer-modified-p))
      (should (string-match "album: Test Album" beets-edit-compact--caption-text))
      (should (string-match "track  title" (beets-edit-compact--header)))
      (let ((overlays (seq-filter
                       (lambda (o) (overlay-get o 'beets-edit-compact))
                       (overlays-in (point-min) (point-max)))))
        ;; Three documents, each: two cells, a first-line remainder, a hidden
        ;; body, and for the first two a hidden separator.
        (should (= (length overlays) 14))
        ;; Track 2's row is not the highlighted one, so its cells carry the
        ;; plain default face.
        (should (seq-find (lambda (o)
                            (let ((row (overlay-get o 'display)))
                              (and (stringp row)
                                   (string-match "Track 2" row)
                                   (eq (get-text-property 0 'face row)
                                       'default))))
                          overlays))
        (should (seq-find (lambda (o) (overlay-get o 'invisible))
                          overlays)))
      (should buffer-read-only)
      (goto-char 10)
      ;; A refused edit must not even start: buffer read-only refuses before the
      ;; buffer is marked modified, unlike the text property alone.
      (should-error (insert "x") :type 'buffer-read-only)
      (should-not (buffer-modified-p))
      (should-error (beets-edit-compact--refuse-edit) :type 'user-error)
      (should-not (buffer-modified-p))
      (beets-edit-compact-mode -1)
      (should-not buffer-read-only)
      (should (equal (buffer-string) before))
      (should-not (buffer-modified-p))
      (should-not tab-line-format)
      (should-not (seq-filter
                   (lambda (o) (overlay-get o 'beets-edit-compact))
                   (overlays-in (point-min) (point-max)))))))

(ert-deftest beets-edit-compact-big-album ()
  "A thirty-track album shares album and artist and rows the rest."
  (beets-edit-tests--with-fixture "edit-track-big.yaml"
    ;; A cap below the longest title, so truncation is exercised.
    (let* ((beets-edit-compact-max-cell-width 50)
           (docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (= (length docs) 30))
      (should (equal (mapcar #'car shared) '("artist" "album")))
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("track" "title")))
      ;; The longest title exceeds the cell cap and truncates, and the truncated
      ;; cell records its full value for expansion.
      (let ((widths (beets-edit-compact--widths
                     docs '("track" "title"))))
        (should (= (cdr (assoc "title" widths))
                   beets-edit-compact-max-cell-width)))
      (beets-edit-mode)
      (beets-edit-compact-mode 1)
      (let ((cell (seq-find (lambda (o)
                              (overlay-get o 'beets-edit-compact-full))
                            (overlays-in (point-min) (point-max)))))
        (should cell)
        (should (string-match "…" (overlay-get cell 'display)))
        ;; The last cell also carries the row's id, in the shadow face the row
        ;; repaints must not clobber.
        (should (string-match "id -?[0-9]+\\'"
                              (overlay-get cell 'display)))
        (should (equal (get-text-property
                        (1- (length (overlay-get cell 'display)))
                        'face (overlay-get cell 'display))
                       '(:inherit (shadow default))))
        (should (string-match "Truncation"
                              (overlay-get cell
                                           'beets-edit-compact-full)))))))

(ert-deftest beets-edit-compact-multi-album ()
  "Multi-album documents share nothing and order columns canonically."
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      (should (= (length docs) 2))
      (should-not shared)
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("album" "albumartist" "year" "genres"
                       "label" "catalognum")))
      ;; List-valued genres parse into a joined value.
      (should (equal (cdr (assoc "genres" (nth 2 (car docs))))
                     "Test Genre"))
      (should (equal (cdr (assoc "genres" (nth 2 (cadr docs))))
                     "Other Genre")))))

(ert-deftest beets-edit-set-field-list-value ()
  "A list-valued field stays a YAML list, split on the display join."
  (with-temp-buffer
    (insert "album: X\ngenres:\n- A\n- B\nid: 1\n")
    (should (= (beets-edit--set-field "genres" "C; D") 1))
    (should (equal (buffer-string)
                   "album: X\ngenres:\n- C\n- D\nid: 1\n"))
    (should (equal (beets-edit--field-value "genres") "C; D"))
    (goto-char (point-min))
    (should (= (beets-edit--set-field "album" "Y; Z") 1))
    (should (string-match-p "^album: Y; Z$" (buffer-string)))))

(ert-deftest beets-edit-compact-normalize ()
  "Normalize works through the read-only view and refreshes rows."
  (beets-edit-tests--with-fixture "edit-track-big.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (> (how-many "‐" (point-min) (point-max)) 0))
    (beets-edit-compact-normalize)
    (should (= (how-many "‐" (point-min) (point-max)) 0))
    (should buffer-read-only)
    (should (seq-find (lambda (o)
                        (let ((row (overlay-get o 'display)))
                          (and (stringp row)
                               (string-match "Track 4 With a Unicode-Hyphen" row))))
                      (overlays-in (point-min) (point-max))))))

(ert-deftest beets-edit-compact-navigation ()
  "Document motion lands on rows and errors at the ends."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-next)
    (should (looking-at "album: Test Album"))
    (should (= (point) (beets-edit--document-start)))
    (beets-edit-compact-next)
    (should-error (beets-edit-compact-next) :type 'user-error)
    (beets-edit-compact-previous)
    (beets-edit-compact-previous)
    (should (= (point) (point-min)))
    (should-error (beets-edit-compact-previous) :type 'user-error)))

(ert-deftest beets-edit-compact-edit-one-document ()
  "Editing a shared field in one document demotes it to the rows."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-next)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "artist"))
              ((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "Test Artist feat. Other")))
      (beets-edit-compact-edit t))
    (should (= (how-many "^artist: Test Artist feat\\. Other$"
                         (point-min) (point-max))
               1))
    (should-not (string-match "artist:" beets-edit-compact--caption-text))
    (should (string-match "album: Test Album" beets-edit-compact--caption-text))
    (should buffer-read-only)
    (should (seq-find (lambda (o)
                        (let ((row (overlay-get o 'display)))
                          (and (stringp row)
                               (string-match "feat\\. Other" row))))
                      (overlays-in (point-min) (point-max))))))

(ert-deftest beets-edit-compact-edit-all-documents ()
  "Editing all documents keeps the field shared with the new value."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "album"))
              ((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "Renamed")))
      (beets-edit-compact-edit-all))
    (should (= (how-many "^album: Renamed$" (point-min) (point-max)) 3))
    (should (string-match "album: Renamed" beets-edit-compact--caption-text))))

(ert-deftest beets-edit-compact-column-preserved ()
  "Row motion keeps the column; entering the view maps the field."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (re-search-forward "^---$")
    (re-search-forward "^title: ")
    (beets-edit-compact-mode 1)
    ;; Point was on doc 2's title, so it lands on doc 2's title cell.
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    (should (> (beets-edit--document-start) (point-min)))
    ;; Up and down keep the column.
    (beets-edit-compact-previous)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    (beets-edit-compact-next)
    (beets-edit-compact-next)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    (beets-edit-compact-mode -1))
  ;; A field without a cell, like the id, falls back to the home column: the
  ;; first content column, skipping the track number.
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (re-search-forward "^id: ")
    (beets-edit-compact-mode 1)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))))

(ert-deftest beets-edit-compact-row-highlight ()
  "The row at point is highlighted; other rows are not."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (beets-edit-compact-mode 1)
    (beets-edit-compact--update-highlight)
    (let ((current (beets-edit-compact--cell-at (point))))
      (should (equal (get-text-property 0 'face
                                        (overlay-get current 'display))
                     '(:inherit (hl-line default)))))
    (beets-edit-compact-next)
    (beets-edit-compact--update-highlight)
    (let ((above (beets-edit-compact--cell-at (point-min)))
          (current (beets-edit-compact--cell-at (point))))
      (should (eq (get-text-property 0 'face (overlay-get above 'display))
                  'default))
      (should (equal (get-text-property 0 'face
                                        (overlay-get current 'display))
                     '(:inherit (hl-line default)))))
    ;; A foreign overlay's face, such as a preview highlight, takes precedence
    ;; over hl-line on the row's cells; the region, whose extent the row cannot
    ;; render faithfully, is left out, and so are fragment-level marks such as a
    ;; spell checker's on the anchor line's first word.
    (let ((preview (make-overlay (beets-edit--document-start)
                                 (line-end-position)))
          (region (make-overlay (beets-edit--document-start)
                                (line-end-position)))
          (fragment (make-overlay (beets-edit--document-start)
                                  (+ (beets-edit--document-start) 5))))
      (overlay-put preview 'face 'highlight)
      (overlay-put region 'face 'region)
      (overlay-put fragment 'face 'flyspell-incorrect)
      (beets-edit-compact--update-highlight)
      (should (equal (get-text-property
                      0 'face
                      (overlay-get (beets-edit-compact--cell-at (point))
                                   'display))
                     '(:inherit (highlight hl-line default))))
      (delete-overlay preview)
      (delete-overlay region)
      (delete-overlay fragment))))

(ert-deftest beets-edit-compact-lifecycle ()
  "The view survives re-enabling, mode changes, and narrowing."
  ;; Documents that differ only by id render as blank rows, no crash.
  (with-temp-buffer
    (insert "album: X\nid: 1\n---\nalbum: X\nid: 2\n")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should beets-edit-compact-mode)
    (should (string-match "album: X" beets-edit-compact--caption-text))
    (should-not (beets-edit-compact--cells (point-min)))
    (beets-edit-compact-mode -1))
  ;; Not a beets buffer: refused, and nothing is touched.
  (with-temp-buffer
    (insert "prose\n")
    (should-error (beets-edit-compact-mode 1) :type 'user-error)
    (should-not beets-edit-compact-mode)
    (should-not buffer-read-only))
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    ;; Re-enabling does not clobber the saved chrome.
    (beets-edit-compact-mode 1)
    (beets-edit-compact-mode 1)
    (beets-edit-compact-mode -1)
    (should-not buffer-read-only)
    (should-not tab-line-format)
    ;; A major mode change while active tears the view down.
    (beets-edit-compact-mode 1)
    (text-mode)
    (should-not (seq-filter (lambda (o)
                              (overlay-get o 'beets-edit-compact))
                            (overlays-in (point-min) (point-max))))
    (should-not buffer-read-only))
  ;; Enabling while narrowed widens; disabling while narrowed still removes
  ;; every overlay.
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (narrow-to-region (point-min) (beets-edit--document-end (point-min)))
    (beets-edit-compact-mode 1)
    (should-not (buffer-narrowed-p))
    (should (= (length (beets-edit-compact--cells (point-min))) 2))
    (narrow-to-region (point-min) (beets-edit--document-end (point-min)))
    (beets-edit-compact-mode -1)
    (widen)
    (should-not (seq-filter (lambda (o)
                              (overlay-get o 'beets-edit-compact))
                            (overlays-in (point-min) (point-max))))))

(ert-deftest beets-edit-compact-lumped-fields ()
  "A short first line renders its overflow fields in the last cell.
The expanded rendering carries every overflow field untruncated."
  (with-temp-buffer
    (insert "a: 1\nb: alpha\nc: beta\nd: gamma\ne: delta"
            (make-string 65 ?e)
            "\nf: eps\nid: 1\n"
            "---\n"
            "a: 2\nb: b2\nc: c2\nd: d2\ne: e2\nf: f2\nid: 2\n")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (let* ((cells (beets-edit-compact--cells (point-min)))
           (last (car (last cells))))
      (should (= (length cells) 4))
      (should (string-match "delta" (overlay-get last 'display)))
      (should (string-match "eps" (overlay-get last 'display)))
      (let ((full (overlay-get last 'beets-edit-compact-full)))
        (should (string-match (make-string 65 ?e) full))
        (should (string-match "eps" full))))))

(ert-deftest beets-edit-compact-fold-no-final-newline ()
  "The fold covers a buffer that lacks a final newline."
  (with-temp-buffer
    (insert "title: Track 1\nid: 1\n---\ntitle: Track 2\nid: 2")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (invisible-p (1- (point-max)))))
  (with-temp-buffer
    (insert "title: Track 1\nid: 1\n---")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (invisible-p (1- (point-max))))))

(ert-deftest beets-edit-compact-cell-motion ()
  "Cell motion moves between columns, flowing across row edges."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "track"))
    (beets-edit-compact-forward-cell)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    ;; Past the row's last cell, flow onto the next row.
    (beets-edit-compact-forward-cell)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "track"))
    (should (> (beets-edit--document-start) (point-min)))
    ;; And back again.
    (beets-edit-compact-backward-cell)
    (should (= (point) (1+ (point-min))))
    (beets-edit-compact-last-cell)
    (should (= (point) (1+ (point-min))))
    ;; Home is the first content column: title, not track.
    (beets-edit-compact-first-cell)
    (should (= (point) (1+ (point-min))))
    (beets-edit-compact-backward-cell)
    (should (= (point) (point-min)))
    ;; The table's first and last cells clamp.
    (beets-edit-compact-backward-cell)
    (should (= (point) (point-min)))
    (beets-edit-compact-last-row)
    (beets-edit-compact-last-cell)
    (let ((last (point)))
      (beets-edit-compact-forward-cell)
      (should (= (point) last)))
    ;; Every spelling of cell motion runs the same commands.
    (dolist (key (list [remap forward-char] [remap right-char]
                       (kbd "TAB")))
      (should (eq (lookup-key beets-edit-compact-mode-map key)
                  #'beets-edit-compact-forward-cell)))
    (dolist (key (list [remap backward-char] [remap left-char]
                       (kbd "<backtab>")))
      (should (eq (lookup-key beets-edit-compact-mode-map key)
                  #'beets-edit-compact-backward-cell)))))

(ert-deftest beets-edit-compact-edit-at-cell ()
  "Without a prefix, e edits the cell at point without asking."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-next)
    (beets-edit-compact-forward-cell)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Should not prompt for a field")))
              ((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "Renamed")))
      (beets-edit-compact-edit))
    (should (= (how-many "^title: Renamed$" (point-min) (point-max)) 1))
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))))

(ert-deftest beets-edit-compact-edit-anchored ()
  "Edits skip on no change, add missing fields, and track the id."
  ;; An unchanged value writes nothing, preserving list fields.
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (let ((before (buffer-string)))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "genres"))
                ((symbol-function 'beets-edit--read-value)
                 (lambda (_field current) current)))
        (beets-edit-compact-edit t))
      (should (equal (buffer-string) before))
      (should (= (how-many "^- Test Genre$" (point-min) (point-max)) 1))))
  ;; A field rendered from the union but absent here gets added.
  (with-temp-buffer
    (insert "album: X\nid: 1\ntitle: A\n---\nalbum: X\nid: 2\n")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-next)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "title"))
              ((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "B")))
      (beets-edit-compact-edit t))
    (should (= (how-many "^title: B$" (point-min) (point-max)) 1))
    (should (equal (beets-edit--ids) '("1" "2"))))
  ;; Buffer edits during the value editor cannot misdirect the write.
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-next)
    (beets-edit-compact-forward-cell)
    (cl-letf (((symbol-function 'beets-edit--read-value)
               (lambda (&rest _)
                 (save-excursion
                   (goto-char (point-min))
                   (let ((inhibit-read-only t))
                     (insert "comments: Test comment\n")))
                 "Renamed")))
      (beets-edit-compact-edit))
    (should (= (how-many "^title: Renamed$" (point-min) (point-max)) 1))
    (should (string-match "^id: 2$"
                          (buffer-substring-no-properties
                           (beets-edit--document-start)
                           (beets-edit--document-end
                            (beets-edit--document-start)))))))

(ert-deftest beets-edit-compact-isearch ()
  "Hidden values are searchable and exits land on their cell."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (re-search-forward "Track 2")
    (let* ((beg (match-beginning 0))
           (overlay (seq-find (lambda (o)
                                (overlay-get o 'isearch-open-invisible))
                              (overlays-at beg))))
      (should overlay)
      (should (overlay-get overlay 'invisible))
      ;; The standard filter accepts the hidden match, opening the overlay
      ;; temporarily as it would mid-search.
      (let ((search-invisible 'open)
            (isearch-opened-overlays nil))
        (should (isearch-filter-visible beg (+ beg 7))))
      ;; Exiting the search on the hidden title line lands on the title cell
      ;; with the fold restored.  Isearch reads the match data right after
      ;; calling the opener; it must come back untouched.
      (string-match "Track \\(2\\)" "Track 2")
      (beets-edit-compact--isearch-temporary overlay nil)
      (should (equal (match-data) (progn (string-match "Track \\(2\\)"
                                                       "Track 2")
                                         (match-data))))
      (beets-edit-compact--isearch-temporary overlay nil)
      (should-not (overlay-get overlay 'invisible))
      ;; The unfolded document shows its whole raw source: the row's cells
      ;; suspend their rendering so the real first line shows, the id line is
      ;; visible in place, and the separator's newline is swallowed so no blank
      ;; line stands in for it.
      (let ((cells (beets-edit-compact--cells
                    (save-excursion (goto-char beg)
                                    (beets-edit--document-start)))))
        (should (seq-every-p
                 (lambda (c) (null (overlay-get c 'display))) cells))
        (should (seq-every-p
                 (lambda (c) (overlay-get c 'beets-edit-compact-suspended))
                 cells))
        ;; The row stands above the raw text as an overlay string.
        (let ((copy (overlay-get (car cells) 'before-string)))
          (should (string-suffix-p "\n" copy))
          (should (string-match-p "Track 2"
                                  (substring-no-properties copy)))))
      (let ((id-pos (save-excursion
                      (goto-char (overlay-start overlay))
                      (re-search-forward "^id: 2$" (overlay-end overlay))
                      (match-beginning 0))))
        (should-not (get-text-property id-pos 'invisible))
        (should (seq-find (lambda (o)
                            (and (overlay-get o 'beets-edit-compact)
                                 (overlay-get o 'invisible)
                                 (= (overlay-end o)
                                    (+ (overlay-start o) 4))))
                          (overlays-in (overlay-end overlay)
                                       (point-max))))
        (goto-char beg)
        (beets-edit-compact--isearch-open overlay)
        (should (get-text-property id-pos 'invisible)))
      (should (overlay-get overlay 'invisible))
      (let ((cells (beets-edit-compact--cells
                    (save-excursion (goto-char beg)
                                    (beets-edit--document-start)))))
        (should (seq-every-p (lambda (c) (overlay-get c 'display)) cells))
        (should-not (seq-some
                     (lambda (c)
                       (overlay-get c 'beets-edit-compact-suspended))
                     cells))
        (should-not (overlay-get (car cells) 'before-string)))
      (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                  'beets-edit-compact-field)
                     "title"))
      (should (> (beets-edit--document-start) (point-min))))))

(ert-deftest beets-edit-compact-unfold-id-first-line ()
  "Unfolding a document whose first line is the id shows it raw."
  (with-temp-buffer
    (insert "id: 1\ntitle: A\ntrack: 1\n---\nid: 2\ntitle: B\ntrack: 2\n")
    (beets-edit-mode)
    (font-lock-ensure)
    (beets-edit-compact-mode 1)
    (let ((overlay (seq-find (lambda (o)
                               (overlay-get o 'isearch-open-invisible))
                             (overlays-in (point-min)
                                          (beets-edit--document-end
                                           (point-min)))))
          (isearch-mode t))
      (beets-edit-compact--isearch-temporary overlay nil)
      ;; The id line, the document's first, is revealed in place.
      (should-not (invisible-p (point-min)))
      (should-not (overlay-get (car (beets-edit-compact--cells (point-min)))
                               'display))
      (beets-edit-compact--isearch-temporary overlay t)
      (should (invisible-p (point-min)))
      (should (overlay-get (car (beets-edit-compact--cells (point-min)))
                           'display)))))

(ert-deftest beets-edit-compact-isearch-content-filter ()
  "Matches entirely under a row's cells are not search stops."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    ;; "al" of the hidden "album:" label, under the two cells.
    (should-not (funcall isearch-filter-predicate
                         (point-min) (+ (point-min) 2)))
    ;; Text under cells is not a stop even while its document is unfolded; a
    ;; stop there could not survive the refold.
    (let ((overlay (seq-find (lambda (o)
                               (overlay-get o 'isearch-open-invisible))
                             (overlays-in (point-min)
                                          (beets-edit--document-end
                                           (point-min)))))
          (isearch-mode t))
      (beets-edit-compact--isearch-temporary overlay nil)
      (should-not (funcall isearch-filter-predicate
                           (point-min) (+ (point-min) 2)))
      (beets-edit-compact--isearch-temporary overlay t))
    ;; A label fragment in the folded body is not a stop either.
    (goto-char (point-min))
    (re-search-forward "lbu")
    (should-not (funcall isearch-filter-predicate
                         (match-beginning 0) (match-end 0)))
    ;; A match in the openable body is a real stop.
    (goto-char (point-min))
    (re-search-forward "Track 2")
    (let ((search-invisible 'open)
          (isearch-opened-overlays nil))
      (should (funcall isearch-filter-predicate
                       (match-beginning 0) (match-end 0))))))

(ert-deftest beets-edit-compact-preview-safe ()
  "Preview-style openers unfold only around their actual target.
Consult and friends temporarily open every openable overlay on the
target line.  Previewing a row must leave it rendered: the handler
no-ops when point is outside the overlay and Isearch is not running.  A
preview landing inside the folded body, as a grep jump does, unfolds the
document for real."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (let* ((eol (save-excursion (goto-char (point-min))
                                (line-end-position)))
           (remainder (seq-find (lambda (o)
                                  (overlay-get o 'isearch-open-invisible))
                                (overlays-in (point-min) eol)))
           (body (seq-find (lambda (o)
                             (overlay-get o 'isearch-open-invisible))
                           (overlays-at eol))))
      (should remainder)
      ;; Previewing the row: point at the document start, outside the remainder;
      ;; nothing unfolds.
      (goto-char (point-min))
      (beets-edit-compact--isearch-temporary remainder nil)
      (should (overlay-get remainder 'invisible))
      (should (overlay-get (car (beets-edit-compact--cells (point-min)))
                           'display))
      (beets-edit-compact--isearch-temporary remainder t)
      ;; Previewing into the body: point inside the fold; the document unfolds
      ;; and refolds.
      (goto-char (1+ (overlay-start body)))
      (beets-edit-compact--isearch-temporary body nil)
      (should-not (overlay-get body 'invisible))
      (should-not (overlay-get (car (beets-edit-compact--cells (point-min)))
                               'display))
      (should (overlay-get (car (beets-edit-compact--cells (point-min)))
                           'before-string))
      (beets-edit-compact--isearch-temporary body t)
      (should (overlay-get body 'invisible))
      (should (overlay-get (car (beets-edit-compact--cells (point-min)))
                           'display)))))

(ert-deftest beets-edit-compact-first-line-value-searchable ()
  "A value living on the document's first line is a real match."
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (re-search-forward "^album: \\(.+\\)$")
    (let ((beg (match-beginning 1))
          (end (match-end 1))
          (search-invisible 'open)
          (isearch-opened-overlays nil))
      (should (funcall isearch-filter-predicate beg end))
      ;; The document opened around the match; close it again.
      (dolist (o isearch-opened-overlays)
        (beets-edit-compact--isearch-temporary o t))
      (should (overlay-get (car (beets-edit-compact--cells (point-min)))
                           'display)))))

(ert-deftest beets-edit-compact-remaps ()
  "Raw document motion routes through the view."
  (should (eq (lookup-key beets-edit-compact-mode-map
                          [remap beets-edit-next-document])
              #'beets-edit-compact-next))
  (should (eq (lookup-key beets-edit-compact-mode-map
                          [remap beets-edit-previous-document])
              #'beets-edit-compact-previous)))

(ert-deftest beets-edit-compact-cell-expansion ()
  "Entering a truncated cell expands it; leaving restores it."
  (beets-edit-tests--with-fixture "edit-track-big.yaml"
    (beets-edit-mode)
    ;; A cap below the longest title, so truncation is exercised.
    (setq-local beets-edit-compact-max-cell-width 50)
    (beets-edit-compact-mode 1)
    (let* ((cell (seq-find (lambda (o)
                             (overlay-get o 'beets-edit-compact-full))
                           (overlays-in (point-min) (point-max))))
           (short (overlay-get cell 'display)))
      (goto-char (overlay-start cell))
      (beets-edit-compact--sensor nil nil 'entered)
      (should (equal (overlay-get cell 'display)
                     (overlay-get cell 'beets-edit-compact-full)))
      ;; The expanded rendering keeps the row id and takes the row highlight
      ;; over its whole value, suffix excluded.
      (let* ((expanded (overlay-get cell 'display))
             (suffix (length
                      (overlay-get cell 'beets-edit-compact-suffix))))
        (should (string-match "id -?[0-9]+\\'" expanded))
        (beets-edit-compact--set-cell-face cell '(hl-line))
        (let ((expanded (overlay-get cell 'display)))
          (should (equal (get-text-property
                          (1- (- (length expanded) suffix)) 'face expanded)
                         '(:inherit (hl-line default))))
          ;; The expanded cell is on the highlighted row, whose faces now cover
          ;; the id suffix too.
          (should (equal (get-text-property (1- (length expanded))
                                            'face expanded)
                         '(:inherit (shadow hl-line default))))))
      (beets-edit-compact--sensor nil nil 'left)
      (should (equal (substring-no-properties (overlay-get cell 'display))
                     (substring-no-properties short))))))

(defun beets-edit-tests--tracks-by-position ()
  "Return the track values of the buffer's documents, in order."
  (mapcar (lambda (doc) (cdr (assoc "track" (nth 2 doc))))
          (beets-edit-compact--documents)))

(ert-deftest beets-edit-move-document ()
  "Moving a document reorders the tracklist, not just the text."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (let ((before (buffer-string)))
      (goto-char (point-min))
      (re-search-forward "^---$")
      (re-search-forward "^title: ")
      (beets-edit-move-document-up)
      ;; Content moved, but the track sequence still reads 1 2 3.
      (should (equal (beets-edit--ids) '("2" "1" "3")))
      (should (equal (beets-edit-tests--tracks-by-position)
                     '("1" "2" "3")))
      (should (looking-at "Track 2"))
      (should-not (beets-edit--malformed-lines))
      (should (= (beets-edit--count-documents-without-id) 0))
      (beets-edit-move-document-down)
      (should (equal (buffer-string) before))
      ;; The last document, without a trailing separator, moves too.
      (beets-edit-move-document-down)
      (should (equal (beets-edit--ids) '("1" "3" "2")))
      (should (equal (beets-edit-tests--tracks-by-position)
                     '("1" "2" "3")))
      (should (looking-at "Track 2"))
      (should-error (beets-edit-move-document-down) :type 'user-error)
      (goto-char (point-min))
      (should-error (beets-edit-move-document-up) :type 'user-error)))
  ;; Documents without positional fields transpose as plain text.
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (beets-edit-move-document-down)
    (should (equal (beets-edit--ids) '("2" "1")))))

(ert-deftest beets-edit-move-document-edges ()
  "Trailing separators are not documents; negative counts mirror."
  (with-temp-buffer
    (insert "title: a\nid: 1\n---\n")
    (goto-char (point-min))
    (let ((before (buffer-string)))
      (should-error (beets-edit-move-document-down) :type 'user-error)
      (should (equal (buffer-string) before))))
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (beets-edit-next-document)
    (beets-edit-move-document-down -1)
    (should (equal (beets-edit--ids) '("2" "1" "3")))
    (beets-edit-move-document-up -1)
    (should (equal (beets-edit--ids) '("1" "2" "3")))))

(ert-deftest beets-edit-compact-undo-clean ()
  "In-view undo works, and undone text carries no view properties."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (setq buffer-undo-list nil)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (cl-letf (((symbol-function 'beets-edit--read-value)
               (lambda (&rest _) "Renamed")))
      (beets-edit-compact-edit))
    (should (= (how-many "^track: Renamed$" (point-min) (point-max)) 1))
    (should (eq (lookup-key beets-edit-compact-mode-map [remap undo])
                #'beets-edit-compact-undo))
    (should (eq (lookup-key beets-edit-compact-mode-map [remap undo-redo])
                #'beets-edit-compact-redo))
    (undo-boundary)
    (beets-edit-compact-undo)
    (should (= (how-many "^track: 1$" (point-min) (point-max)) 1))
    (undo-boundary)
    (beets-edit-compact-redo)
    (should (= (how-many "^track: Renamed$" (point-min) (point-max)) 1))
    (undo-boundary)
    (beets-edit-compact-undo)
    (should (= (how-many "^track: 1$" (point-min) (point-max)) 1))
    (beets-edit-compact-mode -1)
    (goto-char (point-min))
    (re-search-forward "^track: ")
    (should-not (get-text-property (point) 'read-only))
    (insert "x")
    (should (looking-back "x" 1))))

(ert-deftest beets-edit-compact-move ()
  "Rows transpose from the compact view, staying locked and current."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (beets-edit-compact-forward-cell)
    (beets-edit-compact-move-down)
    (should (equal (beets-edit--ids) '("2" "1" "3")))
    (should (equal (beets-edit-tests--tracks-by-position)
                   '("1" "2" "3")))
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    (should (> (beets-edit--document-start) (point-min)))
    (should buffer-read-only)
    (should (seq-find (lambda (o)
                        (let ((row (overlay-get o 'display)))
                          (and (stringp row)
                               (string-match "Track 2" row)
                               (< (overlay-start o)
                                  (beets-edit--document-start)))))
                      (overlays-in (point-min) (point-max))))))

(ert-deftest beets-edit-case-sensitivity ()
  "Structural matchers do not case-fold, whatever the buffer says."
  (with-temp-buffer
    (insert "Track: 9\nid: 1\n")
    ;; The uppercase line is not a match; the field is added instead.
    (beets-edit--set-field "track" "5")
    (should (= (how-many "^Track: 9$" (point-min) (point-max)) 1))
    (should (= (how-many "^track: 5$" (point-min) (point-max)) 1))
    (should (equal (beets-edit--malformed-lines) '(1)))
    (should-not (member "Track" (beets-edit--field-names))))
  (with-temp-buffer
    (insert "Key: value\nID: 123\n")
    (should-not (beets-edit-buffer-p))))

(ert-deftest beets-edit-buffer-p-bare-first-field ()
  "Detection accepts a list-valued first field with no value text."
  (with-temp-buffer
    (insert "genres:\n- Rock\nid: 1\n")
    (should (beets-edit-buffer-p))))

(ert-deftest beets-edit-malformed-lines ()
  "Structurally damaged lines are detected; intact buffers pass."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (should-not (beets-edit--malformed-lines))
    (should-not (beets-edit--check-structure))
    (goto-char (point-min))
    (re-search-forward "^title: ")
    (beginning-of-line)
    (delete-char 7)
    (should (equal (beets-edit--malformed-lines) '(4)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (beets-edit--check-structure) :type 'user-error))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (should-not (beets-edit--check-structure))))
  ;; List item lines are part of the format, not damage.
  (with-temp-buffer
    (insert "genres:\n- Rock\n- \nid: 1\n")
    (should-not (beets-edit--malformed-lines)))
  ;; Continuations and items with no field to belong to are damage.
  (with-temp-buffer
    (insert "  stray\nalbum: x\nid: 1\n---\n- orphan\nid: 2\n")
    (should (equal (beets-edit--malformed-lines) '(1 5)))))

(ert-deftest beets-edit-buffer-p-widens ()
  "Detection sees past the narrowing `set-auto-mode' applies."
  (with-temp-buffer
    (insert "album: " (make-string 4200 ?x) "\nid: 1\n")
    (narrow-to-region (point-min) 4000)
    (should (beets-edit-buffer-p))))

(ert-deftest beets-edit-compact-row-highlight-covers-id ()
  "The row faces extend across the gap and the id suffix."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (beets-edit-compact-mode 1)
    (beets-edit-compact--update-highlight)
    (let* ((cells (beets-edit-compact--cells (point-min)))
           (display (overlay-get (car (last cells)) 'display))
           (len (length display)))
      (should (equal (get-text-property (1- len) 'face display)
                     '(:inherit (shadow hl-line default))))
      (should (equal (get-text-property (- len 7) 'face display)
                     '(:inherit (hl-line default)))))))

(ert-deftest beets-edit-compact-highlight-changes ()
  "Cells of changed fields render the change face themselves."
  (require 'hilit-chg)
  (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t)))
    (beets-edit-tests--with-fixture "edit-track-default.yaml"
      (beets-edit-mode)
      (highlight-changes-mode 1)
      (goto-char (point-min))
      (beets-edit-compact-mode 1)
      (beets-edit-compact-next)
      (beets-edit-compact-move-down)
      (let* ((start (beets-edit--document-start))
             (cell (car (beets-edit-compact--cells start))))
        (should (equal (overlay-get cell 'beets-edit-compact-base)
                       '(highlight-changes)))
        (should (equal (get-text-property
                        0 'face (overlay-get cell 'display))
                       '(:inherit (highlight-changes default)))))
      ;; The untouched first document's cells carry no change face.
      (let ((cell (car (beets-edit-compact--cells (point-min)))))
        (should-not (overlay-get cell 'beets-edit-compact-base))))))

(ert-deftest beets-edit-compact-changed-fields-continuation ()
  "A change on a continuation line counts for its field."
  (with-temp-buffer
    (insert "comments: Test comment\n  wrapped tail\nid: 1\n")
    (setq-local highlight-changes-mode t)
    (goto-char (point-min))
    (forward-line 1)
    (put-text-property (point) (line-end-position) 'hilit-chg 'chg)
    (should (equal (beets-edit-compact--changed-fields
                    (point-min) (point-max))
                   '("comments")))))

(ert-deftest beets-edit-compact-buffer-motion ()
  "M-< and M-> land on home cells; pages move by row."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (eq (lookup-key beets-edit-compact-mode-map
                            [remap beginning-of-buffer])
                #'beets-edit-compact-first-row))
    (should (eq (lookup-key beets-edit-compact-mode-map
                            [remap end-of-buffer])
                #'beets-edit-compact-last-row))
    (should (eq (lookup-key beets-edit-compact-mode-map
                            [remap forward-page])
                #'beets-edit-compact-next))
    (should (eq (lookup-key beets-edit-compact-mode-map
                            [remap scroll-up-command])
                #'beets-edit-compact-scroll-up))
    (should (eq (lookup-key beets-edit-compact-mode-map
                            [remap scroll-down-command])
                #'beets-edit-compact-scroll-down))
    (beets-edit-compact-last-row)
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))
    (should (> (beets-edit--document-start) (point-min)))
    (beets-edit-compact-first-row)
    (should (= (beets-edit--document-start) (point-min)))
    (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                'beets-edit-compact-field)
                   "title"))))

(ert-deftest beets-edit-compact-quit-lands-on-field ()
  "Leaving the view puts point on the field of the cell at point."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-min))
    (beets-edit-compact-mode 1)
    (beets-edit-compact-next)
    (beets-edit-compact-first-cell)
    (beets-edit-compact-mode -1)
    (should (looking-back "^title: " (pos-bol)))
    (should (looking-at "Track 2"))))

(ert-deftest beets-edit-compact-refuses-deletions ()
  "Deletion and yank commands explain the editing commands."
  (should (eq (lookup-key beets-edit-compact-mode-map [remap delete-char])
              #'beets-edit-compact--refuse-edit))
  (should (eq (lookup-key beets-edit-compact-mode-map [remap yank])
              #'beets-edit-compact--refuse-edit))
  (should (eq (lookup-key beets-edit-compact-mode-map [remap kill-line])
              #'beets-edit-compact--refuse-edit)))

(ert-deftest beets-edit-compact-imenu-jump ()
  "Imenu jumps land on the row's home cell."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (memq #'beets-edit-compact--imenu-jump imenu-after-jump-hook))
    (let ((entry (nth 1 (beets-edit--imenu-index))))
      (goto-char (cdr entry))
      (run-hooks 'imenu-after-jump-hook)
      (should (equal (overlay-get (beets-edit-compact--cell-at (point))
                                  'beets-edit-compact-field)
                     "title"))
      (should (> (beets-edit--document-start) (point-min))))))

(ert-deftest beets-edit-compact-tab-line-round-trip ()
  "Buffer tabs from `tab-line-mode' survive a compact round trip.
The caption borrows the tab line slot while the view is active; the
tabs, and only this buffer's, must come back on exit."
  (require 'tab-line)
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (tab-line-mode 1)
    (let ((tabs tab-line-format))
      (should tabs)
      (beets-edit-compact-mode 1)
      (should (string-match "album" (beets-edit-compact--tab-line)))
      (beets-edit-compact-mode -1)
      (should (equal tab-line-format tabs))
      (should tab-line-mode))))

(ert-deftest beets-edit-compact-highlight-changes-current-row ()
  "The row highlight does not spread one field's change mark.
The change overlay covers the row's anchor line, so sampling it as a row
face would paint every cell as changed."
  (require 'hilit-chg)
  (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t)))
    (beets-edit-tests--with-fixture "edit-track-default.yaml"
      (beets-edit-mode)
      (highlight-changes-mode 1)
      (beets-edit-compact-mode 1)
      (goto-char (point-min))
      (beets-edit-compact-next)
      (let ((inhibit-read-only t)
            (start (beets-edit--document-start)))
        (beets-edit-compact--write start (beets-edit--document-end start)
                                   "album" "Other"))
      (beets-edit-compact--refresh)
      (beets-edit-compact--goto-column (beets-edit--document-start) 0)
      (beets-edit-compact--update-highlight)
      (let* ((cells (beets-edit-compact--cells (beets-edit--document-start)))
             (cell (lambda (name)
                     (seq-find (lambda (c)
                                 (equal (overlay-get
                                         c 'beets-edit-compact-field)
                                        name))
                               cells))))
        (should (equal (get-text-property
                        0 'face (overlay-get (funcall cell "album") 'display))
                       '(:inherit (highlight-changes hl-line default))))
        (should (equal (get-text-property
                        0 'face (overlay-get (funcall cell "track") 'display))
                       '(:inherit (hl-line default))))))))

(ert-deftest beets-edit-compact-never-shared-fields ()
  "Position and identity fields keep their column when identical."
  (with-temp-buffer
    (insert "artist: Same\nid: 1\ntitle: Same Song\ntrack: 1\n---\n"
            "artist: Same\nid: 2\ntitle: Same Song\ntrack: 1\n")
    (beets-edit-mode)
    (let* ((docs (beets-edit-compact--documents))
           (shared (beets-edit-compact--shared-fields docs)))
      ;; The artist collapses; the identical title and track do not.
      (should (equal (mapcar #'car shared) '("artist")))
      (should (equal (beets-edit-compact--row-fields docs shared)
                     '("track" "title"))))
    ;; The exemption is customizable.
    (let ((beets-edit-compact-never-shared-fields nil))
      (should (equal (mapcar #'car (beets-edit-compact--shared-fields
                                    (beets-edit-compact--documents)))
                     '("track" "title" "artist"))))))

(ert-deftest beets-edit-compact-caption-percent ()
  "Values with % survive the tab line's mode-line format processing."
  (with-temp-buffer
    (insert "album: 100% Test\ntitle: A\nid: 1\n"
            "---\nalbum: 100% Test\ntitle: B\nid: 2\n")
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (should (string-match-p "100%% Test" beets-edit-compact--caption-text))))

(ert-deftest beets-edit-compact-isearch-bare-label ()
  "A list field's bare label is filtered like any other label."
  (beets-edit-tests--with-fixture "edit-album-multi.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (re-search-forward "^genres:$")
    (should-not (funcall isearch-filter-predicate
                         (match-beginning 0) (match-end 0)))))

(ert-deftest beets-edit-previous-document-overshoot ()
  "An overshooting count errors without moving point."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (goto-char (point-max))
    (beets-edit-previous-document)
    (let ((before (point)))
      (should-error (beets-edit-previous-document 5) :type 'user-error)
      (should (= (point) before)))
    (beets-edit-compact-mode 1)
    (let ((before (point)))
      (should-error (beets-edit-compact-previous 5) :type 'user-error)
      (should (= (point) before)))
    (goto-char (point-min))
    (beets-edit-compact-first-cell)
    (let ((before (point)))
      (should-error (beets-edit-compact-next 5) :type 'user-error)
      (should (= (point) before)))))

(ert-deftest beets-edit-compact-move-overshoot ()
  "An overshooting move still leaves the view refreshed."
  (beets-edit-tests--with-fixture "edit-track-default.yaml"
    (beets-edit-mode)
    (beets-edit-compact-mode 1)
    (goto-char (point-min))
    (should-error (beets-edit-compact-move-down 5) :type 'user-error)
    (dolist (start (let (starts)
                     (save-excursion
                       (goto-char (point-min))
                       (push (point) starts)
                       (while (re-search-forward "^---$" nil t)
                         (push (1+ (point)) starts)))
                     starts))
      (should (beets-edit-compact--cells start)))))

(provide 'beets-edit-mode-tests)

;;; beets-edit-mode-tests.el ends here
