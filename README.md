# beets-edit-mode

An Emacs major mode for the metadata editing buffers created by the [beets](https://beets.io) `edit` plugin. It makes beets' own metadata editing more pleasant inside Emacs. The mode never touches your library: it only shapes the buffer text, which beets reads back and applies as usual when the edit ends.

The plugin hands your editor a stream of flat YAML documents, one per track or album, separated by `---` lines. The mode highlights the field names and separators, and flags tabs in indentation, since YAML forbids them there.

You should never edit the `id` lines. beets uses them to match the documents back to library items, and it silently ignores a document whose id is unknown. The mode hides these lines entirely and shows each id as a dim annotation at the right edge of its document instead. Saving warns if an id was edited, duplicated, or deleted without its document, and if any line no longer looks like a field, a continuation, a separator, or a blank line. Reordering or deleting whole documents is safe and produces no warnings.

## Key bindings

- `C-c C-f`: Set a field across every document, with completion over the fields in the buffer. A name outside them, confirmed with a second `RET`, adds a new field.
- `C-c C-q`: Normalize typography that sneaks in from MusicBrainz and copy-paste: dash and minus lookalikes, the horizontal ellipsis, no-break spaces, invisible characters, straight apostrophes between letters, Roman numeral codepoints, and more. The text is first composed to Unicode normalization form C, so that visually identical but byte-different accents cannot break your searches. The rules live in the option `beets-edit-normalize-rules`. Characters that may be intentional (curly quotation marks, letter apostrophes such as the Hawaiian okina, CJK punctuation) are left alone.
- `C-c C-t`: Reveal the raw id lines.
- `C-c C-v`: Toggle the compact view described below.
- `M-n`/`M-p`: Move to the next or previous document, staying on the same field, so that repeated fields can be compared track by track.
- `M-up`/`M-down`: Move a document through the tracklist. The disc and track numbers are exchanged along the way, so pulling a track down really renumbers it.

Documents are also pages, so `C-x ]`, `C-x [`, and `C-x n p` work. They appear in Imenu as well, indexed by track number and title, or by album in album-level buffers.

## Compact view

`C-c C-v` turns the buffer into a table in the style of Org's column view. Fields identical across all documents move to a caption at the very top, and each document renders as a single row of the fields that differ, under labeled columns, with its id dimmed at the end of the row:

```
artist: Van der Graaf Generator    album: Godbluff
track  title
1      The Undercover Man  id -1
2      Scorched Earth      id -2
3      Arrow               id -3
4      The Sleepwalkers    id -4
```

The buffer text underneath is untouched and the view is read-only. Editing goes through commands:

| Key                        | Action                                              |
|----------------------------|-----------------------------------------------------|
| `n` / `p`                  | Move to the next/previous row, keeping the column   |
| `f` / `b`, `TAB` / `S-TAB` | Move to the next/previous cell, flowing across rows |
| `e`, `RET`                 | Edit the cell at point in a dedicated buffer        |
| `C-u e`, `C-u RET`         | Ask which field to edit instead                     |
| `E`                        | Set one field across every document                 |
| `M-up` / `M-down`          | Move the row through the tracklist                  |
| `q`                        | Return to the raw buffer                            |

The standard motions do the table-appropriate thing: `C-n`/`C-p` move by row, `C-f`/`C-b` by cell, `C-a`/`C-e` to the row's home and last cell, `M-<`/`M->` to the first and last row, `C-x ]`/`C-x [` treat rows as pages, and `C-v`/`M-v` scroll keeping the column. The raw view's `M-n`/`M-p` move by row as well. The home cell is the first content column, usually the title, skipping the disc and track numbers; the cursor also lands there when it has nowhere better.

The value editor is a real buffer, so minor modes such as `electric-quote-local-mode` apply in full. `C-c C-c` applies the value and `C-c C-k` cancels. Editing a caption field for one document changes only that document, after which the field is no longer shared and moves into the rows. The field prompts accept new names here as well.

Isearch works on the view's data: the rows render values that live in the hidden text, and searching unfolds a matched document temporarily beneath its row, showing its raw source, like searching folded Org headings. Exiting refolds it and leaves point on the matched field's cell.

Columns follow the beets edit plugin's canonical field order, which you may change via `beets-edit-compact-field-order`. Cells wider than `beets-edit-compact-max-cell-width` truncate, and expand in place while point is inside them. Fields listed in `beets-edit-compact-never-shared-fields` (by default the track and disc numbers, the title, the year, and the genres, whose uniformity across documents is often coincidence rather than context) keep their column even when identical everywhere.

## Installation

The mode requires Emacs 29.1 or later. Install it from the repository; with Emacs 30's `use-package`:

```elisp
(use-package beets-edit-mode
  :vc (:url "https://github.com/emreyolcu/beets-edit-mode")
  :init
  ;; The plugin's buffers have random temporary file names,
  ;; so nothing associates them with the mode; detect them by content.
  (add-to-list 'magic-mode-alist '(beets-edit-buffer-p . beets-edit-mode))
  :hook
  ;; Both are optional.
  ((beets-edit-mode . beets-edit-compact-mode)      ; start in the compact view
   (beets-edit-mode . electric-quote-local-mode)))  ; curly quotes as you type
```

On Emacs 29, install once with `M-x package-vc-install RET https://github.com/emreyolcu/beets-edit-mode RET` and keep the rest of the block. Elpaca and straight.el take the same repository in their usual recipe form.

With the `electric-quote-local-mode` hook above, a typed `'` becomes `’`, which a plain `'` in a search will not match. Isearch's character folding covers this: `M-s '` lets `'` match `’`, and setting `search-default-mode` to `char-fold-to-regexp` makes that the default.
