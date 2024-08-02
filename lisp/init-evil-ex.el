;;; init-evil-ex.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2024, b40yd, all rights reserved.

;; Author: b40yd <bb.qnyd@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

(defvar +evil-want-move-window-to-wrap-around nil
  "If non-nil, `+evil/window-move-*' commands will wrap around.")

;;;###autoload
(defun +evil/next-beginning-of-method (count)
  "Jump to the beginning of the COUNT-th method/function after point."
  (interactive "p")
  (beginning-of-defun (- count)))

;;;###autoload
(defun +evil/previous-beginning-of-method (count)
  "Jump to the beginning of the COUNT-th method/function before point."
  (interactive "p")
  (beginning-of-defun count))

;;;###autoload
(defalias #'+evil/next-end-of-method #'end-of-defun
  "Jump to the end of the COUNT-th method/function after point.")

;;;###autoload
(defun +evil/previous-end-of-method (count)
  "Jump to the end of the COUNT-th method/function before point."
  (interactive "p")
  (end-of-defun (- count)))

;;;###autoload
(defun +evil/next-preproc-directive (count)
  "Jump to the COUNT-th preprocessor directive after point.

By default, this only recognizes C preproc directives. To change this see
`+evil-preprocessor-regexp'."
  (interactive "p")
  ;; TODO More generalized search, to support directives in other languages?
  (if (re-search-forward +evil-preprocessor-regexp nil t count)
      (goto-char (match-beginning 0))
    (user-error "No preprocessor directives %s point"
                (if (> count 0) "after" "before"))))

;;;###autoload
(defun +evil/previous-preproc-directive (count)
  "Jump to the COUNT-th preprocessor directive before point.

See `+evil/next-preproc-directive' for details."
  (interactive "p")
  (+evil/next-preproc-directive (- count)))

;;; ] SPC / [ SPC
;;;###autoload
(defun +evil/insert-newline-below (count)
  "Insert COUNT blank line(s) below current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-below))))

;;;###autoload
(defun +evil/insert-newline-above (count)
  "Insert COUNT blank line(s) above current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-above))))

;;; ]t / [t
;;;###autoload
(defun +evil/next-frame (count)
  "Focus next frame."
  (interactive "p")
  (dotimes (_ (abs count))
    (let ((frame (if (> count 0) (next-frame) (previous-frame))))
      (if (eq frame (selected-frame))
          (user-error "No other frame")
        (select-frame-set-input-focus frame)))))

;;;###autoload
(defun +evil/previous-frame (count)
  "Focus previous frame."
  (interactive "p")
  (+evil/next-frame (- count)))

;;; ]f / [f
  ;;;###autoload
(defun dotfairy-glob (&rest segments)
  "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
  (declare (side-effect-free t))
  (let* (case-fold-search
         file-name-handler-alist
         (path (apply #'file-name-concat segments)))
    (if (string-suffix-p "/" path)
        (cl-delete-if-not #'file-directory-p (file-expand-wildcards (substring path 0 -1)))
      (file-expand-wildcards path))))

(defun +evil--next-file (n)
  (unless buffer-file-name
    (user-error "Must be called from a file-visiting buffer"))
  (let* ((directory (file-name-directory buffer-file-name))
         (filename (file-name-nondirectory buffer-file-name))
         (files (cl-remove-if-not #'file-regular-p (dotfairy-glob (file-name-directory buffer-file-name) "[!.]*")))
         (index (cl-position filename files :test #'file-equal-p)))
    (when (null index)
      (user-error "Couldn't find this file in current directory"))
    (let ((index (+ index n)))
      (cond ((>= index (length files))
             (user-error "No files after this one"))
            ((< index 0)
             (user-error "No files before this one"))
            ((expand-file-name (nth index files) directory))))))

;;;###autoload
(defun +evil/next-file (count)
  "Open file following this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file count)))

;;;###autoload
(defun +evil/previous-file (count)
  "Open file preceding this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file (- count))))


;;
;;; Encoding/Decoding

;; NOTE For ]x / [x see :lang web
;; - `+web:encode-html-entities'
;; - `+web:decode-html-entities'

(defun +evil--encode (beg end fn)
  (save-excursion
    (goto-char beg)
    (let* ((end (if (eq evil-this-type 'line) (1- end) end))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (funcall fn text)))))

;;; ]u / [u
(evil-define-operator +evil:url-encode (_count &optional beg end)
  "TODO"
  (interactive "<c><r>")
  (+evil--encode beg end #'url-encode-url))

(evil-define-operator +evil:url-decode (_count &optional beg end)
  "TODO"
  (interactive "<c><r>")
  (+evil--encode beg end #'url-unhex-string))

;;; ]y / [y
(evil-define-operator +evil:c-string-encode (_count &optional beg end)
  "TODO"
  (interactive "<c><r>")
  (+evil--encode
   beg end
   (lambda (text)
     (replace-regexp-in-string "[\"\\]" (lambda (ch) (concat "\\" ch)) text))))

(evil-define-operator +evil:c-string-decode (_count &optional beg end)
  "TODO"
  (interactive "<c><r>")
  (+evil--encode
   beg end
   (lambda (text)
     (replace-regexp-in-string "\\\\[\"\\]" (lambda (str) (substring str 1)) text))))

;;;###autoload
(defun +evil/shift-right ()
  "vnoremap < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/shift-left ()
  "vnoremap > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/alt-paste ()
  "Call `evil-paste-after' but invert `evil-kill-on-visual-paste'.
By default, this replaces the selection with what's in the clipboard without
replacing its contents."
  (interactive)
  (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
    (call-interactively #'evil-paste-after)))
;;; Standalone
;;; gp
;;;###autoload
(defun +evil/reselect-paste ()
  "Return to visual mode and reselect the last pasted region."
  (interactive)
  (cl-destructuring-bind (_ _ _ beg end &optional _)
      evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))
  ;;;###autoload
(defun +evil--window-swap (direction &optional invert-wrap?)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, uses evil-window-move-* (e.g. `evil-window-move-far-left').
If already at the edge of the frame and `+evil-want-move-window-to-wrap-around'
is non-nil, move the window to the other end of the frame. Inverts
`+evil-want-move-window-to-wrap-around' if INVERT-WRAP? is non-nil."
  (unless (memq direction '(left right up down))
    (user-error "Invalid direction: %s" direction))
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (that-window (window-in-direction direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (if (and (window-at-side-p
                  this-window (pcase direction ('up 'top) ('down 'bottom) (_ direction)))
                 (not (cl-loop for dir in (if (memq direction '(left right))
                                              '(up down) '(left right))
                               if (window-in-direction dir nil this-window)
                               return t)))
            (if (funcall (if invert-wrap? #'not #'identity) +evil-want-move-window-to-wrap-around)
                (call-interactively
                 (pcase direction
                   ('left  #'evil-window-move-far-right)
                   ('right #'evil-window-move-far-left)
                   ('up    #'evil-window-move-very-bottom)
                   ('down  #'evil-window-move-very-top)))
              (user-error "Window is already at the edge"))
          (call-interactively
           (pcase direction
             ('left  #'evil-window-move-far-left)
             ('right #'evil-window-move-far-right)
             ('up    #'evil-window-move-very-top)
             ('down  #'evil-window-move-very-bottom))))
      (unless that-window
        (setq that-window (split-window this-window nil direction))
        (with-selected-window that-window
          (switch-to-buffer (dotfairy-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (window-swap-states this-window that-window)
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left (&optional arg)
  "Swap windows to the left."
  (interactive "P")
  (+evil--window-swap 'left (or arg +evil-want-move-window-to-wrap-around)))
;;;###autoload
(defun +evil/window-move-right (&optional arg)
  "Swap windows to the right"
  (interactive "P")
  (+evil--window-swap 'right (or arg +evil-want-move-window-to-wrap-around)))
;;;###autoload
(defun +evil/window-move-up (&optional arg)
  "Swap windows upward."
  (interactive "P")
  (+evil--window-swap 'up (or arg +evil-want-move-window-to-wrap-around)))
;;;###autoload
(defun +evil/window-move-down (&optional arg)
  "Swap windows downward."
  (interactive "P")
  (+evil--window-swap 'down (or arg +evil-want-move-window-to-wrap-around)))

;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
  If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
  If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(after! evil
  (evil-set-initial-state 'bongo-library-mode 'emacs))

(map!
 ;; ported from vim-unimpaired
 :n  "] SPC" #'+evil/insert-newline-below
 :n  "[ SPC" #'+evil/insert-newline-above
 :n  "]b"    #'next-buffer
 :n  "[b"    #'previous-buffer
 :n  "]f"    #'+evil/next-file
 :n  "[f"    #'+evil/previous-file
 :m  "]u"    #'+evil:url-encode
 :m  "[u"    #'+evil:url-decode
 :m  "]y"    #'+evil:c-string-encode
 :m  "[y"    #'+evil:c-string-decode
 ;; custom vim-unmpaired-esque keys
 :m  "]#"    #'+evil/next-preproc-directive
 :m  "[#"    #'+evil/previous-preproc-directive
 :m  "]e"    #'next-error
 :m  "[e"    #'previous-error
 :n  "]F"    #'+evil/next-frame
 :n  "[F"    #'+evil/previous-frame
 :m  "]h"    #'outline-next-visible-heading
 :m  "[h"    #'outline-previous-visible-heading
 :m  "]m"    #'+evil/next-beginning-of-method
 :m  "[m"    #'+evil/previous-beginning-of-method
 :m  "]M"    #'+evil/next-end-of-method
 :m  "[M"    #'+evil/previous-end-of-method
 :n  "[o"    #'+evil/insert-newline-above
 :n  "]o"    #'+evil/insert-newline-below
 :n  "gp"    #'+evil/reselect-paste
 :v  "gp"    #'+evil/alt-paste
 :nv "gO"    #'imenu
 :n  "g="    #'evil-numbers/inc-at-pt
 :n  "g-"    #'evil-numbers/dec-at-pt
 :v  "g="    #'evil-numbers/inc-at-pt-incremental
 :v  "g-"    #'evil-numbers/dec-at-pt-incremental
 :v  "g+"    #'evil-numbers/inc-at-pt
 ;; custom evil keybinds
 :n  "zx"    #'kill-current-buffer
 :n  "ZX"    #'dotfairy/save-and-kill-buffer
 ;; don't leave visual mode after shifting
 :v  "<"     #'+evil/shift-left  ; vnoremap < <gv
 :v  ">"     #'+evil/shift-right  ; vnoremap > >gv
 ;; window management (prefix "C-w")
 (:map evil-window-map
  ;; Navigation
  "C-h"     #'evil-window-left
  "C-j"     #'evil-window-down
  "C-k"     #'evil-window-up
  "C-l"     #'evil-window-right
  "C-w"     #'other-window
  ;; Extra split commands
  "S"       #'+evil/window-split-and-follow
  "V"       #'+evil/window-vsplit-and-follow
  ;; Swapping windows
  "H"       #'+evil/window-move-left
  "J"       #'+evil/window-move-down
  "K"       #'+evil/window-move-up
  "L"       #'+evil/window-move-right
  "C-S-w"   #'ace-swap-window)
 ;; Window undo/redo
 "C-u"     #'winner-undo
 "C-r"     #'winner-redo

 ;; evil-surround
 :v "S" #'evil-surround-region
 :o "s" #'evil-surround-edit
 :o "S" #'evil-Surround-edit
 )

(provide 'init-evil-ex)
;;; init-evil-ex.el ends here
