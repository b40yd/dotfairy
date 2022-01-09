;;; init-indent-yank.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2022, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
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

;; @see https://emacs-china.org/t/indent-yank/19524
(require 'dash)
(require 'init-funcs)

(defun indent-yank-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "\\_>*$")))

(defun indent-yank-remove-indent-in-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((indent most-positive-fixnum))
      (while (not (eobp))
        (when (not (indent-yank-line-empty-p))
          (setq indent (min indent (current-indentation))))
        (forward-line 1))
      (goto-char (point-min))
      (while (not (eobp))
        (when (>= (current-indentation) indent)
          (beginning-of-line)
          (delete-forward-char indent))
        (forward-line 1)))
    (buffer-string)))

(defun indent-yank-yank (&optional arg)
  (interactive "*P")
  (let* ((arg (or arg 1))
         (indent (current-indentation))
         (text-without-indent (indent-yank-remove-indent-in-string (current-kill 0)))
         (text-yank (replace-regexp-in-string
                     "\n" (concat "\n" (-repeat indent ? )) text-without-indent)))
    (let ((head (nth (- arg 1) kill-ring)))
      (setf (nth (- arg 1) kill-ring) text-yank)
      (yank arg)
      (setf (car kill-ring) head))))

(defun indent-yank-before-insert-for-yank (args)
  (if indent-yank-mode (list (replace-regexp-in-string "\n"
                                                       (concat "\n"
                                                               (-repeat (current-indentation) ? ))
                                                       (indent-yank-remove-indent-in-string (car args))))
    args))

(advice-add #'insert-for-yank :filter-args #'indent-yank-before-insert-for-yank)

(define-minor-mode indent-yank-mode
  "Minor mode for yanking based on indentation at point.")

(add-hook!  '(org-mode-hook python-mode-hook) 'indent-yank-mode)

(provide 'init-indent-yank)
;;; init-indent-yank.el ends here
