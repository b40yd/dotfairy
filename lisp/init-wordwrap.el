;;; init-wordwrap.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

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
(require 'cl-lib)
(require 'dash)
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package adaptive-wrap)
(use-package dtrt-indent)
(use-package smartparens)

(defvar +word-wrap--major-mode-is-visual nil)
(defvar +word-wrap--major-mode-is-text nil)
(defvar +word-wrap--enable-adaptive-wrap-mode nil)
(defvar +word-wrap--enable-visual-line-mode nil)
(defvar +word-wrap--major-mode-indent-var nil)

(defvar adaptive-wrap-extra-indent)
(defun +word-wrap--adjust-extra-indent-a (fn beg end)
  "Contextually adjust extra word-wrap indentation."
  (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg)))
    (funcall fn beg end)))

(defun +word-wrap--calc-extra-indent (p)
  "Calculate extra word-wrap indentation at point."
  (if (not (or +word-wrap--major-mode-is-text
               (sp-point-in-string-or-comment p)))
      (pcase +word-wrap-extra-indent
        ('double
         (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
        ('single
         (symbol-value +word-wrap--major-mode-indent-var))
        ((and (pred integerp) fixed)
         fixed)
        (_ 0))
    0))

;;;###autoload
(define-minor-mode +word-wrap-mode
  "Wrap long lines in the buffer with language-aware indentation.
This mode configures `adaptive-wrap' and `visual-line-mode' to wrap long lines
without modifying the buffer content. This is useful when dealing with legacy
code which contains gratuitously long lines, or running emacs on your
wrist-phone.
Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+word-wrap-extra-indent'."
  :init-value nil
  (if +word-wrap-mode
      (progn
        (setq-local +word-wrap--major-mode-is-visual
                    (memq major-mode +word-wrap-visual-modes))
        (setq-local +word-wrap--major-mode-is-text
                    (memq major-mode +word-wrap-text-modes))

        (setq-local +word-wrap--enable-adaptive-wrap-mode
                    (and (not (bound-and-true-p adaptive-wrap-prefix-mode))
                         (not +word-wrap--major-mode-is-visual)))

        (setq-local +word-wrap--enable-visual-line-mode
                    (not (bound-and-true-p visual-line-mode)))

        (unless +word-wrap--major-mode-is-visual
          (require 'dtrt-indent) ; for dtrt-indent--search-hook-mapping
          (require 'smartparens) ; for sp-point-in-string-or-comment

          (setq-local +word-wrap--major-mode-indent-var
                      (caddr (dtrt-indent--search-hook-mapping major-mode)))

          (advice-add #'adaptive-wrap-fill-context-prefix :around #'+word-wrap--adjust-extra-indent-a))

        (when +word-wrap--enable-adaptive-wrap-mode
          (adaptive-wrap-prefix-mode +1))
        (when +word-wrap--enable-visual-line-mode
          (visual-line-mode +1)))

    ;; disable +word-wrap-mode
    (unless +word-wrap--major-mode-is-visual
      (advice-remove #'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a))

    (when +word-wrap--enable-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (when +word-wrap--enable-visual-line-mode
      (visual-line-mode -1))))

(defun +word-wrap--enable-global-mode ()
  "Enable `+word-wrap-mode' for `+word-wrap-global-mode'.
Wrapping will be automatically enabled in all modes except special modes, or
modes explicitly listed in `+word-wrap-disabled-modes'."
  (unless (or (eq (get major-mode 'mode-class) 'special)
              (memq major-mode +word-wrap-disabled-modes))
    (+word-wrap-mode +1)))

;;;###autoload
(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode
  +word-wrap--enable-global-mode)

(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.
When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.
Otherwise no extra indentation will be used.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
`+word-wrap-mode'.")

(defvar +word-wrap-visual-modes
  '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use
`adaptive-wrap-prefix-mode'.")

(defvar +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode
              latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode))

(provide 'init-wordwrap)
;;; init-wordwrap.el ends here
