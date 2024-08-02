;;; init-wordwrap.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2024 b40yd

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
(require 'cl-lib)
(require 'dash)
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package adaptive-wrap)
(use-package dtrt-indent)

(defvar +word-wrap--major-mode-is-visual nil)
(defvar +word-wrap--major-mode-is-text nil)
(defvar +word-wrap--enable-adaptive-wrap-mode nil)
(defvar +word-wrap--enable-visual-line-mode nil)
(defvar +word-wrap--enable-visual-fill-mode nil)
(defvar +word-wrap--disable-auto-fill-mode nil)
(defvar +word-wrap--major-mode-indent-var nil)

(defvar adaptive-wrap-extra-indent)

(use-package visual-fill-column)

(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.
When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.
Otherwise no extra indentation will be used.")

(defvar +word-wrap-fill-style nil
  "How to handle `fill-column' in `+word-wrap-mode'.
When 'auto, long lines will soft-wrap at `fill-column'. If `auto-fill-mode' is
enabled, its behaviour will not be affected.
When 'soft, long lines will soft-wrap at `fill-column' and `auto-fill-mode' will
be forcibly disabled.
Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected.")

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


(defun +word-wrap--adjust-extra-indent-a (fn beg end)
  "Contextually adjust extra word-wrap indentation."
  (let ((adaptive-wrap-extra-indent (+word-wrap--calc-extra-indent beg)))
    (funcall fn beg end)))


(cl-defstruct text-state
  ;; The last point checked by text--syntax-ppss and its result, used for
  ;; memoization
  last-syntax-ppss-point ;; a list (point point-min point-max)
  last-syntax-ppss-result
  )

(defvar-local text-state (make-text-state)
  "State for the current buffer.")
(defun text--reset-memoization (&rest ignored)
  "Reset memoization as a safety precaution.
IGNORED is a dummy argument used to eat up arguments passed from
the hook where this is executed."
  (setf (text-state-last-syntax-ppss-point text-state) nil
        (text-state-last-syntax-ppss-result text-state) nil))
(defun text--syntax-ppss (&optional p)
  "Memoize the last result of `syntax-ppss'.
P is the point at which we run `syntax-ppss'"
  (let ((p (or p (point)))
        (mem-p (text-state-last-syntax-ppss-point text-state)))
    (if (and (eq p (nth 0 mem-p))
             (eq (point-min) (nth 1 mem-p))
             (eq (point-max) (nth 2 mem-p)))
        (text-state-last-syntax-ppss-result text-state)
      ;; Add hook to reset memoization if necessary
      (unless (text-state-last-syntax-ppss-point text-state)
        (add-hook 'before-change-functions 'text--reset-memoization t t))
      (setf (text-state-last-syntax-ppss-point text-state)
            (list p (point-min) (point-max))
            (text-state-last-syntax-ppss-result text-state) (syntax-ppss p)))))

(defun text-point-in-string (&optional p)
  "Return non-nil if point is inside string or documentation string.
This function actually returns the 3rd element of `syntax-ppss'
which can be a number if the string is delimited by that
character or t if the string is delimited by general string
fences.
If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (text--syntax-ppss p)))))

(defun text-point-in-comment (&optional p)
  "Return non-nil if point is inside comment.
If optional argument P is present test this instead off point."
  (setq p (or p (point)))
  (ignore-errors
    (save-excursion
      ;; We cannot be in a comment if we are inside a string
      (unless (nth 3 (text--syntax-ppss p))
        (or (nth 4 (text--syntax-ppss p))
            ;; this also test opening and closing comment delimiters... we
            ;; need to chack that it is not newline, which is in "comment
            ;; ender" class in elisp-mode, but we just want it to be
            ;; treated as whitespace
            (and (< p (point-max))
                 (memq (char-syntax (char-after p)) '(?< ?>))
                 (not (eq (char-after p) ?\n)))
            ;; we also need to test the special syntax flag for comment
            ;; starters and enders, because `syntax-ppss' does not yet
            ;; know if we are inside a comment or not (e.g. / can be a
            ;; division or comment starter...).
            (-when-let (s (car (syntax-after p)))
              (or (and (/= 0 (logand (lsh 1 16) s))
                       (nth 4 (syntax-ppss (+ p 2))))
                  (and (/= 0 (logand (lsh 1 17) s))
                       (nth 4 (syntax-ppss (+ p 1))))
                  (and (/= 0 (logand (lsh 1 18) s))
                       (nth 4 (syntax-ppss (- p 1))))
                  (and (/= 0 (logand (lsh 1 19) s))
                       (nth 4 (syntax-ppss (- p 2)))))))))))

(defun text-point-in-string-or-comment (&optional p)
  "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
  (or (text-point-in-string p)
      (text-point-in-comment p)))

(defun +word-wrap--calc-extra-indent (p)
  "Calculate extra word-wrap indentation at point."
  (if (not (or +word-wrap--major-mode-is-text
               (text-point-in-string-or-comment p)))
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
This mode configures `adaptive-wrap', `visual-line-mode' and
`visual-fill-column-mode' to wrap long lines without modifying the buffer
content. This is useful when dealing with legacy code which contains
gratuitously long lines, or running emacs on your wrist-phone.
Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+word-wrap-extra-indent'.
Long lines will wrap at the window margin by default, or can optionally be
wrapped at `fill-column' by configuring `+word-wrap-fill-style'."
  :init-value nil
  (if +word-wrap-mode
      (progn
        (setq-local
         +word-wrap--major-mode-is-visual
         (memq major-mode +word-wrap-visual-modes)
         +word-wrap--major-mode-is-text
         (memq major-mode +word-wrap-text-modes)
         +word-wrap--enable-adaptive-wrap-mode
         (and (not (bound-and-true-p adaptive-wrap-prefix-mode))
              (not +word-wrap--major-mode-is-visual))
         +word-wrap--enable-visual-line-mode
         (not (bound-and-true-p visual-line-mode))
         +word-wrap--enable-visual-fill-mode
         (and (not (bound-and-true-p visual-fill-column-mode))
              (memq +word-wrap-fill-style '(auto soft)))
         +word-wrap--disable-auto-fill-mode
         (and (bound-and-true-p auto-fill-function)
              (eq +word-wrap-fill-style 'soft)))

        (unless +word-wrap--major-mode-is-visual
          (require 'dtrt-indent) ; for dtrt-indent--search-hook-mapping
          (setq-local +word-wrap--major-mode-indent-var
                      (let ((indent-var (caddr (dtrt-indent--search-hook-mapping major-mode))))
                        (if (listp indent-var)
                            (car indent-var)
                          indent-var)))

          (advice-add #'adaptive-wrap-fill-context-prefix :around #'+word-wrap--adjust-extra-indent-a))

        (when +word-wrap--enable-adaptive-wrap-mode
          (adaptive-wrap-prefix-mode +1))
        (when +word-wrap--enable-visual-line-mode
          (visual-line-mode +1))
        (when +word-wrap--enable-visual-fill-mode
          (visual-fill-column-mode +1))
        (when +word-wrap--disable-auto-fill-mode
          (auto-fill-mode -1)))

    ;; disable +word-wrap-mode
    (unless +word-wrap--major-mode-is-visual
      (advice-remove #'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a))

    (when +word-wrap--enable-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (when +word-wrap--enable-visual-line-mode
      (visual-line-mode -1))
    (when +word-wrap--enable-visual-fill-mode
      (visual-fill-column-mode -1))
    (when +word-wrap--disable-auto-fill-mode
      (auto-fill-mode +1))))

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

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode))

(provide 'init-wordwrap)
;;; init-wordwrap.el ends here
