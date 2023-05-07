;;; init-tags.el ---                                   -*- lexical-binding: t; -*-

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
(use-package citre
  :diminish
  :commands citre-jump-back
  :functions xref-go-back
  :bind (:map prog-mode-map
         ("C-x c j" . citre-jump+)
         ("C-x c k" . citre-jump-back+)
         ("C-x c p" . citre-peek)
         ("C-x c a" . citre-ace-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :init
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))

  (with-no-warnings
    (with-eval-after-load 'projectile
      (setq citre-project-root-function #'projectile-project-root))

    ;; Use Citre xref backend as a fallback
    (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
      (let ((fetcher (apply fn args))
            (citre-fetcher
             (let ((xref-backend-functions '(citre-xref-backend t)))
               (ignore xref-backend-functions)
               (apply fn args))))
        (lambda ()
          (or (with-demoted-errors "%s, fallback to citre"
                (funcall fetcher))
              (funcall citre-fetcher)))))

    ;; Combine completions from Citre and lsp
    (defun lsp-citre-capf-function ()
      "A capf backend that tries lsp first, then Citre."
      (let ((lsp-result (cond
                         ((bound-and-true-p lsp-mode)
                          (and (fboundp #'lsp-completion-at-point)
                               (lsp-completion-at-point)))
                         ((bound-and-true-p eglot--managed-mode)
                          (and (fboundp #'eglot-completion-at-point)
                               (eglot-completion-at-point))))))
        (if (and lsp-result
                 (try-completion
                  (buffer-substring (nth 0 lsp-result)
                                    (nth 1 lsp-result))
                  (nth 2 lsp-result)))
            lsp-result
          (citre-completion-at-point))))

    (defun enable-lsp-citre-capf-backend ()
      "Enable the lsp + Citre capf backend in current buffer."
      (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))
(provide 'init-tags)
;;; init-tags.el ends here
