;;; init-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  7ym0n.q6e

;; Author: 7ym0n.q6e <bb.qnyd@gmail.com>
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

(use-package python
  :ensure t
  :hook ((inferior-python-mode . (lambda ()
                                   (process-query-on-exit-flag
                                    (get-process "Python")))))
  :init
  (setq python-indent-offset 4)
  :config
  ;; (when (executable-find "jupyter")
  ;;   (setq lsp-python-executable-cmd "jupyter"))

  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  ;; ;; ;; Env vars
  (when (fboundp 'exec-path-from-shell-copy-env)
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (use-package py-autopep8
    :ensure t
    :hook ((python-mode . py-autopep8-enable-on-save))
    )
  (use-package py-isort
    :ensure t
    :config
    (add-hook 'before-save-hook #'py-isort-before-save))

  (use-package pyimport
    :bind (("C-c e i" . pyimport-insert-missing)
           ("C-c e r" . pyimport-remove-unused)))
  (use-package python-pytest
    :ensure t
    :bind (("C-c t f" . python-pytest-file-dwim)
           ("C-c t F" . python-pytest-file)
           ("C-c t t" . python-pytest-function-dwim)
           ("C-c t T" . python-pytest-function)
           ("C-c t r" . python-pytest-repeat)
           ("C-c t p" . python-pytest-popup)))
  ;; Python: pyright
  (use-package lsp-pyright
    :preface
    ;; Use yapf to format
    (defun lsp-pyright-format-buffer ()
      (interactive)
      (when (and (executable-find "yapf") buffer-file-name)
        (call-process "yapf" nil nil nil "-i" buffer-file-name)))
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
    :init (when (executable-find "python3")
            (setq lsp-pyright-python-executable-cmd "python3")))
  )

(provide 'init-python)
;;; init-python.el ends here
