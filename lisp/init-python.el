;;; init-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 b40yd

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

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)


;;; Code:

(use-package poetry
  :defer t
  :ensure t
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode)
  (map! :after python
        :localleader
        :map python-mode-map
        :desc "poetry" "p" #'poetry))

(use-package pyvenv
  :after python
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv))

(use-package python
  :after poetry
  :ensure t
  :hook ((inferior-python-mode . (lambda ()
                                   (process-query-on-exit-flag
                                    (get-process "Python")))))
  :init
  (set-ligatures! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")

  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems link the unversioned one to Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)

  (use-package python-black
    :demand t
    :after python
    :hook (python-mode . python-black-on-save-mode))

  (use-package pyimport
    :defer t
    :init
    (defun +python/optimize-imports ()
      "organize imports"
      (interactive)
      (pyimport-remove-unused)
      (py-isort-buffer))
    (map! :after python
          :map python-mode-map
          :localleader
          (:prefix ("i" . "imports")
           :desc "Insert missing imports" "i" #'pyimport-insert-missing
           :desc "Remove unused imports"  "R" #'pyimport-remove-unused
           :desc "Optimize imports"       "o" #'+python/optimize-imports)))

  (use-package py-isort
    :defer t
    :init
    (map! :after python
          :map python-mode-map
          :localleader
          (:prefix ("i" . "imports")
           :desc "Sort imports"      "s" #'py-isort-buffer
           :desc "Sort region"       "r" #'py-isort-region)))

  (use-package python-pytest
    :custom
    (python-pytest-confirm t)
    :init
    (map! :after python
          :localleader
          :map python-mode-map
          :prefix ("t" . "test")
          "a" #'python-pytest
          "f" #'python-pytest-file-dwim
          "F" #'python-pytest-file
          "t" #'python-pytest-function-dwim
          "T" #'python-pytest-function
          "r" #'python-pytest-repeat
          "p" #'python-pytest-dispatch)))

(use-package cython-mode
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  (map! :map cython-mode-map
        :localleader
        :prefix "c"
        :desc "Cython compile buffer"    "c" #'cython-compile))


(provide 'init-python)
;;; init-python.el ends here
