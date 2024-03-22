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
(require 'init-keybinds)

;;; Code:

(use-package poetry
  :defer t
  :ensure t
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package pyvenv
  :after python
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))


(use-package pyenv-mode
  :defer t
  :after python
  :config
;;;###autoload
  (defvar +pyenv--version nil)
;;;###autoload
  (defun +python-pyenv-read-version-from-file ()
    "Read pyenv version from .python-version file."
    (when-let (root-path (projectile-locate-dominating-file default-directory ".python-version"))
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (string-trim (buffer-string)))))
        (if (member version (pyenv-mode-versions))
            version  ;; return.
          (message "pyenv: version `%s' is not installed (set by `%s')."
                   version file-path)))))
;;;###autoload
  (defun +python-pyenv-mode-set-auto-h ()
    "Set pyenv-mode version from buffer-local variable."
    (when (eq major-mode 'python-mode)
      (when (not (local-variable-p '+pyenv--version))
        (make-local-variable '+pyenv--version)
        (setq +pyenv--version (+python-pyenv-read-version-from-file)))
      (if +pyenv--version
          (pyenv-mode-set +pyenv--version)
        (pyenv-mode-unset))))

  (when (executable-find "pyenv")
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (add-hook 'python-mode-local-vars-hook #'+python-pyenv-mode-set-auto-h))


(use-package python
  :ensure t
  :hook ((inferior-python-mode . (lambda ()
                                   (process-query-on-exit-flag
                                    (get-process "Python"))))
         (python-mode . (lambda ()
                          (dotfairy-set-prettify '(("**2" . ?²)
                                                   ("**3" . ?³)
                                                   ("**4" . ?⁴)
                                                   ("**5" . ?⁵)
                                                   ("**6" . ?⁶)
                                                   ("**7" . ?⁷)
                                                   ("**8" . ?⁸)
                                                   ("**9" . ?⁹)
                                                   ("**-1" . (?⁻ (Br . Bl) ?¹))  ; ⁻¹
                                                   ("**-2" . (?⁻ (Br . Bl) ?²))  ; ⁻²
                                                   ("**-3" . (?⁻ (Br . Bl) ?³))  ; ⁻³
                                                   ("**-4" . (?⁻ (Br . Bl) ?⁴))  ; ⁻⁴
                                                   ("**-5" . (?⁻ (Br . Bl) ?⁵))  ; ⁻⁵
                                                   ("**-6" . (?⁻ (Br . Bl) ?⁶))  ; ⁻⁶
                                                   ("**-7" . (?⁻ (Br . Bl) ?⁷))  ; ⁻⁷
                                                   ("**-8" . (?⁻ (Br . Bl) ?⁸))  ; ⁻⁸
                                                   ("**-9" . (?⁻ (Br . Bl) ?⁹))  ; ⁻⁹
                                                   ("def" . ?ƒ)
                                                   ("lambda" . ?λ)
                                                   ("===" . ?≡)
                                                   ("yield" . ?⟻)
                                                   ("str" . ?𝕊)
                                                   ("in" . ?∈)
                                                   ("not" . ?¬)
                                                   ("sum" . ?∑)
                                                   ("prod" . ?∏)  ; numpy.prod; unpythonic.fold.prod  https://github.com/Technologicat/unpythonic
                                                   ("product" . ?∏)  ; pandas; also alternative name for prod in numpy
                                                   ("and" . ?∩)
                                                   ("or" . ?∪)
                                                   ("not in" . ?∉)  ; "not in" is only used for testing the absence of membership.
                                                   ("is" . ?≡)
                                                   ("is not" . ?≢)
                                                   ("any" . ?∃)
                                                   ("False" . ?𝔽)
                                                   ("True" . ?𝕋)
                                                   ("None" . ?∅))))))
  :init
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
