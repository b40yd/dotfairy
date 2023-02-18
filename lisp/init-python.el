;;; init-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  7ym0n.q6e

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

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'init-keybinds)

;;; Code:
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
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))

  (use-package python-black
    :demand t
    :after python
    :hook (python-mode . python-black-on-save-mode))

  (use-package pyimport
    :defer t)

  (use-package py-isort
    :defer t)

  (use-package python-pytest
    :custom
    (python-pytest-confirm t))

  (defun +python/optimize-imports ()
    "organize imports"
    (interactive)
    (pyimport-remove-unused)
    (py-isort-buffer))

  (map! :localleader
        :after python
        :map python-mode-map
        (:prefix ("i" . "imports")
         :desc "Insert missing imports" "i" #'pyimport-insert-missing
         :desc "Remove unused imports"  "r" #'pyimport-remove-unused
         :desc "Optimize imports"       "o" #'+python/optimize-imports)
        (:prefix ("i" . "imports")
         :desc "Sort imports"      "s" #'py-isort-buffer
         :desc "Sort region"       "r" #'py-isort-region)
        (:prefix ("t" . "test")
         "f" #'python-pytest-file-dwim
         "F" #'python-pytest-file
         "s" #'python-pytest-function-dwim
         "S" #'python-pytest-function
         "r" #'python-pytest-repeat
         "p" #'python-pytest-dispatch)))

(use-package pyvenv
  :after python
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))


(use-package pyenv-mode
  :after python
  :autoload (+python-pyenv-mode-set-auto-h +python-pyenv-read-version-from-file)
  :hook ((pyenv-mode . +python-pyenv-mode-set-auto-h)
         (python-mode . (lambda ()
                          (when (executable-find "pyenv")
                            (pyenv-mode +1)
                            (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv")))))))
  :config
  ;;;###autoload
  (defvar +pyenv--version nil)

  (defun +python-pyenv-set-venv (version)
    "Set venv path and version."
    (when (featurep 'lsp-mode)
      (setq lsp-pyright-venv-path (expand-file-name (concat "versions/" version)
                                                    (or (getenv "PYENV_ROOT") "~/.pyenv"))))
    (setq python-shell-interpreter version)
    (pyenv-mode-set +pyenv--version))

;;;###autoload
  (defun +python-pyenv-mode-set-auto-h ()
    "Set pyenv-mode version from buffer-local variable."
    (when (eq major-mode 'python-mode)
      (when (not (local-variable-p '+pyenv--version))
        (make-local-variable '+pyenv--version)
        (setq +pyenv--version (+python-pyenv-read-version-from-file)))
      (if +pyenv--version
          (+python-pyenv-set-venv +pyenv--version)
        (pyenv-mode-unset))))

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
                   version file-path))))))


(use-package conda
  :after python
  :commands (+python/set-conda-home)
  :config
  ;;;###autoload
  (defun +python/set-conda-home ()
    "Set `conda-anaconda-home' (ANACONDA_HOME).
Usually it's `~/.anaconda3' on local machine, but it can be set to a remote
directory using TRAMP syntax, e.g. `/ssh:host:/usr/bin/anaconda3'. This way, you
can use a remote conda environment, including the corresponding remote python
executable and packages."
    (interactive)
    (require 'conda)
    (when-let (home (read-directory-name "Set conda home: " "~" nil nil conda-anaconda-home))
      (setq conda-anaconda-home home)
      (message "Successfully changed conda home to: %s" (abbreviate-file-name home))))

  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/.miniforge3"
                                "~/anaconda3"
                                "~/miniconda3"
                                "~/miniforge3"
                                "~/opt/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base"
                                "~/.conda")
               if (file-directory-p dir)
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))

(use-package poetry
  :ensure t
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(provide 'init-python)
;;; init-python.el ends here
