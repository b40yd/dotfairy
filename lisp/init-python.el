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
                                                   ("sum" . ?∑)
                                                   ("prod" . ?∏)  ; numpy.prod; unpythonic.fold.prod  https://github.com/Technologicat/unpythonic
                                                   ("product" . ?∏)  ; pandas; also alternative name for prod in numpy
                                                   ("and" . ?∩)
                                                   ("or" . ?∪)
                                                   ("not in" . ?∉)  ; "not in" is only used for testing the absence of membership.
                                                   ("is" . ?≡)
                                                   ("is not" . ?≢)
                                                   ("all" . ?∀)
                                                   ("any" . ?∃)
                                                   ("None" . ?∅)))))
         )
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)

  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (use-package pyimport
    :defer t
    :init
    (map! :after python
          :map python-mode-map
          :localleader
          (:prefix ("i" . "imports")
           :desc "Insert missing imports" "i" #'pyimport-insert-missing
           :desc "Remove unused imports"  "r" #'pyimport-remove-unused
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
          (:prefix ("t" . "test")
           "f" #'python-pytest-file-dwim
           "F" #'python-pytest-file
           "s" #'python-pytest-function-dwim
           "S" #'python-pytest-function
           "r" #'python-pytest-repeat
           "p" #'python-pytest-dispatch
           )))
  )

(provide 'init-python)
;;; init-python.el ends here
