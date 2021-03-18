;;; init-clang.el ---                                -*- lexical-binding: t; -*-

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

;; C/C++ Mode
;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring))))

(use-package cc-mode
  :ensure t
  :bind (:map c-mode-base-map
              ("C-c C-c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))


;;
;; rtags enable jump-to-function definition
;; system need to install rtags first
;;
;; for centos, you need llvm-devel, cppunit-devl
;; install gcc-4.9, cmake 3.1 and download rtags from github and make it
;;

(use-package rtags
  :ensure t
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  )

;;
;; cmake-ide enable rdm(rtags) auto start and rc(rtags) to watch directory
;;
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  )


;;
;; for editting CMakeLists.txt
;;
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode-hook (lambda()
                               (add-to-list (make-local-variable 'company-backends)
                                            'company-cmake)))
  )

;;
;; for c formatting
;;
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "llvm")
  (add-hook 'c-mode-hook (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
  (add-hook 'c++-mode-hook (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
  )

(provide 'init-clang)
;;; init-clang.el ends here
