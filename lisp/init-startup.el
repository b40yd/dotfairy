;;; init-startup.el ---                                   -*- lexical-binding: t; -*-

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

;;; Code:
(require 'init-package)

;; Base
(require 'init-basic)
(require 'init-keybinds)

(pcase dotfairy-keybind-mode
  ('evil
   (require 'init-evil)

   (defun +default-disable-delete-selection-mode-h ()
     (delete-selection-mode -1))

   (add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
   (add-hook 'evil-insert-state-exit-hook  #'+default-disable-delete-selection-mode-h)

   ;; Make SPC u SPC u [...]
   (map! :map universal-argument-map
         :prefix dotfairy-leader-key     "u" #'universal-argument-more
         :prefix dotfairy-leader-alt-key "u" #'universal-argument-more))
  ('emacs
   ;; Sensible deafult key bindings for non-evil users
   (setq dotfairy-leader-alt-key "C-c"
         dotfairy-localleader-alt-key "C-c SPC")))

(require 'init-hydra)
(require 'init-kill-ring)

;; UI
(require 'init-ui)

;; Editor
(require 'init-iedit)
(require 'init-indent-yank)
(require 'init-wordwrap)
(require 'init-markdown)

;; Project
(require 'init-ibuffer)
(require 'init-project)
(require 'init-windows)

;; Complete
(require 'init-yasnippet)
(require 'init-vertico)
(require 'init-completion)

(require 'init-template)

;; Tools
(require 'init-ligatures)
(require 'init-git)
(require 'init-make)
(require 'init-eshell)
(require 'init-dired)
(require 'init-restclient)
(require 'init-docker)
(require 'init-vterm)
(require 'init-ssh)
(require 'init-dict)
(require 'init-pass)
(require 'init-calendar)
(require 'init-player)

;; Language
(require 'init-elisp)
(require 'init-clang)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-web)
(require 'init-json)
(require 'init-org)
(require 'init-yaml)
(require 'init-asm)
(require 'init-sql)
(require 'init-lua)
(require 'init-prog)

;; Lsp Server
(require 'init-lsp)

;; Checking
(require 'init-flymake)

;; Debugger
(require 'init-debugger)

;; Highlight
(require 'init-highlight)


;; Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
  (defun dotfairy-init-leader-keys-h ()
    "Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal dotfairy-leader-alt-key "C-c")
                   (set-keymap-parent dotfairy-leader-map mode-specific-map))
                  ((equal dotfairy-leader-alt-key "C-x")
                   (set-keymap-parent dotfairy-leader-map ctl-x-map)))
            (define-key map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
        (evil-define-key* dotfairy-leader-key-states map (kbd dotfairy-leader-key) 'dotfairy/leader)
        (evil-define-key* dotfairy-leader-alt-key-states map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
      (general-override-mode +1))))

(provide 'init-startup)
;;; init-startup.el ends here
