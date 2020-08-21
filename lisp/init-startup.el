;;; init-startup.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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

(require 'subr-x)

;; add core path
(add-to-list 'load-path (file-name-directory load-file-name))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

;;; Directories/files
(defconst dotfairy-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst dotfairy-lisp-dir (concat dotfairy-emacs-dir "lisp/")
  "The root directory of DotFairy's core files. Must end with a slash.")

(defconst dotfairy-local-dir
  (if-let (localdir (getenv "DOTFAIRYLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat dotfairy-emacs-dir ".local/"))
  "Root directory for local storage.
Use this as a storage location for this system's installation of DotFairy Emacs.
These files should not be shared across systems. By default, it is used by
`dotfairy-etc-dir' and `dotfairy-cache-dir'. Must end with a slash.")

(defconst dotfairy-etc-dir (concat dotfairy-local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst dotfairy-cache-dir (concat dotfairy-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defconst dotfairy-private-dir
  (if-let (dotfairydir (getenv "DOTFAIRYDOOMDIR"))
      (expand-file-name (file-name-as-directory dotfairydir))
    (or (let ((xdgdir
               (expand-file-name "dotfairy/"
                                 (or (getenv "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.dotfairy.d/"))
  "Where your private configuration is placed.
Defaults to ~/.config/dotfairy, ~/.dotfairy.d or the value of the DOOMDIR envvar;
whichever is found first. Must end in a slash.")

;;
;; Config file
(setq  package-user-dir                   (concat dotfairy-local-dir "elpa/")
       custom-file                        (concat dotfairy-private-dir "custom.el")
       dotfairy-config-file               (concat dotfairy-private-dir "config.el")
       dotfairy-example-config-file       (concat dotfairy-emacs-dir "config.el"))

(let* ((my-lisp-dir package-user-dir)
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(when (and (file-exists-p dotfairy-example-config-file)
           (not (file-exists-p dotfairy-config-file)))
  ;; At the first startup copy `config.el' from the example
  (copy-file dotfairy-example-config-file dotfairy-config-file))

;; Load config file
(if (and dotfairy-config-file
         (file-readable-p dotfairy-config-file))
    (load dotfairy-config-file))

;; Load Custon-fle
(if (and custom-file
         (file-readable-p custom-file))
    (load custom-file))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty

(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)
;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

(when (eq system-type 'windows-nt)
    (setq locale-coding-system 'gb18030)
    (setq w32-unicode-filenames 'nil)
    (setq file-name-coding-system 'gb18030))

;; UI
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Basic modes
(recentf-mode 1)

(ignore-errors (savehist-mode 1))
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; Keybindings
(global-set-key (kbd "C-.") #'imenu)
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(require 'init-package)

;; (defun dotfairy/initial-layers()
;;   (message "dotfairy/initial-layers ..."))
;; (add-hook 'emacs-startup-hook #'dotfairy/initial-layers) ;

;; Base
(require 'init-basic)
(require 'init-iedit)
(require 'init-hydra)
(require 'init-ui)
(require 'init-projectile)
(require 'init-ibuffer)
(require 'init-windows)

;;checking
(require 'init-flycheck)

;; Complete
(require 'init-company)
(require 'init-ivy)

;; Tools
(require 'init-lsp)
(require 'init-git)
(require 'init-eshell)
;;(require 'init-dired)
(require 'init-restclient)
(require 'init-docker)

;; Language
(require 'init-clang)
(require 'init-go)
(require 'init-python)
(require 'init-web)
(require 'init-markdown)
(require 'init-json)
(require 'init-org)

(provide 'init-startup)
;;; init-startup.el ends here
