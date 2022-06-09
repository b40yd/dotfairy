;;; config.el ---                                    -*- lexical-binding: t; -*-

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
(require 'init-const)
(require 'init-custom)

(setq dotfairy-full-name "user name")           ; User full name
(setq dotfairy-mail-address "user@email.com")   ; Email address
;; (setq dotfairy-proxy "127.0.0.1:1080")          ; Network proxy
(setq dotfairy-quelpa-upgrade nil) ; Enable `quelpa-upgrade-p' t or nil
;; (setq dotfairy-completion-style 'childframe) ; Completion display style default `childframe', or set `minibuffer'.
;; (setq dotfairy-server nil)                      ; Enable `server-mode' or not: t or nil
(setq dotfairy-package-archives 'netease)   ; Package repo: melpa, emacs-china, netease, bfsu, ustc or tuna
;; Color theme:
;; dotfairy-theme-list
;; '((default . doom-one)
;;   (doom-one . doom-one)
;;   (doom-monokai-pro     . doom-monokai-pro)
;;   (doom-dark+    . doom-dark+)
;;   (doom-one-light   . doom-one-light)
;;   (doom-solarized-light    . doom-solarized-light)
;;   (doom-city-lights    . doom-city-lights)
;;   (doom-tomorrow-day    . doom-tomorrow-day)
;;   (doom-tomorrow-night   . doom-tomorrow-night))
(setq dotfairy-theme 'default)
(setq dotfairy-complete 'vertico) ;; Vertico or Ivy achieves full compatibility with built-in completion commands
(setq dotfairy-lsp 'lsp-bridge)   ;; Use lsp-mode, eglot or lsp-bridge code complete
;; (setq dotfairy-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode go-mode)) ; Ignore format on save for some languages
;; (setq dotfairy-company-prescient nil) ; Enable `company-prescient' or not. it's on Windows 10 very slow.
;; confirm exit emacs
(setq confirm-kill-emacs 'y-or-n-p)
(setq ssh-manager-sessions '()) ;Add SSH connect sessions

;; Fonts
(defun dotfairy-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                           "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :font font
                                        :height (cond (IS-MAC 180)
                                                      (IS-WINDOWS 110)
                                                      (t 130))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("PowerlineSymbols" "Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'emoji `(,font . "iso10646-1") nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Zen Hei Mono" "WenQuanYi Micro Hei" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff) font))))
(dotfairy-setup-fonts)
(add-hook 'server-after-make-frame-hook #'dotfairy-setup-fonts)

;; default workspace
(setq default-directory "~/")
;; .authinfo
;; machine git.scanbuf.net/api/v4 login <your_git_user>^forge password <your_git_auth_token>
;;
(with-eval-after-load 'forge
  ;; (push '("git.scanbuf.net" "git.scanbuf.net/api/v4" "git.scanbuf.net" forge-gitlab-repository) forge-alist)
  ;; (push '("git.scanbuf.net" "git.scanbuf.net/api/v4" "git.scanbuf.net" forge-github-repository) forge-alist)
  )

;; setting proxy
;; (dotfairy/proxy-http-toggle)
;; (dotfairy/proxy-socks-toggle)

;; (byte-recompile-directory package-user-dir 0 0) ;
;;; config.el ends here
