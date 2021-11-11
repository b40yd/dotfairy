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

(setq dotfairy-full-name "user name")           ; User full name
(setq dotfairy-mail-address "user@email.com")   ; Email address
;; (setq dotfairy-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq dotfairy-server nil)                      ; Enable `server-mode' or not: t or nil
(setq dotfairy-package-archives 'netease)   ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
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
;; (setq dotfairy-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq dotfairy-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode go-mode)) ; Ignore format on save for some languages
;; (setq dotfairy-company-prescient nil) ; Enable `company-prescient' or not. it's on Windows 10 very slow.
;; confirm exit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Fonts
(defun dotfairy-set-fonts ()
  ;; Set default font
  (cl-loop for font in '("Source Code Pro" "SF Mono" "Hack" "Fira Code"
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
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Zen Hei Mono" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (dotfairy-set-fonts)))
  (when (display-graphic-p)
    (dotfairy-set-fonts)))

;; default workspace
(setq default-directory "~/")
;; .authinfo
;; machine git.scanbuf.net/api/v4 login <your_git_user>^forge password <your_git_auth_token>
;;
(with-eval-after-load 'forge
  ;; (push '("git.scanbuf.net" "git.scanbuf.net/api/v4" "git.scanbuf.net" forge-gitlab-repository) forge-alist)
  ;; (push '("git.scanbuf.net" "git.scanbuf.net/api/v4" "git.scanbuf.net" forge-github-repository) forge-alist)
  )
;; (byte-recompile-directory package-user-dir 0 0) ;
;;; config.el ends here
