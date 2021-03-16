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
(setq dotfairy-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
;; (setq dotfairy-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq dotfairy-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode go-mode)) ; Ignore format on save for some languages

;; confirm exit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Source Code Pro" "SF Mono" "Hack" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (IS-MAC 130)
                                                    (IS-WINDOWS 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Zen Hei Mono" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

;; default workspace
(setq default-directory "~/")

;; (byte-recompile-directory package-user-dir 0 0) ;
;;; config.el ends here
