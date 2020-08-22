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

;; Personal information
(setq user-full-name "7ym0n.q6e"
      user-mail-address "bb.qnyd@gmail.com")

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("org" . "http://mirrors.cloud.tencent.com/elpa/org/")))

 ;; Display 'lambda' as 'λ' (just for fun)
(global-prettify-symbols-mode 1)
(set-default 'cursor-type 'bar)
(setq default-frame-alist '((width . 140) (height . 40)))
;; (setq default-frame-alist '((width . 180) (height . 40)))
;; (set-frame-parameter nil 'fullscreen 'maximized)
;; Set fonts global
(set-face-attribute
 'default nil
 :font (font-spec :name "Ubuntu Mono"
                  :weight 'normal
                  :slant 'normal
                  :size 12.5))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name "文泉驿等宽微米黑"
              :weight 'normal
              :slant 'normal
              :size 11.0)))

;; (byte-recompile-directory package-user-dir 0 0) ;
;;; config.el ends here
