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

(defcustom dotfairy-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'dotfairy
  :type 'boolean)

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

(defun my/autoinsert-yas-expand()
      "Replace text in yasnippet template."
      (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(custom-set-variables
 '(auto-insert 'other)
 '(auto-insert-query nil)
 '(auto-insert-directory "~/.emacs.d/private/templates/")
 '(auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
                       (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])
                       (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                       (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
                       (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
                       (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand]))))

;; (byte-recompile-directory package-user-dir 0 0) ;
;;; config.el ends here
