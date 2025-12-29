;;; early-init.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
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

;; Defer garbage collection further back in the startup process
(if noninteractive  ; in CLI sessions
    (setq gc-cons-threshold #x8000000   ; 128MB
          ;; Backport from 29 (see emacs-mirror/emacs@73a384a98698)
          gc-cons-percentage 1.0)
  (setq gc-cons-threshold most-positive-fixnum))

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil
      inhibit-automatic-native-compilation nil)


;; To speedup the Emacs windows, reducing the count on searching `load-path'
(when (eq system-type 'windows-nt)
  (setq load-suffixes '(".elc" ".el")) ;; to avoid searching .so/.dll
  (setq load-file-rep-suffixes '(""))) ;; to avoid searching *.gz

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)
;; enable all disabled commands
(setq disabled-command-function nil)

;; Set default coding system
(set-language-environment "UTF-8")

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

(setq-default mode-line-format nil)

;; For LSP performance
;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
