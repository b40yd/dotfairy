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
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil
      inhibit-automatic-native-compilation nil)

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

;; To speedup the Emacs windows, reducing the count on searching `load-path'
;; is significant, there're ways to do that:
;;  1. (setq load-suffixes '(".elc" ".el")) ;; to avoid searching .so/.dll
;;  2. (setq load-file-rep-suffixes '(""))  ;; to avoid searching *.gz
;;  3. Use the load-hints in https://mail.gnu.org/archive/html/bug-gnu-emacs/2024-10/msg00905.html
;;  4. Combin the packages into one directory.
;; This code slice can make the package.el to install packages in the package-user-dir/"all" to speedup Emacs:
(when (eq system-type 'windows-nt)
  (setq load-suffixes '(".elc" ".el"))
  (setq load-file-rep-suffixes '(""))

  (defun package-user-all-dir ()
    (file-name-concat package-user-dir "all"))

  (define-advice package-unpack (:around (ofun pkg-desc) ADV)
    (let ((pkg-dir (funcall ofun pkg-desc)))
      (when-let* (pkg-dir
                  (pkg-file (format "%s-pkg.el" (package-desc-name pkg-desc)))
                  (files (seq-difference (directory-files pkg-dir)
                                         '("." "..") 'string=))
                  (target-dir (file-name-as-directory (package-user-all-dir))))
        (make-directory target-dir t)
        (unless (seq-intersection files (directory-files target-dir) #'string=)
          (with-current-buffer (find-file-noselect (file-name-concat pkg-dir pkg-file))
            (goto-char (point-max))
            (when (re-search-backward ")" nil t)
              (newline-and-indent)
              (insert (format ":files '%S" (remove pkg-file files)))
              (save-buffer))
            (kill-buffer))
          (dolist (file files)
            (rename-file (expand-file-name file pkg-dir) target-dir))
          (delete-directory pkg-dir)
          (setf (package-desc-dir (car (alist-get (package-desc-name pkg-desc)
                                                  package-alist)))
                target-dir)
          (setq pkg-dir target-dir)))
      pkg-dir))

  (define-advice package-load-all-descriptors (:after () ADV)
    (when-let* ((pkg-dir (package-user-all-dir))
                ((file-directory-p pkg-dir)))
      (dolist (pkg-file (directory-files pkg-dir t ".*-pkg.el"))
        (cl-letf (((symbol-function 'package--description-file)
                   (lambda (_) pkg-file)))
          (package-load-descriptor pkg-dir)))))

  (define-advice package-delete (:around (ofun pkg-desc &optional force nosave) ADV)
    (if (string-prefix-p (package-user-all-dir)
                         (package-desc-dir pkg-desc))
        (cl-letf* ((default-directory (package-user-all-dir))
                   (pkg-file (format "%s-pkg.el" (package-desc-name pkg-desc)))
                   ((symbol-function 'package--delete-directory)
                    (lambda (x)
                      (with-current-buffer (find-file-noselect pkg-file)
                        (goto-char (point-min))
                        (when (re-search-forward ":files '\\((.*)\\)" nil t)
                          (dolist (file (read (match-string 1)))
                            (delete-file file))))
                      (delete-file pkg-file))))
          (funcall ofun pkg-desc force nosave))
      (funcall ofun pkg-desc force nosave))))

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
