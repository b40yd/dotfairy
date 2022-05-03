;;; init-session.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(defvar desktop-base-file-name)
(defvar desktop-dirname)
(defvar desktop-restore-eager)
(defvar desktop-file-modtime)

(use-package restart-emacs)

;;
;;; Helpers

;;;###autoload
(defun dotfairy-session-file (&optional name)
  "TODO"
  (cond ((require 'persp-mode nil t)
         (expand-file-name (or name persp-auto-save-fname) persp-save-dir))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))


;;;###autoload
(defun dotfairy-save-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (dotfairy-session-file))))
  (cond ((require 'persp-mode nil t)
         (unless persp-mode (persp-mode +1))
         (setq persp-auto-save-opt 0)
         (persp-save-state-to-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

;;;###autoload
(defun dotfairy-load-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (dotfairy-session-file))))
  (message "Attempting to load %s" file)
  (cond ((not (file-readable-p file))
         (message "No session file at %S to read from" file))
        ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (let ((allowed (persp-list-persp-names-in-file file)))
           (cl-loop for name being the hash-keys of *persp-hash*
                    unless (member name allowed)
                    do (persp-kill name))
           (persp-load-state-from-file file)))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Command line switch

;;;###autoload
(defun dotfairy-restore-session-handler (&rest _)
  "TODO"
  (add-hook 'window-setup-hook #'dotfairy-load-session 'append))

;;;###autoload
(add-to-list 'command-switch-alist (cons "--restore" #'dotfairy-restore-session-handler))


;;
;;; Commands

;;;###autoload
(defun dotfairy/quickload-session ()
  "TODO"
  (interactive)
  (message "Restoring session...")
  (dotfairy-load-session)
  (message "Session restored. Welcome back."))

;;;###autoload
(defun dotfairy/quicksave-session ()
  "TODO"
  (interactive)
  (message "Saving session")
  (dotfairy-save-session)
  (message "Saving session...DONE"))

;;;###autoload
(defun dotfairy/load-session (file)
  "TODO"
  (interactive
   (let ((session-file (dotfairy-session-file)))
     (list (or (read-file-name "Session to restore: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file)
                               t)
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (dotfairy-load-session file)
  (message "Session restored. Welcome back."))

;;;###autoload
(defun dotfairy/save-session (file)
  "TODO"
  (interactive
   (let ((session-file (dotfairy-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (dotfairy-save-session file))

;;;###autoload
(defalias 'dotfairy/restart #'restart-emacs)

;;;###autoload
(defun dotfairy/restart-and-restore (&optional debug)
  "Restart Emacs (and the daemon, if active).
If DEBUG (the prefix arg) is given, start the new instance with the --debug
switch."
  (interactive "P")
  (dotfairy/quicksave-session)
  (save-some-buffers nil t)
  (letf! ((#'save-buffers-kill-emacs #'kill-emacs)
          (confirm-kill-emacs))
    (restart-emacs
     (append (if debug (list "--debug-init"))
             (when (boundp 'chemacs-current-emacs-profile)
               (list "--with-profile" chemacs-current-emacs-profile))
             (list "--restore")))))

(defun dotfairy/restore-previous-session ()
  "Restore the previous session."
  (interactive)
  (when (bound-and-true-p persp-mode)
    (restore-session persp-auto-save-fname)))

(defun dotfairy/restore-session (fname)
  "Restore the specified session."
  (interactive (list (read-file-name "Load perspectives from a file: "
                                     persp-save-dir)))
  (when (bound-and-true-p persp-mode)
    (message "Restoring session...")
    (quit-window t)
    (condition-case-unless-debug err
        (persp-load-state-from-file fname)
      (error "Error: Unable to restore session -- %s" err))
    (message "Done")))
(provide 'init-session)
;;; init-session.el ends here
