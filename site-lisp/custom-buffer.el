;;; init-buffers.el ---                                   -*- lexical-binding: t; -*-

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

;;;###autoload
(defvar dotfairy-real-buffer-functions
  '(dotfairy-dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`dotfairy-unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.
Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.
See `dotfairy-real-buffer-p' for more information.")

;;;###autoload
(defvar dotfairy-unreal-buffer-functions
  '(minibufferp dotfairy-special-buffer-p dotfairy-non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `dotfairy-real-buffer-functions'. They are passed one argument: the buffer to
be tested.
Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.
See `dotfairy-real-buffer-p' for more information.")

;;;###autoload
(defvar-local dotfairy-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`dotfairy-real-buffer-p' for more information.")

;;;###autoload
(defvar dotfairy-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

;;
;;; Functions
;;;###autoload
(defun dotfairy-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (dotfairy-real-buffer-p buf)
      (eq buf (dotfairy-fallback-buffer))))

;;;###autoload
(defun dotfairy-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `dotfairy-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create dotfairy-fallback-buffer-name)))

;;;###autoload
(defalias 'dotfairy-buffer-list #'buffer-list)

;;;###autoload
(defun dotfairy-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.
If PROJECT is nil, default to the current project.
If no project is active, return all buffers."
  (let ((buffers (dotfairy-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (dotfairy-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun dotfairy-open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (dotfairy-buffer-list)
           if (buffer-live-p buffer)
           if (dotfairy-real-buffer-p buffer)
           if (with-current-buffer buffer (dotfairy-project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun dotfairy-dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun dotfairy-special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun dotfairy-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun dotfairy-visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

;;;###autoload
(defun dotfairy-buried-buffer-p (buf)
  "Return non-nil if BUF is not visible."
  (not (dotfairy-visible-buffer-p buf)))

;;;###autoload
(defun dotfairy-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun dotfairy-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `dotfairy-real-buffer-p'."
  (cl-remove-if-not #'dotfairy-real-buffer-p (or buffer-list (dotfairy-buffer-list))))

;;;###autoload
(defun dotfairy-real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.
A real buffer is a useful buffer; a first class citizen in Dotfairy. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.
The exact criteria for a real buffer is:
  1. A non-nil value for the buffer-local value of the `dotfairy-real-buffer-p'
     variable OR
  2. Any function in `dotfairy-real-buffer-functions' returns non-nil OR
  3. None of the functions in `dotfairy-unreal-buffer-functions' must return
     non-nil.
If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (when-let (basebuf (buffer-base-buffer buf))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (dotfairy-temp-buffer-p buf))
         (or (buffer-local-value 'dotfairy-real-buffer-p buf)
             (run-hook-with-args-until-success 'dotfairy-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'dotfairy-unreal-buffer-functions buf))))))

;;;###autoload
(defun dotfairy-unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.
See `dotfairy-real-buffer-p' for details on what that means."
  (not (dotfairy-real-buffer-p buffer-or-name)))

;;;###autoload
(defun dotfairy-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (dotfairy-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (dotfairy-buffer-list)))))

;;;###autoload
(defun dotfairy-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun dotfairy-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

;;;###autoload
(defun dotfairy-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (dotfairy-buffer-list))))

;;;###autoload
(defun dotfairy-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (dotfairy-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun dotfairy-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real).
See `dotfairy-real-buffer-p' for an explanation for real buffers."
  (with-current-buffer buffer
    (setq dotfairy-real-buffer-p flag)))

;;;###autoload
(defun dotfairy-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun dotfairy-fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (dotfairy-unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (dotfairy-unreal-buffer-p (window-buffer))
          (switch-to-buffer (dotfairy-fallback-buffer)))))))

;;;###autoload
(defun dotfairy-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (dotfairy-fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun dotfairy-kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (dotfairy-fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun dotfairy-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (dotfairy-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Hooks
;;;###autoload
(defun dotfairy-mark-buffer-as-real-h ()
  "Hook function that marks the current buffer as real.
See `dotfairy-real-buffer-p' for an explanation for real buffers."
  (dotfairy-set-buffer-real (current-buffer) t))


;;
;; Interactive commands

;;;###autoload
(defun dotfairy/save-and-kill-buffer ()
  "Save the current buffer to file, then kill it."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

;;;###autoload
(defun dotfairy/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer or the fallback buffer.
If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (dotfairy-kill-buffer-fixup-windows buffer))


(defun dotfairy--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun dotfairy/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.
If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (dotfairy-project-buffer-list)
           (dotfairy-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (dotfairy-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (dotfairy--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun dotfairy/kill-other-buffers (&optional buffer-list interactive)
  "Kill all other buffers (besides the current one).
If the prefix arg is passed, kill only buffers that belong to the current
project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (dotfairy-project-buffer-list)
                 (dotfairy-buffer-list)))
         t))
  (mapc #'dotfairy-kill-buffer-and-windows buffer-list)
  (dotfairy--message-or-count
   interactive "Killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun dotfairy/kill-matching-buffers (pattern &optional buffer-list interactive)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If the prefix arg is passed, only kill matching buffers in the current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         (if current-prefix-arg
             (dotfairy-project-buffer-list)
           (dotfairy-buffer-list))
         t))
  (dotfairy-kill-matching-buffers pattern buffer-list)
  (when interactive
    (message "Killed %d buffer(s)"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun dotfairy/kill-buried-buffers (&optional buffer-list interactive)
  "Kill buffers that are buried.
If PROJECT-P (universal argument), only kill buried buffers belonging to the
current project."
  (interactive
   (list (dotfairy-buried-buffers
          (if current-prefix-arg (dotfairy-project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (dotfairy--message-or-count
   interactive "Killed %d buried buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun dotfairy/kill-project-buffers (project &optional interactive)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let (open-projects (dotfairy-open-projects))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (dotfairy-project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (dotfairy-project-buffer-list project)))
      (dotfairy-kill-buffers-fixup-windows buffer-list)
      (dotfairy--message-or-count
       interactive "Killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))

(provide 'custom-buffer)
;;; init-buffers.el ends here
