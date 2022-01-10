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
