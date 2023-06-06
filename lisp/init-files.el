;;; init-files.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2024 b40yd

;; Author: b40yd <bb.qnyd@gmail.com>
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

(defun dotfairy/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun dotfairy/rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun dotfairy/sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun dotfairy/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (dotfairy/sudo-file-path file)))

;;;###autoload
(defun dotfairy/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (dotfairy/sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;###autoload
(defun dotfairy/sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (dotfairy/sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

;; Open custom file
(defun dotfairy/open-custom-file()
  "Open or create `custom-file'."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p dotfairy-custom-example-file)
        (copy-file dotfairy-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" dotfairy-custom-example-file)))
  (find-file custom-file))

;; Reload configurations
(defun dotfairy/reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file)
  (funcall major-mode))

(defun dotfairy/open-init-file()
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun dotfairy/remove-recent-file (file)
  "Remove FILE from your recently-opened-files list."
  (interactive
   (list (completing-read "Remove recent file: " recentf-list
                          nil t)))
  (setq recentf-list (delete file recentf-list))
  (recentf-save-list)
  (message "Removed %S from `recentf-list'" (abbreviate-file-name file)))
(provide 'init-files)
;;; init-files.el ends here
