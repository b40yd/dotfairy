;;; custom-projectile.el ---                                   -*- lexical-binding: t; -*-

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
;;
;;; Commands

;;;###autoload
(defun dotfairy/find-file-in-other-project (project-root)
  "Preforms `projectile-find-file' in a known project of your choosing."
  (interactive
   (list
    (completing-read "Find file in project: " (projectile-relevant-known-projects))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (dotfairy-project-find-file project-root))

;;;###autoload
(defun dotfairy/browse-in-other-project (project-root)
  "Preforms `find-file' in a known project of your choosing."
  (interactive
   (list
    (completing-read "Browse in project: " (projectile-relevant-known-projects))))
  (unless (file-directory-p project-root)
    (error "Project directory '%s' doesn't exist" project-root))
  (dotfairy-project-browse project-root))

;;;###autoload
(defun dotfairy/browse-in-emacsd ()
  "Browse files from `dotfairy-emacs-dir'."
  (interactive) (dotfairy-project-browse dotfairy-emacs-dir))

;;;###autoload
(defun dotfairy/find-file-in-emacsd ()
  "Find a file under `dotfairy-emacs-dir', recursively."
  (interactive) (dotfairy-project-find-file dotfairy-emacs-dir))

;;
;;; Library
;;;###autoload
(defun dotfairy-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (dotfairy-project-root dir)
       t))

;;;###autoload
(defun dotfairy-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun dotfairy-project-name (&optional dir)
  "Return the name of the current project.
Returns '-' if not in a valid project."
  (if-let (project-root (or (dotfairy-project-root dir)
                            (if dir (expand-file-name dir))))
      (funcall projectile-project-name-function project-root)
    "-"))

;;;###autoload
(defun dotfairy-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (dotfairy-project-root dir)))

;;;###autoload
(defun dotfairy-project-find-file (dir)
  "Jump to a file in DIR (searched recursively).
If DIR is not a project, it will be indexed (but not cached)."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename dir))
         (projectile-project-root (dotfairy-project-root dir))
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and projectile-project-root (file-equal-p projectile-project-root default-directory))
           (unless (dotfairy-project-p default-directory)
             ;; Disable caching if this is not a real project; caching
             ;; non-projects easily has the potential to inflate the projectile
             ;; cache beyond reason.
             (setq projectile-enable-caching nil))
           (call-interactively
            ;; Intentionally avoid `helm-projectile-find-file', because it runs
            ;; asynchronously, and thus doesn't see the lexical
            ;; `default-directory'
            (if (featurep 'ivy)
                #'counsel-projectile-find-file
              #'projectile-find-file)))
          ((fboundp 'counsel-file-jump) ; ivy only
           (call-interactively #'counsel-file-jump))
          ((project-current nil dir)
           (project-find-file-in nil nil dir))
          ((fboundp 'helm-find-files)
           (call-interactively #'helm-find-files))
          ((call-interactively #'find-file)))))

;;;###autoload
(defun dotfairy-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively
     (cond ((featurep 'ivy)
            #'counsel-find-file)
           (#'find-file)))))

(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (if (featurep 'ivy)
      (call-interactively #'counsel-file-jump)
    (cmd! (dotfairy-project-find-file default-directory))))

;;;###autoload
(defun +default/discover-projects (arg)
  "Discover projects in `projectile-project-search-path'.
If prefix ARG is non-nil, prompt for the search path."
  (interactive "P")
  (if arg
      (call-interactively #'projectile-discover-projects-in-directory)
    (if (not projectile-project-search-path)
        (user-error "`projectile-project-search-path' is empty; don't know where to search")
      (letf! (defun projectile-add-known-project (project-root)
               (unless (projectile-ignored-project-p project-root)
                 (funcall projectile-add-known-project project-root)
                 (message "Added %S to known project roots" project-root)))
        (dolist (dir projectile-project-search-path)
          (if (not (file-accessible-directory-p dir))
              (message "%S was inaccessible and couldn't searched" dir)
            (projectile-discover-projects-in-directory dir)))))))

;;;###autoload
(defun +default/dired (arg)
  "Open a directory in dired.
If prefix ARG is non-nil, prompt for a known project to open in dired."
  (interactive "P")
  (apply #'dired
         (if arg
             (list (completing-read "Open dired in project: " projectile-known-projects))
           (dired-read-dir-and-switches ""))))


(defun +default/browse-project ()
  "Browse files from the current project's root."
  (interactive) (dotfairy-project-browse (dotfairy-project-root)))


(defun +default/browse-notes ()
  "Browse files from `org-directory'."
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (dotfairy-project-browse org-directory))
;;;###autoload
(defun +default/find-in-notes ()
  "Find a file under `org-directory', recursively."
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (dotfairy-project-find-file org-directory))


;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.
If a compilation window is already open, recompile that instead.
If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

(provide 'custom-projectile)
;;; custom-projectile.el ends here
