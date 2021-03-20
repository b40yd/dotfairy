;;; custom-ivy.el ---                                   -*- lexical-binding: t; -*-

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
(defun +ivy/compile ()
  "Execute a compile command from the current buffer's directory."
  (interactive)
  (counsel-compile default-directory))

;;;###autoload
(defun +ivy/project-compile ()
  "Execute a compile command from the current project's root."
  (interactive)
  (counsel-compile (projectile-project-root)))

;;
;; Library

(defun +ivy--switch-buffer-preview ()
  (let (ivy-use-virtual-buffers ivy--virtual-buffers)
    (counsel--switch-buffer-update-fn)))

(defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
(defalias '+ivy--switch-buffer-unwind      #'counsel--switch-buffer-unwind)

(defvar +ivy-buffer-preview nil
  "If non-nil, preview buffers while switching, à la `counsel-switch-buffer'.
When nil, don't preview anything.
When non-nil, preview non-virtual buffers.
When 'everything, also preview virtual buffers")

(defvar +ivy-buffer-unreal-face 'font-lock-comment-face
  "The face for unreal buffers in `ivy-switch-to-buffer'.")

(defvar +ivy-edit-functions nil
  "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")


;;;###autoload
(defun +ivy/woccur ()
  "Invoke a wgrep buffer on the current ivy results, if supported."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let ((caller (ivy-state-caller ivy-last)))
    (if-let (occur-fn (plist-get +ivy-edit-functions caller))
        (ivy-exit-with-action
         (lambda (_) (funcall occur-fn)))
      (if-let (occur-fn (plist-get ivy--occurs-list caller))
          (let ((buffer (generate-new-buffer
                         (format "*ivy-occur%s \"%s\"*"
                                 (if caller (concat " " (prin1-to-string caller)) "")
                                 ivy-text))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (funcall occur-fn))
              (setf (ivy-state-text ivy-last) ivy-text)
              (setq ivy-occur-last ivy-last)
              (setq-local ivy--directory ivy--directory))
            (ivy-exit-with-action
             `(lambda (_)
                (pop-to-buffer ,buffer)
                (ivy-wgrep-change-to-wgrep-mode))))
        (user-error "%S doesn't support wgrep" caller)))))


(defun +ivy--is-workspace-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (+workspace-contains-buffer-p buffer)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")

(defun +ivy--is-workspace-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))


(defun +ivy--switch-buffer (workspace other)
  (let ((current (not other))
        prompt action filter update unwind)
    (cond ((and workspace current)
           (setq prompt "Switch to workspace buffer: "
                 action #'ivy--switch-buffer-action
                 filter #'+ivy--is-workspace-other-buffer-p))
          (workspace
           (setq prompt "Switch to workspace buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action
                 filter #'+ivy--is-workspace-buffer-p))
          (current
           (setq prompt "Switch to buffer: "
                 action #'ivy--switch-buffer-action))
          ((setq prompt "Switch to buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action)))
    (when +ivy-buffer-preview
      (cond ((not (and ivy-use-virtual-buffers
                       (eq +ivy-buffer-preview 'everything)))
             (setq update #'+ivy--switch-buffer-preview
                   unwind #'+ivy--switch-buffer-unwind))
            ((setq update #'+ivy--switch-buffer-preview-all
                   unwind #'+ivy--switch-buffer-unwind))))
    (ivy-read prompt 'internal-complete-buffer
              :action action
              :predicate filter
              :update-fn update
              :unwind unwind
              :preselect (buffer-name (other-buffer (current-buffer)))
              :matcher #'ivy--switch-buffer-matcher
              :keymap ivy-switch-buffer-map
              ;; NOTE A clever disguise, needed for virtual buffers.
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.
If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;;;###autoload
(defun +ivy/switch-workspace-buffer-other-window ()
  "Switch another window to a buffer within the current workspace."
  (interactive)
  (+ivy--switch-buffer t t))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (+ivy--switch-buffer nil nil))

;;;###autoload
(defun +ivy/switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (+ivy--switch-buffer nil t))

;;;###autoload
(cl-defun +ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.
:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (when (featurep 'counsel)
    (let* ((this-command 'counsel-rg)
           (project-root (or (dotfairy-project-root) default-directory))
           (directory (or in project-root))
           (args (concat (if all-files " -uu")
                         (unless recursive " --maxdepth 1")
                         " --hidden -g!.git "
                         (mapconcat #'shell-quote-argument args " "))))
      (setq deactivate-mark t)
      (counsel-rg
       query
       directory args
       (or prompt
           (format "Search project [%s]: "
                   (cond ((equal directory default-directory)
                          "./")
                         ((equal directory project-root)
                          (projectile-project-name))
                         ((file-relative-name directory project-root)))
                   (string-trim args)))))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+ivy/project-search arg initial-query default-directory))

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively
     (cond ((featurep 'ivy)  #'+ivy/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.
If a selection is active, pre-fill the prompt with it."
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'swiper-isearch-thing-at-point
     #'swiper-isearch)))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively
     (cond ((featurep 'ivy)  #'+ivy/project-search)

           (#'projectile-ripgrep)))))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))

(provide 'custom-ivy)
;;; custom-ivy.el ends here
