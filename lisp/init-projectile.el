;;; init-projectile.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  7ym0n.q6e

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
(require 'init-const)
(require 'init-funcs)

(defvar projectile-project-root nil)
(defvar projectile-enable-caching)
(defvar projectile-require-project-root)
(defvar dotfairy-projectile-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "The filename of the `fd' executable. On some distros it's 'fdfind' (ubuntu,
debian, and derivatives). On most it's 'fd'.")

(use-package projectile
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "ⓟ"
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-cache-file (concat dotfairy-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching t
        projectile-project-search-path '("~/")
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat dotfairy-cache-dir "projectile.projects"))
  (projectile-mode +1)

  :config
  ;; Projectile runs four functions to determine the root (in this order):
  ;;
  ;; + `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;    for an explicit path.
  ;; + `projectile-root-bottom-up' -> searches from / to your current directory
  ;;   for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;   includes .git and .project
  ;; + `projectile-root-top-down' -> searches from the current directory down to
  ;;   / the paths listed in `projectile-root-files', like package.json,
  ;;   setup.py, or Cargo.toml
  ;; + `projectile-root-top-down-recurring' -> searches from the current
  ;;   directory down to / for a directory that has one of
  ;;   `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;   parent directory with the same file.
  ;;
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".project"     ; dotfairy project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  (push (abbreviate-file-name dotfairy-local-dir) projectile-globally-ignored-directories)

  ;; Override projectile's dirconfig file '.projectile' with dotfairy's project marker '.project'.
  (defadvice! my--projectile-dirconfig-file-a ()
    :override #'projectile-dirconfig-file
    (cond
     ;; Prefers '.projectile' to maintain compatibility with existing projects.
     ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
     ((expand-file-name ".project" (projectile-project-root)))))

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when IS-WINDOWS
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'hybrid
            projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))


(use-package treemacs
  :autoload (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   30
        treemacs-persist-file (concat dotfairy-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat dotfairy-cache-dir "treemacs-last-error-persist"))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; Projectile integration
  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
           ("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :after magit
    :autoload treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))
  (use-package treemacs-persp
    :after persp-mode
    :demand t
    :functions treemacs-set-scope-type
    :config (treemacs-set-scope-type 'Perspectives)))

;; Custom

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

(defun dotfairy--update-files (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))
      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))
      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))
        (when (and (bound-and-true-p projectile-mode)
                   (dotfairy-project-p)
                   (projectile-file-cached-p file (dotfairy-project-root)))
          (projectile-purge-file-from-cache file))))
    (dolist (default-directory toplevels)
      (magit-refresh))
    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))
;;;###autoload
(defun dotfairy/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; Ensures that windows displaying this buffer will be switched to
          ;; real buffers (`dotfairy-real-buffer-p')
          (dotfairy/kill-this-buffer-in-all-windows buf t)
          (dotfairy--update-files path)
          (message "Deleted %S" short-path))))))

;;;###autoload
(defun dotfairy/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (dotfairy--update-files old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

;;;###autoload
(defun dotfairy/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (dotfairy--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

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

;;;###autoload
(defun dotfairy/add-directory-as-project (dir)
  "Register an arbitrary directory as a project.
If DIR is not a valid project, a .project file will be created within it. This
command will throw an error if a parent of DIR is a valid project (which would
mask DIR)."
  (interactive "D")
  (let ((short-dir (abbreviate-file-name dir)))
    (unless (dotfairy-project-p dir)
      (with-temp-file (dotfairy-path dir ".project")))
    (let ((proj-dir (dotfairy-project-root dir)))
      (unless (file-equal-p proj-dir dir)
        (user-error "Can't add %S as a project, because %S is already a project"
                    short-dir (abbreviate-file-name proj-dir)))
      (message "%S was not a project; adding .project file to it"
               short-dir (abbreviate-file-name proj-dir))
      (projectile-add-known-project dir))))
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
            ;; because it runs asynchronously, and thus doesn't see the lexical
            ;; `default-directory'
            (if (bound-and-true-p ivy-mode)
                #'counsel-projectile-find-file
              #'projectile-find-file)))
          ((and (bound-and-true-p vertico-mode)
                (fboundp '+vertico/find-file-in))
           (+vertico/find-file-in default-directory))
          ((and (bound-and-true-p ivy-mode)
                (fboundp 'counsel-file-jump))
           (call-interactively #'counsel-file-jump))
          ((project-current nil dir)
           (project-find-file-in nil nil dir))
          ((call-interactively #'find-file)))))

;;;###autoload
(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (dotfairy-project-find-file default-directory))

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
  (interactive)
  (dotfairy-project-browse (or (dotfairy-project-root) default-directory)))

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
     (cond ((bound-and-true-p ivy-mode)
            #'+ivy/project-search-from-cwd)
           ((bound-and-true-p vertico-mode)
            #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (+default/search-cwd 'other))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.
If a selection is active and multi-line, perform a search restricted to that
region.
If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))
      (cond ((bound-and-true-p ivy-mode)
             (call-interactively
              (if (and start end (not multiline-p))
                  #'swiper-isearch-thing-at-point
                #'swiper-isearch)))
            ((bound-and-true-p vertico-mode)
             (if (and start end (not multiline-p))
                 (consult-line
                  (replace-regexp-in-string
                   " " "\\\\ "
                   (rxt-quote-pcre
                    (buffer-substring-no-properties start end))))
               (call-interactively #'consult-line)))))))

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
     (cond ((bound-and-true-p ivy-mode)
            #'+ivy/project-search)
           ((bound-and-true-p vertico-mode)
            #'+vertico/project-search)
           (#'projectile-ripgrep)))))

;;;###autoload
(defun +default/search-other-project ()
  "Conduct a text search in a known project."
  (interactive)
  (+default/search-project 'other))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (dotfairy-thing-at-point-or-region) ""))
         (let ((projectile-project-root nil))
           (if current-prefix-arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             (dotfairy-project-root default-directory)))))
  (cond ((bound-and-true-p ivy-mode)
         (+ivy/project-search nil symbol dir))
        ((bound-and-true-p vertico-mode)
         (+vertico/project-search nil symbol dir))
        (regexp-quote symbol)))

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (dotfairy-thing-at-point-or-region) ""))))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   symbol org-directory))
(provide 'init-projectile)
;;; init-projectile.el ends here
