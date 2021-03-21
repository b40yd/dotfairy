;;; init-projectile.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  7ym0n.q6e

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


(defvar projectile-project-root nil)
(defvar projectile-enable-caching)
(defvar projectile-require-project-root)

(use-package projectile
  :init
  (setq projectile-mode-line-prefix "ⓟ"
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-cache-file (concat dotfairy-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching t
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat dotfairy-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/" "/tmp"))
  (global-set-key [remap find-tag]         #'projectile-find-tag)
  (use-package ag
    :ensure t)
  (use-package counsel-projectile
    :bind (:map projectile-mode-map
                ("C-x p" . projectile-command-map))
    :hook
    (after-init . counsel-projectile-mode)
    :config
    ;; no highlighting visited files; slows down the filtering
    (ivy-set-display-transformer #'counsel-projectile-find-file nil)
    (setq counsel-projectile-sort-files t))

  :hook (after-init . projectile-mode)
  :config
  (projectile-mode +1)
                                        ; Projectile runs four functions to determine the root (in this order):
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
                  ".project"     ; project marker
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

  ;; Disable commands that won't work, as is, and that Doom already provides a
  ;; better alternative for.
  (put 'projectile-ag 'disabled "Use +{ivy}/project-search instead")
  (put 'projectile-ripgrep 'disabled "Use +{ivy}/project-search instead")
  (put 'projectile-grep 'disabled "Use +{ivy}/project-search instead")

  ;; It breaks projectile's project root resolution if HOME is a project (e.g.
  ;; it's a git repo). In that case, we disable bottom-up root searching to
  ;; prevent issues. This makes project resolution a little slower and less
  ;; accurate in some cases.
  (let ((default-directory "~"))
    (when (cl-find-if #'projectile-file-exists-p
                      projectile-project-root-files-bottom-up)
      (setq projectile-project-root-files
            (append projectile-project-root-files-bottom-up
                    projectile-project-root-files)
            projectile-project-root-files-bottom-up nil)))


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

  (setq projectile-completion-system 'ivy))


(provide 'init-projectile)
;;; init-projectile.el ends here
