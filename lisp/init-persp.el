;;; init-persp.el ---                                -*- lexical-binding: t; -*-

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
(require 'init-custom)

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-nil-hidden t
              persp-autokill-buffer-on-remove 'kill-weak
              persp-reset-windows-on-nil-window-conf nil
              persp-set-last-persp-for-new-frames t
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-save-opt (if noninteractive 0 1) ; auto-save on kill
              persp-auto-resume-time (if dotfairy-dashboard 0 1.0))
  :config
  (setq persp-save-dir (concat dotfairy-etc-dir "workspaces/"))
  ;; Save and load frame parameters (size & position)
  (defvar persp-frame-file (expand-file-name "persp-auto-save" persp-save-dir)
    "File of saving frame parameters.")

  (with-no-warnings
    ;; Don't save if the state is not loaded
    (defvar persp-state-loaded nil
      "Whether the state is loaded.")

    (defun my-persp-after-load-state (&rest _)
      (setq persp-state-loaded t))
    (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'find-file-hook #'my-persp-after-load-state)))

    (defun my-persp-asave-on-exit (fn &optional interactive-query opt)
      (if persp-state-loaded
          (funcall fn interactive-query opt)
        t))
    (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

  ;; Don't save dead or temporary buffers
  (add-to-list 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore dead buffers."
                 (not (buffer-live-p b))))
  (add-to-list 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore temporary buffers."
                 (let ((bname (file-name-nondirectory (buffer-name b))))
                   (or (string-prefix-p ".newsrc" bname)
                       (string-prefix-p "magit" bname)
                       (string-prefix-p "Pfuture-Callback" bname)
                       (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                       (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)
                       (eq (buffer-local-value 'major-mode b) 'erc-mode)
                       (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                       (eq (buffer-local-value 'major-mode b) 'nov-mode)
                       (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Ivy Integraticon
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 (lambda (b)
                   (when persp-mode
                     (if-let ((persp (get-current-persp)))
                         (not (persp-contain-buffer-p b persp))
                       nil)))))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory)))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
  :config
  (with-no-warnings
    ;; HACK: Allow saving to files
    (defun my-persp-mode-projectile-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-projectile-bridge t persp)
                (persp-add-buffer (projectile-project-buffers)
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-projectile-bridge-add-new-persp
                :override #'my-persp-mode-projectile-bridge-add-new-persp)))
(require 'init-session)
(provide 'init-persp)
;;; init-persp.el ends here
