;;; init-package.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 b40yd

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
(require 'init-funcs)

;; create custom env path
(make-custom-directory)

(setq package-user-dir dotfairy-package-dir)
(setq custom-file (expand-file-name "custom.el" dotfairy-private-dir))

(when (and (file-exists-p dotfairy-custom-example-file)
           (not (file-exists-p custom-file)))
  (copy-file dotfairy-custom-example-file custom-file)

  ;; Test and select the fastest package archives
  (message "Testing connection... Please wait a moment.")
  (set-package-archives (dotfairy-test-package-archives 'no-chart)))

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; Load custom-post file
(defun load-custom-post-file ()
  "Load custom-post file."
  (cond ((file-exists-p dotfairy-custom-post-org-file)
         (and (fboundp 'org-babel-load-file)
              (org-babel-load-file dotfairy-custom-post-org-file)))
        ((file-exists-p dotfairy-custom-post-file)
         (load dotfairy-custom-post-file))))
(add-hook 'after-init-hook #'load-custom-post-file)

;; HACK: DO NOT save `package-selected-packages' to `custom-file'
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to variable `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Set ELPA packages
(set-package-archives dotfairy-package-archives nil nil t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))

;; (eval-when-compile
;;   (require 'use-package))

;; Use quelpa install packages
(use-package quelpa
  :init
  (setq quelpa-upgrade-p dotfairy-quelpa-upgrade
        quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package)
(eval-when-compile
  (require 'quelpa-use-package))

;; Don't display minor modes
(use-package diminish)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Update packages
(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :init
    (setq auto-package-update-delete-old-versions t
          auto-package-update-hide-results t)
    (defalias 'package-upgrade-all #'auto-package-update-now)))

;; Update
(defun update-config ()
  "Update Dotfairy Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))

    (message "Updating configurations...")
    (cd dir)
    (shell-command "git pull")
    (message "Updating configurations...done")))
(defalias 'dotfairy/update-config #'update-config)

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-update-all)
  (message "Updating packages...done"))
(defalias 'dotfairy/update-packages #'update-packages)

(defun update-config-and-packages()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (update-packages))
(defalias 'dotfairy/update #'update-config-and-packages)

(defun update-all()
  "Update dotfiles, org files, configurations and packages to the latest."
  (interactive)
  (update-org)
  (update-config-and-packages))
(defalias 'dotfairy/update-all #'update-all)

(defun dotfairy/try-get-org-remote-repository ()
  "Download org remote repository."
  (interactive)
  (let ((roam-dir (expand-file-name "roam" dotfairy-org-dir)))
    (if (and (stringp dotfairy-org-repository)
             (not (string= dotfairy-org-repository "")))
        (progn
          (message "Get Org remote files...")
          (shell-command (concat "git clone " dotfairy-org-repository " " dotfairy-org-dir))
          (if (not (file-exists-p roam-dir))
              (make-directory roam-dir))
          (message "Get Org remote files...done")))))

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name dotfairy-org-dir)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating Org files...done"))
      (dotfairy/try-get-org-remote-repository))))
(defalias 'dotfairy/update-org #'update-org)


(provide 'init-package)
;;; init-package.el ends here
