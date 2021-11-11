;;; init-const.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n.q6e, all rights reserved.

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
(require 'subr-x)

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;;; Directories/files
(defconst dotfairy-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst dotfairy-lisp-dir (concat dotfairy-emacs-dir "lisp/")
  "The root directory of DotFairy's core files. Must end with a slash.")

(defconst dotfairy-local-dir
  (if-let (localdir (getenv "DOTFAIRYLOCALDIR"))
      (expand-file-name (file-name-as-directory localdir))
    (concat dotfairy-emacs-dir ".local/"))
  "Root directory for local storage.
Use this as a storage location for this system's installation of DotFairy Emacs.
These files should not be shared across systems. By default, it is used by
`dotfairy-etc-dir' and `dotfairy-cache-dir'. Must end with a slash.")

(defconst dotfairy-org-dir (concat dotfairy-local-dir "org/")
  "The root directory of DotFairy's core files. Must end with a slash.")

(defconst dotfairy-etc-dir (concat dotfairy-local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst dotfairy-cache-dir (concat dotfairy-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defconst dotfairy-package-dir (expand-file-name "elpa/" dotfairy-local-dir))

(defconst dotfairy-private-dir
  (if-let (dotfairydir (getenv "DOTFAIRYDIR"))
      (expand-file-name (file-name-as-directory dotfairydir))
    (or (let ((xdgdir
               (expand-file-name "dotfairy/"
                                 (or (getenv "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.dotfairy.d/"))
  "Where your private configuration is placed.
Defaults to ~/.config/dotfairy, ~/.dotfairy.d or the value of the DOTFAIRYDIR envvar;
whichever is found first. Must end in a slash.")

(defconst dotfairy-custom-example-file
  (expand-file-name "config-example.el" dotfairy-emacs-dir)
  "Custom example file of Dotfairy Emacs.")

(defconst dotfairy-custom-post-file
  (expand-file-name "config.el" user-emacs-directory)
  "Custom file after startup.
Put private configurations to override defaults here.")

(defconst dotfairy-custom-post-org-file
  (expand-file-name "config.org" user-emacs-directory)
  "Custom org file after startup.
Put private configurations to override defaults here.
Loaded by `org-babel-load-file'.")

(provide 'init-const)
;;; init-const.el ends here
