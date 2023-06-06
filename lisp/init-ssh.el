;;; init-ssh.el --- A SSH manager remote servers  tools -*- lexical-binding: t -*-

;; Copyright © 2020-2024 b40yd

;; Author: b40yd <bb.qnyd@gmail.com>
;; Keywords: ssh, tools
;; URL: https://github.com/7ym0n/dotfairy-ssh-manager
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (f "0.20.0"))

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
;; A SSH session manager and files upload or download tools for Emacs.
;; It's like `xshell', `mobaxterm' or other tools same work.

;;; Code:
(use-package ssh-manager
  :ensure nil
  :quelpa (ssh-manager :fetcher github :repo "b40yd/dotfairy-ssh-manager")
  :init
  (setq ssh-manager-sshpass-path (expand-file-name "sshpass" dotfairy-local-dir)
        ssh-manager-sshpass-bin (concat ssh-manager-sshpass-path "/bin/sshpass")))

(provide 'init-ssh)
;;; ssh-manager.el ends here
