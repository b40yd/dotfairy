;;; init-ssh.el ---                                   -*- lexical-binding: t; -*-

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
(require 'cl-generic)
(require 'cl-lib)
(require 'dash)

(cl-defstruct dotfairy-ssh-session
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defcustom dotfairy-ssh-session-file (expand-file-name (locate-user-emacs-file ".dotfairy-ssh-session-v1"))
  "File where session information is stored."
  :group 'dotfairy
  :type 'file)

(defvar dotfairy--ssh-session nil
  "Contain the `dotfairy-ssh-session' for the current Emacs instance.")
(defvar dotfairy--ssh-show-message t
  "If non-nil, show debug message from `dotfairy'.")

(defun dotfairy--ssh-message  (format &rest args)
  "Wrapper for `message'
We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `dotfairy--ssh-info', `dotfairy--ssh-warn' and `dotfairy--ssh-error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.
See #2049"
  (when dotfairy--ssh-show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

(defun dotfairy--ssh-info (format &rest args)
  "Display dotfairy info message with FORMAT with ARGS."
  (dotfairy--ssh-message "%s :: %s" (propertize "LSP" 'face 'success) (apply #'format format args)))

(defun dotfairy--ssh-warn (format &rest args)
  "Display dotfairy warn message with FORMAT with ARGS."
  (dotfairy--ssh-message "%s :: %s" (propertize "LSP" 'face 'warning) (apply #'format format args)))

(defun dotfairy--ssh-error (format &rest args)
  "Display dotfairy error message with FORMAT with ARGS."
  (dotfairy--ssh-message "%s :: %s" (propertize "LSP" 'face 'error) (apply #'format format args)))
(defun dotfairy--ssh-read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun dotfairy--ssh-persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME.
This function creates the parent directories if they don't exist
yet."
  (let ((print-length nil)
        (print-level nil))
    ;; Create all parent directories:
    (apply #'f-mkdir (f-split (f-parent file-name)))
    (f-write-text (prin1-to-string to-persist) 'utf-8 file-name)))

(defun dotfairy--ssh-persist-session (session)
  "Persist SESSION to `dotfairy-ssh-session-file'."
  (dotfairy--ssh-persist dotfairy-ssh-session-file (make-dotfairy-ssh-session
                                                :servers (dotfairy-ssh-session-servers session))))
(defun dotfairy--ssh-load-default-session ()
  "Load default session."
  (setq dotfairy--ssh-session (or (condition-case err
                                  (dotfairy--ssh-read-from-file dotfairy-ssh-session-file)
                                (error (dotfairy--ssh-error "Failed to parse the session %s, starting with clean one."
                                                        (error-message-string err))
                                       nil))
                              (make-dotfairy-ssh-session))))

(defun dotfairy-ssh-session ()
  "Get the session associated with the current buffer."
  (or dotfairy--ssh-session (setq dotfairy--ssh-session (dotfairy--ssh-load-default-session))))

;;
;; sshpass for MacOS
;; curl -L https://raw.githubusercontent.com/kadwanev/bigboybrew/master/Library/Formula/sshpass.rb -o sshpass.rb
;; brew install sshpass.rb
;; sshpass with TOTP support https://github.com/dora38/sshpass.git
;; brew install oath-toolkit
;; yum install oathtool gnupg2
;; sshpass for Linux(debian, centos)
;; sudo apt-get install sshpass
;; sudo yum install sshpass
(defun dotfairy-connect-ssh (server)
  (let* ((session-name (plist-get server :session-name))
         (username (plist-get server :username))
         (password (plist-get server :password))
         (port (plist-get server :hostport))
         (host (plist-get server :hostname))
         (totp-key (if (string-empty-p (plist-get server :totp-key))
                       ""
                     (with-temp-buffer
                       (or (apply #'call-process "oathtool" nil t nil (list "--totp" "-b" (plist-get server :totp-key)))
                           "")
                       (string-trim (buffer-string)))
                     ))
         (totp-message (if (string-empty-p (plist-get server :totp-message))
                           ""
                         (format "%s" (plist-get server :totp-message))))
         )
    (if (string-empty-p password)
        (set-buffer (apply 'make-term
                           session-name
                           "ssh"
                           nil
                           (list host "-l" username "-p" port)))
      (if (string-empty-p totp-key)
          (set-buffer (apply 'make-term
                             session-name
                             "sshpass"
                             nil
                             (list "-p" password "ssh" host "-l" username "-p" port)))
        (if (string-empty-p totp-message)
            (set-buffer (apply 'make-term
                               session-name
                               "sshpass"
                               nil
                               (list "-p" password "-o" totp-key "ssh" host "-l" username "-p" port)))
          (set-buffer (apply 'make-term
                             session-name
                             "sshpass"
                             nil
                             (list "-p" password "-o" totp-key "-O" totp-message "ssh" host "-l" username "-p" port)))
          )))
    (define-key term-mode-map (kbd "C-c c-b") 'switch-to-buffer)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer (format "*%s*" session-name))
    ))

(defun dotfairy--filter-ssh-session ()
  "filter ssh session name list."
  (let ((lst '()))
    (dolist (server (->> (dotfairy-ssh-session)
                         (dotfairy-ssh-session-servers)))
      (setq lst (append lst (list (plist-get server :session-name)))))
    lst))

(defun dotfairy/ssh-server-switch-to (session)
  "Select ssh server to connect."
  (interactive (list (completing-read "Select server to connect: "
                                      (dotfairy--filter-ssh-session)
                                      )))
  (dolist (server (->> (dotfairy-ssh-session)
                       (dotfairy-ssh-session-servers)))
    (if (equal (plist-get server :session-name) session)
        (dotfairy-connect-ssh server)
      (dotfairy--ssh-error "not connect."))))

(defun dotfairy/create-remote-ssh (session-name host port user password totp-key totp-message)
  "Connect to a remote host by SSH."
  (interactive "sSesion name: \nsHost: \nsPort (default 22): \nsUser: \nsPassword: \nsTOTP key: \nsTOTP message: ")
  (let* ((port (if (equal port "")
                   "22"
                 port))
         (let-password (if (not (equal password ""))
                           (format "-p %s" password)
                         ""))
         (let-totp-key (if (not (equal totp-key ""))
                           (if (executable-find "oathtool")
                               (format "oathtool --totp -b %s" totp-key)
                             "")
                         ""))
         (let-totp-message (if (not (equal totp-message ""))
                               (format "-o %s" totp-message)
                             ""))
         (switches (list host "-l" user "-p" port))
         (let-plist (list :session-name session-name
                          :username  user
                          :password  password
                          :hostname  host
                          :hostport  port
                          :totp-key  totp-key
                          :totp-message  totp-message)))
    (cl-pushnew let-plist
                (dotfairy-ssh-session-servers (dotfairy-ssh-session)) :test 'equal)
    (dotfairy--ssh-persist-session (dotfairy-ssh-session))
    (dotfairy-connect-ssh let-plist)))

(defun dotfairy/remove-ssh-server (session)
  "Remove session from the list of servers."
  (interactive (list (completing-read "Select server to connect: "
                                      (dotfairy--filter-ssh-session)
                                      )))
  (dolist (server (->> (dotfairy-ssh-session)
                       (dotfairy-ssh-session-servers)))
    (if (string= session (plist-get server :session-name))
        (let* ((let-sessions (dotfairy-ssh-session)))
          (setf (dotfairy-ssh-session-servers let-sessions)
                (-remove-item server (dotfairy-ssh-session-servers let-sessions)))
          (dotfairy--ssh-persist-session (dotfairy-ssh-session))))))

(provide 'init-ssh)
;;; init-ssh.el ends here
