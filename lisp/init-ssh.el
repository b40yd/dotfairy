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
(require 'term)
(require 'f)
(require 'init-funcs)

;; ssh-manager-mode
(cl-defstruct dotfairy-ssh-session-groups
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defvar dotfairy--ssh-session-groups nil
  "Contain the `dotfairy-ssh-session-groups' for the current Emacs instance.")

(defun dotfairy-ssh-session-groups ()
  (or dotfairy--ssh-session-groups (setq dotfairy--ssh-session-groups (make-dotfairy-ssh-session-groups))))

(defun dotfairy/show-ssh-session-groups ()
  "Show ssh server groups."
  (interactive)
  (message (format "%s" (dotfairy-ssh-session-groups-servers (dotfairy-ssh-session-groups)))))

(defun dotfairy/add-this-ssh-session-to-groups ()
  "Add this ssh server session to groups."
  (interactive)
  (cl-pushnew (buffer-name) (dotfairy-ssh-session-groups-servers (dotfairy-ssh-session-groups)) :test 'equal))

(defun dotfairy/remove-this-ssh-session-from-groups ()
  "Remove this ssh server session from groups."
  (interactive)
  (dotfairy--remove-buffer-name-from-groups (buffer-name)))

(defun dotfairy--remove-buffer-name-from-groups (buf-name)
  "Remove buffer name from groups."

  (setf (dotfairy-ssh-session-groups-servers (dotfairy-ssh-session-groups))
        (-remove-item buf-name (dotfairy-ssh-session-groups-servers (dotfairy-ssh-session-groups)))))

(defun dotfairy/remove-ssh-session-from-groups (session)
  "Remove ssh server session from groups."
  (interactive  (list (completing-read "Select server to connect: "
                                       (dotfairy-ssh-session-groups-servers (dotfairy-ssh-session-groups))
                                       )))
  (dotfairy--remove-buffer-name-from-groups session)
  )

(defun dotfairy-ssh-send-cmd-to-session-groups (cmd)
  (let ((current-buf (current-buffer)))
    (dolist (server (->> (dotfairy-ssh-session-groups)
                         (dotfairy-ssh-session-groups-servers)))
      (dotfairy--send-cmd-to-buffer server cmd))
    (switch-to-buffer current-buf)))

(defvar ssh-manager-mode-map nil "keymap for `ssh-manager-mode'")

(setq ssh-manager-mode-map (make-sparse-keymap))

(define-key ssh-manager-mode-map (kbd "C-c C-c") 'dotfairy/execute-buffer-cmd-to-ssh)
(define-key ssh-manager-mode-map (kbd "C-c C-e") 'dotfairy/execute-region-cmd-to-ssh)
(define-key ssh-manager-mode-map (kbd "C-c C-.") 'dotfairy/execute-current-line-cmd-to-ssh)

(define-derived-mode ssh-manager-mode prog-mode "SSH Manager Mode"
  (font-lock-fontify-buffer)
  (use-local-map ssh-manager-mode-map))

(defun dotfairy/execute-current-line-cmd-to-ssh ()
  "Execute command to ssh server groups."
  (interactive)
  (let ((line (dotfairy/read-current-line-cmd)))
    (dotfairy-ssh-send-cmd-to-session-groups line)
    (reindent-then-newline-and-indent)
    ))

(defun dotfairy/read-current-line-cmd ()
  "Read current line command."
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun dotfairy/execute-region-cmd-to-ssh ()
  "Execute region cmd to ssh"
  (interactive)
  (let ((begin (region-beginning))
        (end (region-end)))
    (dotfairy-ssh-send-cmd-to-session-groups (buffer-substring begin end))))

(defun dotfairy/execute-buffer-cmd-to-ssh ()
  "Execute buffer cmd to ssh"
  (interactive)
  (dotfairy-ssh-send-cmd-to-session-groups (buffer-substring-no-properties (point-min) (point-max)))
  )

(defun ssh-manager ()
  (interactive)
  (let ((buffer (generate-new-buffer "*SSH Manager*")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)
    (funcall 'ssh-manager-mode)
    (setq buffer-offer-save t)))

;; ssh connect session
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
  (dotfairy--ssh-message "%s :: %s" (propertize "SSH" 'face 'success) (apply #'format format args)))

(defun dotfairy--ssh-warn (format &rest args)
  "Display dotfairy warn message with FORMAT with ARGS."
  (dotfairy--ssh-message "%s :: %s" (propertize "SSH" 'face 'warning) (apply #'format format args)))

(defun dotfairy--ssh-error (format &rest args)
  "Display dotfairy error message with FORMAT with ARGS."
  (dotfairy--ssh-message "%s :: %s" (propertize "SSH" 'face 'error) (apply #'format format args)))
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

(defun term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))
                            ))))

(defun docker--init-term-mode (term-name)
  "Init term mode"
  (progn
    (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
    (define-key term-raw-map (kbd "C-c C-b") 'switch-to-buffer)
    (define-key term-raw-map (kbd "C-c C-a") 'dotfairy/add-this-ssh-session-to-groups)
    (define-key term-raw-map (kbd "C-c C-r") 'dotfairy/remove-this-ssh-session-from-groups)
    (define-key term-raw-map (kbd "C-c M-a") 'dotfairy/show-ssh-session-groups)
    (term-mode)
    (term-char-mode)
    (term-handle-close)
    (add-hook 'kill-buffer-hook 'term-kill-buffer-hook)
    (switch-to-buffer (format "*%s*" term-name))))

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
         (kind (plist-get server :kind))
         (username (plist-get server :remote-user))
         (password (plist-get server :remote-password))
         (port (plist-get server :remote-port))
         (host (plist-get server :remote-host))
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
         (proxy-host (plist-get server :proxy-host))
         (proxy-port (plist-get server :proxy-port))
         (proxy-user (plist-get server :proxy-user))
         (index 1))
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" session-name index)))
      (setq index (1+ index)))
    (cond ((string= kind "proxy")
           (if (or (string-empty-p host)
                   (string-empty-p proxy-host))
               (dotfairy--ssh-error "<Proxy host> and <Remote host> must be set. please check its.")
             (progn
               (let* ((argv '())
                      (term-argv '())
                      (term-name (format "%s<%s>" session-name index)))
                 (if (not (string-empty-p totp-key))
                     (setq argv (append argv `("-o" ,totp-key))))
                 (if (not (string-empty-p totp-message))
                     (setq argv (append argv `("-O" ,totp-message))))
                 (if (length> argv 0)
                     (let ((remote-server (format "%s@%s" username host))
                           (proxy-server (format "%s@%s:%s" proxy-user proxy-host proxy-port)))
                       (setq term-argv
                             `("-p"
                               ,password
                               ,@argv
                               "ssh"
                               ,remote-server
                               "-p"
                               ,port
                               "-J"
                               ,proxy-server))
                       )
                   (setq term-argv
                         (list "-p"
                               password
                               "ssh"
                               (format "%s@%s" username host)
                               "-p"
                               port
                               "-J"
                               (format "%s@%s:%s" proxy-user proxy-host proxy-port))))
                 (set-buffer (apply 'make-term term-name
                                    "sshpass"
                                    nil
                                    term-argv))
                 (docker--init-term-mode term-name)
                 ))))
          ((string= kind "direct")
           (let* ((argv '())
                  (term-name (format "%s<%s>" session-name index)))
             (if (not (string-empty-p password))
                 (setq argv (append argv `("-p" ,password))))
             (if (not (string-empty-p totp-key))
                 (setq argv (append argv `("-o" ,totp-key))))
             (if (not (string-empty-p totp-message))
                 (setq argv (append argv `("-O" ,totp-message))))
             (if (string-empty-p host)
                 (dotfairy--ssh-error "SSH hostname must be set. it's cannot empty.")
               (setq argv (append argv `("ssh" ,host)))
               (if (not (string-empty-p username))
                   (setq argv (append argv `("-l" ,username))))
               (if (not (string-empty-p port))
                   (progn
                     (setq argv (append argv `("-p" ,port)))
                     (set-buffer (apply 'make-term term-name
                                        "sshpass"
                                        nil
                                        argv))
                     (docker--init-term-mode term-name))))
             )))))


(defun term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    (dotfairy--ssh-info "remove %s from server groups." (buffer-name (current-buffer)))
    (dotfairy--remove-buffer-name-from-groups (buffer-name (current-buffer)))
    (switch-to-buffer "*scratch*")))

(defun dotfairy--send-cmd-to-buffer (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "*terminal*"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (cond ((derived-mode-p 'comint-mode)
                 (insert input)
                 (comint-send-input))
                ((derived-mode-p 'term-mode)
                 (term-send-string process input)
                 (term-send-input))))))))

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
      )))


(defun dotfairy--read-session-config-from-minibuffer (kind &optional ssh-session-config)
  "Read session config from minibuffer."
  (let* ((ssh-session '())
         (session-name (read-string "Session Name: " (if (not (equal ssh-session-config nil))
                                                         (plist-get ssh-session-config :session-name))
                                    )))
    (if (string-empty-p session-name)
        (dotfairy--ssh-error "session name cannot empty.")
      (setq ssh-session (plist-put ssh-session :session-name session-name))
      (setq ssh-session (plist-put ssh-session :kind kind))
      (if (string= kind "proxy")
          (let* ((proxy-host (read-string "Proxy hostname: " (if (not (equal ssh-session-config nil))
                                                                 (plist-get ssh-session-config :proxy-host))))
                 (proxy-port (read-string "Proxy port(22): " (if (not (equal ssh-session-config nil))
                                                                 (plist-get ssh-session-config :proxy-port))))
                 (proxy-user (read-string "Proxy username(root): " (if (not (equal ssh-session-config nil))
                                                                       (plist-get ssh-session-config :proxy-user)))))
            (setq ssh-session (plist-put ssh-session :proxy-host proxy-host))
            (setq ssh-session (plist-put ssh-session :proxy-port (if (string-empty-p proxy-port)
                                                                     "22"
                                                                   proxy-port)))
            (setq ssh-session (plist-put ssh-session :proxy-user (if (string-empty-p proxy-user)
                                                                     "root"
                                                                   proxy-user)))
            ))
      (let* ((remote-host (read-string "Remote hostname: " (if (not (equal ssh-session-config nil))
                                                               (plist-get ssh-session-config :remote-host))))
             (remote-port (read-string "Remote hostport(22): " (if (not (equal ssh-session-config nil))
                                                                   (plist-get ssh-session-config :remote-port))))
             (remote-user (read-string "Remote username(root): " (if (not (equal ssh-session-config nil))
                                                                     (plist-get ssh-session-config :remote-user))))
             (remote-password (read-passwd "Remote password: "))
             (totp-key (read-string "2FA(TOTP) key: " (if (not (equal ssh-session-config nil))
                                                          (plist-get ssh-session-config :totp-key))))
             (totp-message (read-string "2FA(TOTP) message: " (if (not (equal ssh-session-config nil))
                                                                  (plist-get ssh-session-config :totp-message)))))
        (setq ssh-session (plist-put ssh-session :remote-host remote-host))
        (setq ssh-session (plist-put ssh-session :remote-port (if (string-empty-p remote-port)
                                                                  "22"
                                                                remote-port)))
        (setq ssh-session (plist-put ssh-session :remote-user (if (string-empty-p remote-user)
                                                                  "root"
                                                                remote-user)))
        (setq ssh-session (plist-put ssh-session :remote-password (if (string-empty-p remote-password)
                                                                      (let ((passwd (plist-get ssh-session-config :remote-password)))
                                                                        (if (string-empty-p passwd)
                                                                            (dotfairy--ssh-warn "remote connect password is empty.")
                                                                          passwd))
                                                                    remote-password)))
        (setq ssh-session (plist-put ssh-session :totp-key totp-key))
        (setq ssh-session (plist-put ssh-session :totp-message totp-message))
        ))
    ssh-session)
  )

(defun dotfairy/create-ssh-remote (kind)
  "docstring"
  (interactive (list (completing-read "Select connect style: " '(proxy direct))))
  (let* ((ssh-session (dotfairy--read-session-config-from-minibuffer
                       (completing-read "Select connect style: " '(proxy direct)))))
    (cl-pushnew ssh-session
                (dotfairy-ssh-session-servers (dotfairy-ssh-session)) :test 'equal)
    (dotfairy--ssh-persist-session (dotfairy-ssh-session))
    (cond ((string= (plist-get ssh-session :kind) "proxy")
           (if (or (string-empty-p (plist-get ssh-session :proxy-host))
                   (string-empty-p (plist-get ssh-session :remote-host)))
               (dotfairy--ssh-error "<Proxy host> and <Remote host> must be set. please check its.")
             (dotfairy-connect-ssh ssh-session)))
          ((string= (plist-get ssh-session :kind) "direct")
           (if (string-empty-p (plist-get ssh-session :remote-host))
               (dotfairy--ssh-error "<Remote host> must be set. it's cannot empty.")
             (dotfairy-connect-ssh ssh-session)))))
  )


(defun dotfairy/edit-ssh-session-config (session)
  "Edit ssh session config."
  (interactive (list (completing-read "Select server to edit: "
                                      (dotfairy--filter-ssh-session)
                                      )))

  (dolist (server (->> (dotfairy-ssh-session)
                       (dotfairy-ssh-session-servers)))
    (if (string= session (plist-get server :session-name))
        (let* ((let-sessions (dotfairy-ssh-session)))
          (cl-pushnew (dotfairy--read-session-config-from-minibuffer (completing-read "Select connect style: " '(proxy direct))
                                                                     server)
                      (dotfairy-ssh-session-servers (dotfairy-ssh-session)) :test 'equal)
          (setf (dotfairy-ssh-session-servers let-sessions)
                (-remove-item server (dotfairy-ssh-session-servers let-sessions)))
          )
      ))
  )

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


(defun install-ssh-manager-tools ()
  "Install SSH manager tools"
  (interactive)
  (if (not (executable-find "sshpass"))
      (dotfairy-exec-process "sh" "-c" (concat
                                        "rm -rf /tmp/sshpass &&"
                                        " git"
                                        " clone"
                                        " https://github.com/dora38/sshpass"
                                        " /tmp/sshpass"
                                        " &&"
                                        " cd /tmp/sshpass"
                                        " &&"
                                        " ./bootstrap"
                                        " &&"
                                        " ./configure --prefix=/usr/local "
                                        "&&"
                                        " make install; cd -")))
  (if (not (executable-find "oathtool"))
      (dotfairy--ssh-info "your need install oathtool if used 2FA."))
  (dotfairy--ssh-info "installed."))

(provide 'init-ssh)
;;; init-ssh.el ends here
