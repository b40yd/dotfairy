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
(require 'dired)
(require 'subr-x)

;; ssh-manager-mode
(cl-defstruct ssh-manager-session-groups
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defvar ssh-manager--session-groups nil
  "Contain the `ssh-manager-session-groups' for the current Emacs instance.")

(defun ssh-manager-session-groups ()
  (or ssh-manager--session-groups (setq ssh-manager--session-groups (make-ssh-manager-session-groups))))

(defun ssh-manager-show-ssh-session-groups ()
  "Show ssh server groups."
  (interactive)
  (message (format "%s" (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))

(defun ssh-manager-add-this-ssh-session-to-groups ()
  "Add this ssh server session to groups."
  (interactive)
  (cl-pushnew (buffer-name) (ssh-manager-session-groups-servers (ssh-manager-session-groups)) :test 'equal))

(defun ssh-manager-remove-this-ssh-session-from-groups ()
  "Remove this ssh server session from groups."
  (interactive)
  (ssh-manager--remove-buffer-name-from-groups (buffer-name)))

(defun ssh-manager--remove-buffer-name-from-groups (buf-name)
  "Remove buffer name from groups."

  (setf (ssh-manager-session-groups-servers (ssh-manager-session-groups))
        (-remove-item buf-name (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))

(defun ssh-manager-remove-ssh-session-from-groups (session)
  "Remove ssh server session from groups."
  (interactive  (list (completing-read "Select server to connect: "
                                       (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))
  (ssh-manager--remove-buffer-name-from-groups session))

(defun ssh-manager-send-cmd-to-session-groups (cmd)
  (let ((current-buf (current-buffer)))
    (dolist (server (->> (ssh-manager-session-groups)
                         (ssh-manager-session-groups-servers)))
      (ssh-manager--send-cmd-to-buffer server cmd))
    (switch-to-buffer current-buf)))

(defvar ssh-manager-mode-map nil "keymap for `ssh-manager-mode'")

(setq ssh-manager-mode-map (make-sparse-keymap))

(define-key ssh-manager-mode-map (kbd "C-c C-c") 'ssh-manager-execute-buffer-cmd-to-ssh)
(define-key ssh-manager-mode-map (kbd "C-c C-e") 'ssh-manager-execute-region-cmd-to-ssh)
(define-key ssh-manager-mode-map (kbd "C-c C-.") 'ssh-manager-execute-current-line-cmd-to-ssh)

(define-derived-mode ssh-manager-mode prog-mode "SSH Manager Mode"
  (with-no-warnings
    (font-lock-fontify-buffer))
  (use-local-map ssh-manager-mode-map))

(defun ssh-manager-execute-current-line-cmd-to-ssh ()
  "Execute command to ssh server groups."
  (interactive)
  (let ((line (ssh-manager-read-current-line-cmd)))
    (ssh-manager-send-cmd-to-session-groups line)
    (reindent-then-newline-and-indent)))

(defun ssh-manager-read-current-line-cmd ()
  "Read current line command."
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ssh-manager-execute-region-cmd-to-ssh ()
  "Execute region cmd to ssh"
  (interactive)
  (let ((begin (region-beginning))
        (end (region-end)))
    (ssh-manager-send-cmd-to-session-groups (buffer-substring begin end))))

(defun ssh-manager-execute-buffer-cmd-to-ssh ()
  "Execute buffer cmd to ssh"
  (interactive)
  (ssh-manager-send-cmd-to-session-groups (buffer-substring-no-properties (point-min) (point-max))))

(defun ssh-manager ()
  (interactive)
  (let ((buffer (generate-new-buffer "*SSH Manager*")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)
    (funcall 'ssh-manager-mode)
    (setq buffer-offer-save t)))

;; ssh connect session
(cl-defstruct ssh-manager-session
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defcustom ssh-manager-session-file (expand-file-name (locate-user-emacs-file ".ssh-manager-session-v1"))
  "File where session information is stored."
  :group 'ssh-manager
  :type 'file)

(defvar ssh-manager--session nil
  "Contain the `ssh-manager-session' for the current Emacs instance.")
(defvar ssh-manager--show-message t
  "If non-nil, show debug message from `ssh-manager'.")

(defun ssh-manager--message  (format &rest args)
  "Wrapper for `message'
We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `ssh-manager--info', `ssh-manager--warn' and `ssh-manager--error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.
See #2049"
  (when ssh-manager--show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

(defun ssh-manager--info (format &rest args)
  "Display ssh-manager info message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'success) (apply #'format format args)))

(defun ssh-manager--warn (format &rest args)
  "Display ssh-manager warn message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'warning) (apply #'format format args)))

(defun ssh-manager--error (format &rest args)
  "Display ssh-manager error message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'error) (apply #'format format args)))
(defun ssh-manager--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun ssh-manager--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME.
This function creates the parent directories if they don't exist
yet."
  (let ((print-length nil)
        (print-level nil))
    ;; Create all parent directories:
    (apply #'f-mkdir (f-split (f-parent file-name)))
    (f-write-text (prin1-to-string to-persist) 'utf-8 file-name)))

(defun ssh-manager--persist-session (session)
  "Persist SESSION to `ssh-manager-session-file'."
  (ssh-manager--persist ssh-manager-session-file (make-ssh-manager-session
                                                  :servers (ssh-manager-session-servers session))))
(defun ssh-manager--load-default-session ()
  "Load default session."
  (setq ssh-manager--session (or (condition-case err
                                     (ssh-manager--read-from-file ssh-manager-session-file)
                                   (error (ssh-manager--error "Failed to parse the session %s, starting with clean one."
                                                              (error-message-string err))
                                          nil))
                                 (make-ssh-manager-session))))

(defun ssh-manager-session ()
  "Get the session associated with the current buffer."
  (or ssh-manager--session (setq ssh-manager--session (ssh-manager--load-default-session))))

(defun ssh-manager--term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun ssh-manager--init-term-mode (term-name)
  "Init term mode"
  (progn
    (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
    (define-key term-raw-map (kbd "C-c C-b") 'switch-to-buffer)
    (define-key term-raw-map (kbd "C-c C-a") 'ssh-manager-add-this-ssh-session-to-groups)
    (define-key term-raw-map (kbd "C-c C-r") 'ssh-manager-remove-this-ssh-session-from-groups)
    (define-key term-raw-map (kbd "C-c M-a") 'ssh-manager-show-ssh-session-groups)
    (term-mode)
    (term-char-mode)
    (ssh-manager--term-handle-close)
    (add-hook 'kill-buffer-hook 'ssh-managerterm-kill-buffer-hook)
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
(defun ssh-manager-connect-ssh (server)
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
                       (string-trim (buffer-string)))))
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
               (ssh-manager--error "<Proxy host> and <Remote host> must be set. please check its.")
             (progn
               (let* ((argv '())
                      (term-argv '())
                      (term-name (format "%s<%s>" session-name index)))
                 (if (not (string-empty-p totp-key))
                     (setq argv (append argv `("-o" ,totp-key))))
                 (if (not (string-empty-p totp-message))
                     (setq argv (append argv `("-O" ,totp-message))))
                 (if (>= (length argv) 0)
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
                               ,proxy-server)))
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
                 (ssh-manager--init-term-mode term-name)))))
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
                 (ssh-manager--error "SSH hostname must be set. it's cannot empty.")
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
                     (ssh-manager--init-term-mode term-name)))))))))


(defun ssh-managerterm-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    (ssh-manager--info "removed %s from server groups." (buffer-name (current-buffer)))
    (ssh-manager--remove-buffer-name-from-groups (buffer-name (current-buffer)))
    ;; (switch-to-buffer "*scratch*")
    ))

(defun ssh-manager--send-cmd-to-buffer (&optional buffer string)
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

(defun ssh-manager--filter-ssh-session ()
  "filter ssh session name list."
  (let ((lst '()))
    (dolist (server (->> (ssh-manager-session)
                         (ssh-manager-session-servers)))
      (setq lst (append lst (list (plist-get server :session-name)))))
    lst))

(defun ssh-manager-switch-to-server (session)
  "Select ssh server to connect."
  (interactive (list (completing-read "Select server to connect: "
                                      (ssh-manager--filter-ssh-session))))
  (dolist (server (->> (ssh-manager-session)
                       (ssh-manager-session-servers)))
    (if (equal (plist-get server :session-name) session)
        (ssh-manager-connect-ssh server))))


(defun ssh-manager--read-session-config-from-minibuffer (kind &optional ssh-session-config)
  "Read session config from minibuffer."
  (let* ((ssh-session '())
         (session-name (read-string "Session Name: " (if (not (equal ssh-session-config nil))
                                                         (plist-get ssh-session-config :session-name)))))
    (if (string-empty-p session-name)
        (ssh-manager--error "session name cannot empty.")
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
                                                                   proxy-user)))))
      (let* ((remote-host (read-string "Remote hostname: " (if (not (equal ssh-session-config nil))
                                                               (plist-get ssh-session-config :remote-host))))
             (remote-port (read-string "Remote hostport(22): " (if (not (equal ssh-session-config nil))
                                                                   (plist-get ssh-session-config :remote-port))))
             (remote-user (read-string "Remote username(root): " (if (not (equal ssh-session-config nil))
                                                                     (plist-get ssh-session-config :remote-user))))
             (remote-password (read-passwd "Remote password: "))
             (totp-key (read-passwd "2FA(TOTP) key: " (if (not (equal ssh-session-config nil))
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
                                                                            (ssh-manager--warn "remote connect password is empty.")
                                                                          passwd))
                                                                    remote-password)))
        (setq ssh-session (plist-put ssh-session :totp-key totp-key))
        (setq ssh-session (plist-put ssh-session :totp-message totp-message))))
    ssh-session))

(defun ssh-manager-create-ssh-remote (kind)
  "docstring"
  (interactive (list (completing-read "Select connect style: " '(proxy direct))))
  (let* ((ssh-session (ssh-manager--read-session-config-from-minibuffer kind)))
    (cl-pushnew ssh-session
                (ssh-manager-session-servers (ssh-manager-session)) :test 'equal)
    (ssh-manager--persist-session (ssh-manager-session))
    (cond ((string= (plist-get ssh-session :kind) "proxy")
           (if (or (string-empty-p (plist-get ssh-session :proxy-host))
                   (string-empty-p (plist-get ssh-session :remote-host)))
               (ssh-manager--error "<Proxy host> and <Remote host> must be set. please check its.")
             (ssh-manager-connect-ssh ssh-session)))
          ((string= (plist-get ssh-session :kind) "direct")
           (if (string-empty-p (plist-get ssh-session :remote-host))
               (ssh-manager--error "<Remote host> must be set. it's cannot empty.")
             (ssh-manager-connect-ssh ssh-session))))))


(defun ssh-manager-edit-ssh-session-config (session)
  "Edit ssh session config."
  (interactive (list (completing-read "Select server to edit: "
                                      (ssh-manager--filter-ssh-session))))

  (dolist (server (->> (ssh-manager-session)
                       (ssh-manager-session-servers)))
    (if (string= session (plist-get server :session-name))
        (let* ((let-sessions (ssh-manager-session)))
          (cl-pushnew (ssh-manager--read-session-config-from-minibuffer (completing-read "Select connect style: " '(proxy direct))
                                                                        server)
                      (ssh-manager-session-servers (ssh-manager-session)) :test 'equal)
          (setf (ssh-manager-session-servers let-sessions)
                (-remove-item server (ssh-manager-session-servers let-sessions)))))))

(defun ssh-manager-remove-ssh-server (session)
  "Remove session from the list of servers."
  (interactive (list (completing-read "Select server to connect: "
                                      (ssh-manager--filter-ssh-session))))
  (dolist (server (->> (ssh-manager-session)
                       (ssh-manager-session-servers)))
    (if (string= session (plist-get server :session-name))
        (let* ((let-sessions (ssh-manager-session)))
          (setf (ssh-manager-session-servers let-sessions)
                (-remove-item server (ssh-manager-session-servers let-sessions)))
          (ssh-manager--persist-session (ssh-manager-session))))))


(defun ssh-manager-install-tools ()
  "Install SSH manager tools"
  (interactive)
  (if (not (executable-find "sshpass"))
      (ssh-manager-exec-process "sh" "-c" (concat
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
      (ssh-manager--info "your need install oathtool if used 2FA."))
  (ssh-manager--info "installed."))

;; ssh connect session
(cl-defstruct ssh-manager-remote-history
  ;; remote  dir or file history.
  folders
  (metadata (make-hash-table :test 'equal)))

(defvar ssh-manager--remote-history nil
  "Contain the `ssh-manager-session-groups' for the current Emacs instance.")

(defun ssh-manager-remote-history ()
  (or ssh-manager--remote-history (setq ssh-manager--remote-history (make-ssh-manager-remote-history))))

(defun ssh-manager--upload-or-download-files-to-remote-host (server method)
  (let ((argv '())
        (password (plist-get server :remote-password))
        (totp-key (if (string-empty-p (plist-get server :totp-key))
                      ""
                    (with-temp-buffer
                      (or (apply #'call-process "oathtool" nil t nil (list "--totp" "-b" (plist-get server :totp-key)))
                          "")
                      (string-trim (buffer-string)))))
        (totp-message (plist-get server :totp-message))
        (proxy-host (plist-get server :proxy-host))
        (proxy-port (plist-get server :proxy-port))
        (proxy-user (plist-get server :proxy-user))
        (host (plist-get server :remote-host))
        (port (plist-get server :remote-port))
        (user (plist-get server :remote-user)))
    (progn
      (if (not (string-empty-p password))
          (setq argv (append argv `("-p" ,password))))
      (if (not (string-empty-p totp-key))
          (setq argv (append argv `("-o" ,totp-key))))
      (if (not (string-empty-p totp-message))
          (setq argv (append argv `("-O" ,totp-message))))
      (if (string-empty-p host)
          (ssh-manager--error "SSH hostname must be set. it's cannot empty.")
        (if (executable-find "scp")
            (progn
              (setq argv (append argv `("scp" "-r")))
              (if (and (not (string= proxy-host nil))
                       (not (string= proxy-user nil))
                       (not (string= proxy-port nil)))
                  (setq argv (append argv `("-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port)))))
              (if (not (string-empty-p port))
                  (setq argv (append argv `("-P" ,port))))
              (if (and (not (string-empty-p host))
                       (not (string-empty-p user)))
                  (let* ((remote-dir-or-file (completing-read (format "Remote dir or file (/home/%s): " user)
                                                              (ssh-manager-remote-history-folders (ssh-manager-remote-history))
                                                              nil nil))
                         (files (dired-get-marked-files)))
                    (if (string-empty-p remote-dir-or-file)
                        (setq remote-dir-or-file (format "/home/%s" user)))
                    (cl-pushnew remote-dir-or-file (ssh-manager-remote-history-folders (ssh-manager-remote-history)) :test 'equal)
                    (cond ((string= method "upload")
                           (if (= (length files) 0)
                               (when-let ((ask (downcase (read-string "upload current buffer file(y-or-n):"))))
                                 (if (or (string= ask "y")
                                         (string= ask "yes"))
                                     (setq argv (append argv `(,(buffer-file-name) ,(format "%s@%s:%s" user host remote-dir-or-file))))))
                             (if (not (string-empty-p remote-dir-or-file))
                                 (setq argv (append argv `(,@files ,(format "%s@%s:%s" user host remote-dir-or-file)))))))
                          ((string= method "download")
                           (if (derived-mode-p 'dired-mode)
                               (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,(dired-current-directory))))
                             (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,default-directory))))))))))))))

;;;###autoload
(defun ssh-manager-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.
Unlike `ssh-manager-call-process', this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell, so batch scripts could run external programs
synchronously without sacrificing their output.
Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "ssh-manager"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defun ssh-manager-upload-or-download-files-to-remote-host (method)
  "SCP files send to remote host"
  (interactive (list (completing-read "Select upload or download: "
                                      '(upload download))))
  (let ((session-name (completing-read "Select server to connect: "
                                       (ssh-manager--filter-ssh-session))))
    (dolist (session (->> (ssh-manager-session)
                          (ssh-manager-session-servers)))
      (if (string= session-name (plist-get session :session-name))
          (if-let ((argv (ssh-manager--upload-or-download-files-to-remote-host session method)))
              (apply 'ssh-manager-exec-process "sshpass" argv))))
    (if (derived-mode-p 'dired-mode)
        (cond ((string= method "download")
               (revert-buffer))
              ((string= method "upload")
               (dired-unmark-all-marks))))))

(provide 'init-ssh)
;;; init-ssh.el ends here
