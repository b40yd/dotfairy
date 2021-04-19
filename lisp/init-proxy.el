;;; init-proxy.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-custom)
(defvar socks-noproxy)
(defvar socks-server)

;; Network Proxy
(defun dotfairy/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" dotfairy-proxy)
    (message "No HTTP proxy")))

(defun dotfairy/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,dotfairy-proxy)
          ("https" . ,dotfairy-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (dotfairy/proxy-http-show))

(defun dotfairy/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (dotfairy/proxy-http-show))

(defun dotfairy/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (dotfairy/proxy-http-disable)
    (dotfairy/proxy-http-enable)))

(defun dotfairy/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun dotfairy/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (let* ((proxy (split-string dotfairy-proxy "\\s-*:\\s-*"))
         (addr (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq url-gateway-method 'socks
          socks-noproxy '("localhost")
          socks-server `("Default server" ,addr ,port 5)))
  (setenv "all_proxy" (concat "socks5://" dotfairy-proxy))
  (dotfairy/proxy-socks-show))

(defun dotfairy/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (setenv "all_proxy" "")
  (dotfairy/proxy-socks-show))

(defun dotfairy/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (dotfairy/proxy-socks-disable)
    (dotfairy/proxy-socks-enable)))

(provide 'init-proxy)
;;; init-proxy.el ends here
