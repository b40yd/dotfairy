;;; env-example.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 b40yd
;;
;; Author: b40yd <bb.qnyd@gmail.com>
;; Maintainer: b40yd <bb.qnyd@gmail.com>
;; Created: January 14, 2026
;; Modified: January 14, 2026
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/qing.bao/env-example
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;       Environment variables.
;;
;;  Description
;;
;;; Code:


;; For LSP performance
;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;; To avoid loading `exec-path-from-shell' for better performance
;; (setenv "EMACS_PLUS_PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin")

;;; env-example.el ends here
