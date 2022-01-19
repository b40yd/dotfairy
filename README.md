# Dotfairy

![Linux](https://img.shields.io/badge/-Linux-blue?logo=Linux&style=flat&logoColor=white)
![Apple](https://img.shields.io/badge/-MacOS-blue?logo=apple&style=flat&logoColor=white)
![Windows](https://img.shields.io/badge/-Windows-blue?logo=windows&style=flat&logoColor=white)

Dotfairy is simple configure emacs.d, It's work on GNU/Linux, MacOS and Windows 7 later(Some component functions may not work properly).

## Feature

- Clean and Fast.
- Quick fuzzy search.
- Better Org/Markdown/Hugo support.
- Support multiple programming languages

  - C/C++/Java/Python/Shell/Golang/Rust
  - JavaScript/Typescript/JSON/YAML
  - HTML/CSS/XML/React

- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git integration.
- Project/Workspace integration.
- Pomodor integration.
- Support Docker/K8s.
- Support SSH manager.
- Support Pass manager password.
- Better Chinese support:
  - Chinese calendar
  - Youdao/Fanyi

## Usage

## Introduction

Please read [blog](https://scanbuf.net/post/manual/how-do-myself-custom-editor/).

## Install
Now, You can download [denv](https://github.com/7ym0n/denv) scripts. use it install environment.

```shell
mv ~/.emacs.d{,.backup} && git clone https://github.com/7ym0n/dotfairy.git ~/.emacs.d

# install base tools and program develop environment.
git clone https://github.com/7ym0n/denv
cd denv && bash denv --install --sudo
```

## Manual

Add or change the configurations in `~/dotfairy.d/custom.el`, then restart Emacs.

```elisp
(setq dotfairy-full-name "7ym0n")           ; User full name
(setq dotfairy-mail-address "bb.qnyd@gmail.com")   ; Email address
(setq dotfairy-proxy "127.0.0.1:7890")          ; Network proxy
(setq dotfairy-server nil)                      ; Enable `server-mode' or not: t or nil
(setq dotfairy-package-archives 'melpa)   ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
;; Color theme:
;; dotfairy-theme-list
;; '((default . doom-one)
;;   (doom-one . doom-one)
;;   (doom-monokai-pro     . doom-monokai-pro)
;;   (doom-dark+    . doom-dark+)
;;   (doom-one-light   . doom-one-light)
;;   (doom-solarized-light    . doom-solarized-light)
;;   (doom-city-lights    . doom-city-lights)
;;   (doom-tomorrow-day    . doom-tomorrow-day)
;;   (doom-tomorrow-night   . doom-tomorrow-night))
(setq dotfairy-theme 'default)
(setq dotfairy-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq dotfairy-restore-frame-geometry nil)      ; Restore the frame's geometry at startup: t or nil
(setq dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode go-mode)) ; Ignore format on save for some languages
(setq dotfairy-company-prescient nil) ; Enable `company-prescient' or not. it's on Windows 10 very slow.
;; confirm exit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Fonts
(defun dotfairy-set-fonts ()
;; Set default font
  (cl-loop for font in '("Source Code Pro" "SF Mono" "Hack" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (IS-MAC 180)
                                                    (IS-WINDOWS 110)
                                                    (t 130))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("PowerlineSymbols" "Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Zen Hei Mono" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (dotfairy-set-fonts)))
  (when (display-graphic-p)
    (dotfairy-set-fonts)))

;; default workspace
(setq default-directory "~/")
;; .authinfo
;; machine git.scanbuf.net/api/v4 login <your_git_user>^forge password <your_git_auth_token>
;;
(with-eval-after-load 'forge
  (push '("git.scanbuf.net" "git.scanbuf.net/api/v4" "git.scanbuf.net" forge-github-repository) forge-alist)
  )
```

## Screenshot

![dashboard](screenshots/dashboard.png)
![mgit](screenshots/mgit.png)
![git-log](screenshots/gitlog.png)
![org](screenshots/org.png)
![projects](screenshots/projects.png)
![shell-pop](screenshots/shell-pop.png)

## Configure Keymap

See [site-lisp/custom-keybinds.el](site-lisp/custom-keybinds.el).
