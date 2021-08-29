# dotfairy

dotfairy is emacs.d



## UI

- [all-the-icons](https://github.com/domtronn/all-the-icons.el)
- [all-the-icons-ibuffer](https://github.com/seagle0128/all-the-icons-ibuffer)
- [icons-in-terminal.el](https://github.com/seagle0128/icons-in-terminal.el) - [icons-in-terminal](https://github.com/sebastiencs/icons-in-terminal)

## Programming Language

- `Golang`
auto-complete server:
```shell
go get golang.org/x/tools/gopls
go get -u github.com/cweill/gotests/...
go get -u github.com/davidrjenni/reftools/cmd/fillstruct
go install github.com/go-delve/delve/cmd/dlv@latest
go get github.com/fatih/gomodifytags
```

- `Python`
auto-complete server:
```shell
pip3 install trepan3k autopep8 \
    python-language-server pyls-mypy \
    pyls-isort pyls-black jupyter mccabe \
    pydocstyle pyflakes pylint rope \
    flake8-mypy ptvsd -i https://mirrors.aliyun.com/pypi/simple/
```
- `SQL`
sqlformat tools, need install sqlparse.
```shell
pip3 install sqlparse
```
- `BASH`
auto-complete server:
```shell
npm install -g bash-language-server
```

- `JavaScript/TypeScript`
auto-complete server:
```shell
npm i -g typescript-language-server # or
npm i -g typescript
```

- `JSON`
auto-complete server:
```shell
npm i -g vscode-json-languageserver
```

- `CSS/Less/SASS/SCSS`
auto-complete server:
```shell
npm install -g vscode-css-languageserver-bin
```
- HTML
auto-complete server:
```shell
npm install -g vscode-html-languageserver-bin
```

- Clang
auto-complete server:

```
install-package ccls
```

- Rust
auto-complete server:

```
install-package rustup rust-analyzer
```
- SSH Manager
need install `sshpass` and `oathtool`.
`M-x`: `install-ssh-manager-tools`
MacOS:
```shell
brew install oath-toolkit
```
ArchLinux:
```shell
sudo pacman -Syy oath-toolkit
```

## Usage
please read [blog](https://scanbuf.net/post/manual/how-do-myself-custom-editor/)。

## keymap
   see [site-lisp/custom-keybinds.el](site-lisp/custom-keybinds.el).
