#!/bin/bash
#
# Author: 7ym0n.q7e
# Email: bb.qnyd@gmail.com
# Date: 2020-08-17
#
BASEPATH=$(cd `dirname $0`; pwd)
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

#### Useful Color constants settings for loglevels

# Reset Color (for syslog)
NC='\033[0m'
WHITE='\033[0m'
# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
GRAY='\033[0;40m'        # Gray

# Bold
BRED='\033[1;31m'         # Red
BGREEN='\033[1;32m'       # Green
BYELLOW='\033[1;33m'      # Yellow
BWHITE='\033[1;37m'       # White
LOGLEVEL=$LOGLEVEL:-1


#
# Logging functions
#
case $LOGLEVEL in
    error )
        MACHINE_LOG_LEVEL=1
        ;;
    warning )
        MACHINE_LOG_LEVEL=2
        ;;
    ok )
        MACHINE_LOG_LEVEL=3
        ;;
    info )
        MACHINE_LOG_LEVEL=4
        ;;
    debug )
        MACHINE_LOG_LEVEL=5
        ;;
    *)
        MACHINE_LOG_LEVEL=4 ## Default loglevel value to info
esac

_logger() {
    COLOR=$1
    shift
    test -z "$SCRIPT_NAME" && SCRIPT_NAME=$(basename $0)
    builtin echo "$*" | /usr/bin/logger -t "[FAIRY] $SCRIPT_NAME" -p "user.info"
    SCRIPT_NAME_FIXEDLEN=$(printf "%-25.25s" "$SCRIPT_NAME")
    cecho $COLOR "$SCRIPT_NAME_FIXEDLEN $*"
}

cecho () {
    COLOR=$1
    shift
    builtin echo -e "${COLOR}$*${NC}"
}

crit () {
    if [ $MACHINE_LOG_LEVEL -ge 1 ]; then _logger $BRED "[ KO ] $*"; fi
    # This variable incrementation is used to measure failure or success in tests
    CRITICAL_ERRORS_NUMBER=$((CRITICAL_ERRORS_NUMBER+1))
}

no_entity() {
    if [ $MACHINE_LOG_LEVEL -ge 1 ]; then _logger $BGREEN "[ none entity, so it's not scored ] $*"; fi
    # This variable incrementation is used to measure whether the service exists in tests
    NONEXISTENT_NUMBER=$((NONEXISTENT_NUMBER+1))
}

warn () {
    if [ $MACHINE_LOG_LEVEL -ge 2 ]; then _logger $BYELLOW "[WARN] $*"; fi
}

ok () {
    if [ $MACHINE_LOG_LEVEL -ge 3 ]; then _logger $BGREEN "[ OK ] $*"; fi
}

info () {
    if [ $MACHINE_LOG_LEVEL -ge 4 ]; then _logger ''      "[INFO] $*"; fi
}

debug () {
    if [ $MACHINE_LOG_LEVEL -ge 5 ]; then _logger $GRAY "[DBG ] $*"; fi
}

install_package=${install_package:-""}

if [ -f /etc/os-release ];then
    osinfo=$(cat /etc/os-release | grep -E -i "CentOS|rhel|fedora")
    if [ ! -z $osinfo ];then
        install_package="yum install -y"
        return
    fi

    osinfo=$(cat /etc/os-release | grep -E -i "ubuntu|debian|mint")
    if [ ! -z $osinfo ];then
        install_package="apt-get install -y"
        return
    fi

    osinfo=$(cat /etc/os-release | grep -E -i "archlinux|manjaro")
    if [ ! -z $osinfo ];then
        install_package="pacmans -S --noconfirm"
        return
    fi
fi

install-gopls() {
    if type go &> /dev/null;then
        go get golang.org/x/tools/gopls@latest
    else
        install_package golang
        [ $? -ne 0 ] && crit "please install golang env." && exit 1
    fi
}

install-nodejs() {
    if type go &> /dev/null;then
        go get golang.org/x/tools/gopls@latest
    else
        install_package nodejs
        [ $? -ne 0 ] && crit "please install nodejs env." && exit 1
    fi
}

install-bash-server() {
    if type npm &> /dev/null;then
        npm install -g bash-language-server
    fi
}

install-typescript-server() {
    if type npm &> /dev/null;then
        npm install -g typescript-language-server
    fi
}

install-json-server() {
    if type npm &> /dev/null;then
        npm install -g vscode-json-languageserver
    fi
}

install-css-server() {
    if type npm &> /dev/null;then
        npm install -g vscode-css-languageserver-bin
    fi
}

install-html-server() {
    if type npm &> /dev/null;then
        npm install -g vscode-html-languageserver-bin
    fi

}

install-python-server() {
    if type pip &> /dev/null;then
        pip3 install autopep8 python-language-server pyls-mypy pyls-isort pylint pyflakes pyls-black -i https://mirrors.aliyun.com/pypi/simple/
    else
        info "please install pip tools." && exit 1
    fi
}

install-rustup() {
    if type rustup &> /dev/null;then
        install_package rustup && setup-rustup-toolchain
    fi
}

setup-rustup-toolchain() {
    export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup
    export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
    rustup toolchain add nightly && \
    rustup component add rust-src && \
    cargo +nightly install racer && \
    install_package rust-analyzer
}

install-clang() {
    install_package ccls
}

install-all() {
    install-python-server
    install-gopls
    install-nodejs
    install-typescript-server
    install-json-server
    install-css-server
    install-html-server
    install-rustup
    install-clang
}

install-fonts() {
    info "installing Chinese and Ubuntu mono fonts"
    cp -rfv $BASEPATH/fonts/*.ttf /usr/share/fonts/ && mkfontscale && mkfontdir && fc-cache -f -v

    info "installing all-the-icons fonts"
    warn "M-x all-the-icons-install-fonts"
}

init-config-dir() {
    local user=$1
    info "mkdir -pv $HOME/$user/.emacs.d/.local/{elpa,etc/workspaces,org}"
    mkdir -pv ~/.emacs.d/.local/{elpa,etc/workspaces,org}
}

if [ `id -u` -eq 0 ];then
    install-all
    install-fonts
    init-config-dir $1
else
    crit "please use root." && exit 1
fi
