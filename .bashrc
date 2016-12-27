# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
# HISTSIZE=1000
# HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#-------------------------------------------------------------------------------
# OS detection
#-------------------------------------------------------------------------------
# Source: http://stackoverflow.com/questions/394230/detect-the-os-from-a-bash-script/8597411#8597411
PLATFORM='unknown'
# Linux
if   [[ "$OSTYPE" == "linux-gnu" ]]; then
    PLATFORM='linux'
# Mac OSX
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM='osx'
    # POSIX compatibility layer and Linux environment emulation for Windows
elif [[ "$OSTYPE" == "cygwin" ]]; then
    PLATFORM='win'
    # Lightweight shell and GNU utilities compiled for Windows (part of MinGW)
elif [[ "$OSTYPE" == "msys" ]]; then
    PLATFORM='win'
    # I'm not sure this can happen.
elif [[ "$OSTYPE" == "win32" ]]; then
    PLATFORM='win'
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    PLATFORM='freebsd'
fi


#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------

export PATH=$PATH:$HOME/code/Scripts
export PATH=$PATH:$HOME/Applications

if [[ ${PLATFORM} == "osx" ]]; then
    export PATH=$HOME/local/bin:$PATH
    export PATH=$PATH:$HOME/.gem/ruby/2.0.0/bin
    export EDITOR=$HOME/local/bin/emacs
elif [[ ${PLATFORM} == "linux" ]]; then
    export CC=/usr/bin/clang
    export CXX=/usr/bin/clang++
    export ANDROID_HOME=~/code/android-studio/tools/android-sdk-linux
fi


#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------
alias ls='ls -lahGF'
alias cls='printf "\033c"'

if [[ ${PLATFORM} == "linux" ]]; then
    alias gitx='gitg'
    alias rebootL='sudo grub-reboot 0; sudo reboot'
    alias rebootW='sudo grub-reboot 2; sudo reboot'
    if [[ $HOSTNAME = "onyx" ]]; then
	alias 'nvidia'='sudo prime-select'
    fi
fi
if [[ ${PLATFORM} == "osx" ]]; then
    alias gitg='gitx'
fi

#-------------------------------------------------------------------------------
# Prompt text
#-------------------------------------------------------------------------------
if [[ ${PLATFORM} == "osx" ]]; then
    export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
fi


#-------------------------------------------------------------------------------
# ls colors
#-------------------------------------------------------------------------------
export CLICOLOR=1
if [[ ${PLATFORM} == "osx" ]]; then
    #               1 2 3 4 5 6 7 8 9 1011
    export LSCOLORS=Gxfxcxdxbxegedabagacad
    # default:
    # export LSCOLORS=exfxcxdxbxegedabagacad

    # a     black
    # b     red
    # c     green
    # d     brown
    # e     blue
    # f     magenta
    # g     cyan
    # h     light grey
    # A     bold black, usually shows up as dark grey
    # B     bold red
    # C     bold green
    # D     bold brown, usually shows up as yellow
    # E     bold blue
    # F     bold magenta
    # G     bold cyan
    # H     bold light grey; looks like bright white
    # x     default foreground or background

    # 1.   directory
    # 2.   symbolic link
    # 3.   socket
    # 4.   pipe
    # 5.   executable
    # 6.   block special
    # 7.   character special
    # 8.   executable with setuid bit set
    # 9.   executable with setgid bit set
    # 10.  directory writable to others, with sticky bit
    # 11.  directory writable to others, without sticky bit

elif [[ ${PLATFORM} == "linux" ]]; then
    # Linux ls colors: (style, then color)
    LS_COLORS='di=0;35:'              # directory
    LS_COLORS=$LS_COLORS:'fi=0;35:'   # file
    LS_COLORS=$LS_COLORS:'ln=0;35:'   # symbolic link
    LS_COLORS=$LS_COLORS:'pi=0;35:'   # fifo file
    LS_COLORS=$LS_COLORS:'so=0;35:'   # socket file
    LS_COLORS=$LS_COLORS:'bd=0;35:'   # block (buffered) special file
    LS_COLORS=$LS_COLORS:'cd=0;35:'   # character (unbuffered) special file
    LS_COLORS=$LS_COLORS:'or=0;35:'   # symbolic link pointing to a non-existent file (orphan)
    LS_COLORS=$LS_COLORS:'mi=0;35:'   # non-existent file pointed to by a symbolic link (visible when you type ls -l)
    LS_COLORS=$LS_COLORS:'ex=0;35:'   # file which is executable (ie. has 'x' set in permissions).
    # LS_COLORS=$LS_COLORS:'*.rpm=0;35:'   # file with the ending .rpm
    export LS_COLORS

    # Styles:
    # 0   = default colour
    # 1   = bold
    # 4   = underlined
    # 5   = flashing text
    # 7   = reverse field
    # 40  = black background
    # 41  = red background
    # 42  = green background
    # 43  = orange background
    # 44  = blue background
    # 45  = purple background
    # 46  = cyan background
    # 47  = grey background
    # 100 = dark grey background
    # 101 = light red background
    # 102 = light green background
    # 103 = yellow background
    # 104 = light blue background
    # 105 = light purple background
    # 106 = turquoise background

    # Colors:
    # 31  = red
    # 32  = green
    # 33  = orange
    # 34  = blue
    # 35  = purple
    # 36  = cyan
    # 37  = grey
    # 90  = dark grey
    # 91  = light red
    # 92  = light green
    # 93  = yellow
    # 94  = light blue
    # 95  = light purple
    # 96  = turquoise
fi

#-------------------------------------------------------------------------------
# Eternal bash history (comment out HISTSIZE and HISTFILESIZE above)
#-------------------------------------------------------------------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#-------------------------------------------------------------------------------
# system-specific configuration
#-------------------------------------------------------------------------------
source ~/.bash_system
