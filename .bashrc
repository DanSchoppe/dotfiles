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
if [[ ${PLATFORM} == "osx" ]]; then
    export PATH=$HOME/local/bin:$PATH
    export PATH=$PATH:$HOME/local/Cellar/llvm/3.5.1/bin
    export PATH=$PATH:$HOME/Code/rtags/bin
    export PATH=$PATH:$HOME/Applications
    export EDITOR=$HOME/local/bin/emacs
# elif [[ ${PLATFORM} == "linux" ]]; then
fi


#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------
alias ls='ls -lahGF'


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
