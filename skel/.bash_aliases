# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

alias s='less -S'
alias e='emacsclient --no-wait'

alias p0='PS=0 exec bash'
alias p1='PS=1 exec bash'
alias p9='unset PS; exec bash'

