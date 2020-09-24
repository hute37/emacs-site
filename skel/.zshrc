# ------------------------------------------------
export _DOT_ZSHRC_0="$(date  --rfc-3339=ns)"
# ------------------------------------------------

#% echo "% > ~/.zshrc"

# {{{ ---(emacs)---------------------------------------------------------------

[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return                                     
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='%# '

if [[ -n "$EMACS" ]]; then
    if infocmp eterm-color 2>&1 >/dev/null; then
        export TERM=eterm-color
    else
        export TERM=xterm-256color
    fi  
    alias emacs="emacsclient --no-wait"
    export EDITOR="emacsclient --no-wait"
    export VISUAL="emacsclient"
fi

# }}} ------------------------------------------------------------------------

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.

case "$TERM" in
    xdumb) unsetopt zle;;
    *)

ZSH_THEME="robbyrussell"
#ZSH_THEME="avit"
#ZSH_THEME="robbyrussell"
#ZSH_THEME="bureau"
#ZSH_THEME="agnoster"
#ZSH_THEME="gentoo"
#ZSH_THEME="nebirhos"
#ZSH_THEME="fishy"
#ZSH_THEME="jreese"
#ZSH_THEME="lukerandall"
#ZSH_THEME="clean"

    ;;
esac    




#
# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#plugins=(git  zsh-dircolors-solarized)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration


##
# @see: https://github.com/ThiefMaster/zsh-config/blob/master/zshrc.d/shellopts.zsh
#

unsetopt	AUTO_CD


# export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    which nvim >/dev/null  && export EDITOR='nvim' || export EDITOR='vim'    
else
    which nvim >/dev/null  && export EDITOR='nvim' || export EDITOR='vim'    
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
#

setopt shwordsplit

unset GREP_OPTIONS


[ -f ~/.zsh_aliases ] && source ~/.zsh_aliases || true

export ANSIBLE_VAULT_PASSWORD_FILE=~/.ans-wall.asc


### # >>> conda initialize >>>
### # !! Contents within this block are managed by 'conda init' !!
### __conda_setup="$('/data/anaconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
### if [ $? -eq 0 ]; then
###     eval "$__conda_setup"
### else
###     if [ -f "/data/anaconda/etc/profile.d/conda.sh" ]; then
###         . "/data/anaconda/etc/profile.d/conda.sh"
###     else
###         export PATH="/data/anaconda/bin:$PATH"
###     fi
### fi
### unset __conda_setup
### # <<< conda initialize <<<



# ---(pyenv:begin)-----
if [ ! -f ~/.pyrc.off ]; then

export PYENV_ROOT=~/.pyenv

: ${PYRC_PYENV_HOME:=~/.pyenv}
export PYRC_PYENV_HOME


if [ -d "$PYRC_PYENV_HOME" ]; then
if [ ! "$PYRC_PYENV_ENABLE" = "0" ]; then

export PATH=$PYRC_PYENV_HOME/bin:$PATH
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

fi
fi
fi
# ---(pyenv:end)-----

#% echo "% < ~/.zshrc"
# ------------------------------------------------
export _DOT_ZSHRC_1="$(date  --rfc-3339=ns)"
# ------------------------------------------------

