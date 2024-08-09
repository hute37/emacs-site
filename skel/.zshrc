# ------------------------------------------------
export _DOT_ZSHRC_0="$(date  --rfc-3339=ns)"
# ------------------------------------------------

#% echo "% > ~/.zshrc"

# {{{ ---(emacs)---------------------------------------------------------------

[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return                                     
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='%# '

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


if [[ -n "$EMACS" || -n "$INSIDE_EMACS" ]]; then
   if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
       alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
       export TERM=xterm-256color
   else
    if infocmp eterm-color 2>&1 >/dev/null; then
        export TERM=eterm-color
    else
        export TERM=xterm-256color
    fi  
   fi
    alias emacs="emacsclient --no-wait"
    export EDITOR="emacsclient --no-wait"
    export VISUAL="emacsclient"
else
    if [[ -n $SSH_CONNECTION ]]; then
        which nvim >/dev/null  && export EDITOR='nvim' || export EDITOR='vim'    
    else
        which nvim >/dev/null  && export EDITOR='nvim' || export EDITOR='vim'    
    fi
fi
# echo "EDITOR=$EDITOR"
# echo "VISUAL=$VISUAL"
#



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

#ZSH_THEME="robbyrussell"
#ZSH_THEME="avit"
#ZSH_THEME="awesomepanda"
#ZSH_THEME="bureau"
#ZSH_THEME="agnoster"
#ZSH_THEME="gentoo"
#ZSH_THEME="nebirhos"
#ZSH_THEME="fishy"
#ZSH_THEME="jreese"
#ZSH_THEME="lukerandall"
#ZSH_THEME="clean"
#ZSH_THEME="cypher"
#ZSH_THEME="arrow"
#ZSH_THEME="eastwood"
#ZSH_THEME="jnrowe"
#ZSH_THEME="kolo"
#ZSH_THEME="macovsky"
#ZSH_THEME="mgutz"
#ZSH_THEME="muse"
#ZSH_THEME="nicoulaj"
#ZSH_THEME="refined"
#ZSH_THEME="simple"
#ZSH_THEME="skaro"
#ZSH_THEME="terminalparty"
#ZSH_THEME="flazz"
#ZSH_THEME="garyblessington"
#ZSH_THEME="gallois"
#ZSH_THEME="imajes"
#ZSH_THEME="sorin"
#ZSH_THEME="sporty_256"
#ZSH_THEME="sunaku"
#ZSH_THEME="sunrise"
#ZSH_THEME="theunraveler"
#ZSH_THEME="zhann"
#ZSH_THEME="af-magic"
#ZSH_THEME="afowler"
#ZSH_THEME="spaceship"
#ZSH_THEME="agkozak"; AGKOZAK_USER_HOST_DISPLAY=0
ZSH_THEME="sobole"; SOBOLE_THEME_MODE=dark

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
plugins=(git zsh-syntax-highlighting poetry)

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

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
    [ -f /etc/profile.d/vte.sh ] && source /etc/profile.d/vte.sh
fi


[ -f ~/.zsh_aliases ] && source ~/.zsh_aliases || true

### {{{ #ENVIRON //////////////////////////////////////////////////////////////////////////////

export ANSIBLE_VAULT_PASSWORD_FILE=~/.ans-wall.asc

### }}}



### {{{ #AUTH //////////////////////////////////////////////////////////////////////////////

if [ -f ~/.authinfo ]; then

  if [ -z "$OPENAI_API_KEY" ]; then

    vv="$(grep -v '^#' ~/.authinfo | grep 'api.openai.com' | head -n1 | awk '{print $6}')"

    [ -z "$v" ] || export OPENAI_API_KEY="$vv"

  fi

fi

### }}}

### {{{ #NODE //////////////////////////////////////////////////////////////////////////////

if [ -z "$NVM_DIR" ] && [ -d ~/.nvm ]; then

    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

fi


### }}}

### {{{ #CONDA //////////////////////////////////////////////////////////////////////////////
if [ -f ~/.conda.on ]; then
   if [ -z "${CONDA_SHLVL+x}" ]; then
       [ -f /etc/profile.off/conda.sh ] && . /etc/profile.off/conda.sh || true
   fi
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/data/anaconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/data/anaconda/etc/profile.d/conda.sh" ]; then
        . "/data/anaconda/etc/profile.d/conda.sh"
    else
        export PATH="/data/anaconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
 . ~/.conda.on || true
fi
### }}}

# ---(pyenv:begin)-----
if [ ! -f ~/.pyrc.off ]; then

export PYENV_ROOT=~/.pyenv

: ${PYRC_PYENV_HOME:=~/.pyenv}
export PYRC_PYENV_HOME


if [ -d "$PYRC_PYENV_HOME" ]; then
if [ ! "$PYRC_PYENV_ENABLE" = "0" ]; then

if [ -z "$PY_RC_PROFILE" ]; then
export PY_RC_PROFILE=1
    
_py_rc_env_profile() {

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"


}
_py_rc_env_profile

fi        

eval "$(pyenv init -)"

# if [ -n "$VIRTUAL_ENV" ]; then
#    . "$VIRTUAL_ENV/bin/activate"
# else
#    workon default
# fi

fi
fi

: ${PYRC_POETRY_HOME:=~/.poetry}
export PYRC_POETRY_HOME


if [ -d "$PYRC_POETRY_HOME" ]; then
if [ ! "$PYRC_POETRY_ENABLE" = "0" ]; then

export PATH="$HOME/$PYRC_POETRY_HOME/bin:$PATH"

fi
fi

fi
# ---(pyenv:end)-----



#% echo "% < ~/.zshrc"
# ------------------------------------------------
export _DOT_ZSHRC_1="$(date  --rfc-3339=ns)"
# ------------------------------------------------


autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
