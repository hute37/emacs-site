# -*- mode: shell-script;-*-

##
#  environment
#

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi


# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   # export EDITOR='mvim'
   export EDITOR='vim'
 fi

export LESS="-i -j.49 -M -R -S -z-2"
export PAGER=less


