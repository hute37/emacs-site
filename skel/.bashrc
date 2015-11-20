#
# ~/.bashrc
#

# ------------------------------------------------
export _DOT_BASHRC_0=`date  --rfc-3339=ns`
# ------------------------------------------------


# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#PS1='[\u@\h \W]\$ '
#export PS1='\[\e]0;\u@\h \w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '

[ -f ~/.aliases ] && source ~/.aliases || true

# ------------------------------------------------
export _DOT_BASHRC_1=`date  --rfc-3339=ns`
# ------------------------------------------------


