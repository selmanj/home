# Turn on some fancy git info
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="verbose name"
GIT_PS1_SHOWCOLORHINTS=1

# Include both git-prompt and git-bash completion
source ~/.git-completion.bash
source ~/.git-prompt.sh
source ~/.misc.sh

# Set our custom prompt!
# Note that we need to wrap escape codes with \001<escape code>\002 to signal
# to bash that the characters in between should not be counted against line
# width.
if [[ "$(tput colors)" -eq "256" ]]; then 
    # For now, also assume that we support UTF-8
    PROMPT_COMMAND='__git_ps1 "\001\e[0m\e[38;5;24m\002\u\001\e[38;5;238m\002@$(color_hostname_for_prompt $(hostname -s))\001\e[38;5;238m\002:\001\e[0m\002\w" "\n\\\$ "'
else 
    PROMPT_COMMAND='__git_ps1 "\u@\h:\w" "\n\\\$ "'
fi
