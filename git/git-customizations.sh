# Turn on some fancy git info
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="verbose name"
GIT_PS1_SHOWCOLORHINTS=1

# Include both git-prompt and git-bash completion
source ~/.git-completion.bash
source ~/.git-prompt.sh

# Set our custom prompt!
if [[ "$(tput colors)" -eq "256" ]]; then 
    PROMPT_COMMAND='__git_ps1 "\e[0m\e[38;5;24m\u\e[38;5;238m@\e[38;5;90m\h\e[38;5;238m:\e[0m\w" "\\\$ "'
else 
    PROMPT_COMMAND='__git_ps1 "\u@\h:\w" "\\\$ "'
fi
