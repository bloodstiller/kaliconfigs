export ZSH="$HOME/.oh-my-zsh"

ZSH_CUSTOM="$HOME/.oh-my-zsh/custom"

ZSH_THEME="jonathan"

plugins=(git
  tmux
  zsh-autosuggestions
  zsh-syntax-highlighting
  fast-syntax-highlighting
  zsh-autocomplete
)
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh


source $ZSH/oh-my-zsh.sh

setopt autocd              # change directory just by typing its name
#setopt correct            # auto correct mistakes
setopt interactivecomments # allow comments in interactive mode
setopt magicequalsubst     # enable filename expansion for arguments of the form ‘anything=expression’
setopt nonomatch           # hide error message if there is no match for the pattern
setopt notify              # report the status of background jobs immediately
setopt numericglobsort     # sort filenames numerically when it makes sense
setopt promptsubst         # enable command substitution in prompt

WORDCHARS=${WORDCHARS//\/}

PROMPT_EOL_MARK=""

bindkey -e                                        # emacs key bindings
bindkey ' ' magic-space                           # do history expansion on space
bindkey '^U' backward-kill-line                   # ctrl + U
bindkey '^[[3;5~' kill-word                       # ctrl + Supr
bindkey '^[[3~' delete-char                       # delete
bindkey '^[[1;5C' forward-word                    # ctrl + ->
bindkey '^[[1;5D' backward-word                   # ctrl + <-
bindkey '^[[5~' beginning-of-buffer-or-history    # page up
bindkey '^[[6~' end-of-buffer-or-history          # page down
bindkey '^[[H' beginning-of-line                  # home
bindkey '^[[F' end-of-line                        # end
bindkey '^[[Z' undo                               # shift + tab undo last action

autoload -Uz compinit
compinit -d ~/.cache/zcompdump
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' rehash true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

HISTFILE=/home/$USER/.zsh_history

HISTSIZE=200000
SAVEHIST=200000

setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
#setopt share_history         # share command history data

# force zsh to show the complete history
alias history="history 0"

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S\ncpu\t%P'

case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

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
toggle_oneline_prompt(){
    if [ "$PROMPT_ALTERNATIVE" = oneline ]; then
        PROMPT_ALTERNATIVE=twoline
    else
        PROMPT_ALTERNATIVE=oneline
    fi
    configure_prompt
    zle reset-prompt
}
zle -N toggle_oneline_prompt
bindkey ^P toggle_oneline_prompt


precmd() {
    # Print the previously configured title
    print -Pnr -- "$TERM_TITLE"

    # Print a new line before the prompt, but only if it is not the first line
    if [ "$NEWLINE_BEFORE_PROMPT" = yes ]; then
        if [ -z "$_NEW_LINE_BEFORE_PROMPT" ]; then
            _NEW_LINE_BEFORE_PROMPT=1
        else
            print ""
        fi
    fi
}

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    export LS_COLORS="$LS_COLORS:ow=30;44:" # fix ls color for folders with 777 permissions

    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias diff='diff --color=auto'
    alias ip='ip --color=auto'

    export LESS_TERMCAP_mb=$'\E[1;31m'     # begin blink
    export LESS_TERMCAP_md=$'\E[1;36m'     # begin bold
    export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
    export LESS_TERMCAP_so=$'\E[01;33m'    # begin reverse video
    export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
    export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
    export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

    # Take advantage of $LS_COLORS for completion as well
    zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
    zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
fi

if [ -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    . /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    # change suggestion color
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#999'
fi

# enable command-not-found if installed
if [ -f /etc/zsh_command_not_found ]; then
    . /etc/zsh_command_not_found
fi

alias ls='exa -T -L=1 -a -B -h -l -g --icons'
alias lsl='exa -T -L=2 -a -B -h -l -g --icons'
alias lss='exa -T -L=1 -B -h -l -g --icons'

alias cat='batcat'

alias doom='~/.config/emacs/bin/doom'

alias autorecon='sudo env "PATH=$PATH" autorecon'

alias virtnet='sudo virsh net-start default &'

alias urldecode='python3 -c "import sys, urllib.parse as ul; \
    print(ul.unquote_plus(sys.argv[1]))"'

alias urlencode='python3 -c "import sys, urllib.parse as ul; \
    print (ul.quote_plus(sys.argv[1]))"'

alias tmux-save-pane='tmux capture-pane -pS -'

alias cpts='~/Dropbox/40-49_Career/41-Courses/41.22-CPTS'

alias bx='~/Dropbox/40-49_Career/46-Boxes/46.02-HTB'

alias wt='~/Desktop/WindowsTools'

alias lt='~/Desktop/LinuxTools'

alias pws='python3 -m http.server 8000'

alias lgu='sudo ip tuntap add user kali mode tun ligolo && sudo ip link set ligolo up'

#nbx () {
    #mkdir loot scans exploit creds
    #mkdir -p scans/nmap scans/bh
    #mkdir -p creds/hashes creds/usernames creds/passwords
#}

alias dl="~/Dropbox/40-49_Career/46-Boxes/46.02-HTB/DanteLab/PentTest/Org"

alias npt="~/.config/scripts/newpentest.sh"

alias key='0x79ea004594bd7e09'

alias rkey='gpg-connect-agent "scd serialno" "learn --force" /bye'

function secret () {
                output=~/"${1}".$(date +%s).enc
                gpg --encrypt --armor --output ${output} -r 0x79ea004594bd7e09 -r admin@mdbdev.io "${1}" && echo "${1} -> ${output}"
}

function reveal () {
                output=$(echo "${1}" | rev | cut -c16- | rev)
                gpg --decrypt --output ${output} "${1}" && echo "${1} -> ${output}"
}

##function start_logging_tmux_session() {
    ##local logfile=~/tmux_logs/$(date +%Y-%m-%d).log
    ##tmux list-windows -a -F '#{session_name}:#{window_index}' | while read window; do
        ##tmux list-panes -t $window -F '#{session_name}:#{window_index}.#{pane_index}' | while read pane; do
            ##tmux pipe-pane -o -t $pane "ansifilter >> ${logfile}_${pane//:/_}"
        ##done
    ##done
##}
##
##
##alias tml='start_logging_tmux_session'

#!/usr/bin/env bash

#!/usr/bin/env bash


if [ -n "$TMUX_PANE" ] && [ "$TMUX_PANE_LOGGING" != "1" ]; then
  export TMUX_PANE_LOGGING=1
  LOGS=$HOME/tmux_logs/$(date +%Y-%m-%d)
  mkdir --parents $LOGS
  LOG_PATH="$LOGS/pane${TMUX_PANE//[^0-9]/}.log"
  tmux pipe-pane -o -t "${TMUX_PANE}" "exec cat - | ansifilter >> $LOG_PATH"
fi

alias dbs='dropbox start'

#if [[ -z "$TMUX" && -z "$SSH_CONNECTION" && -n "$DISPLAY" ]]; then
#  exec tmux new-session -A -s default \; source-file ~/.tmux.conf
#fi

eval "$(starship init zsh)"
