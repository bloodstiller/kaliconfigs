export ZSH="$HOME/.oh-my-zsh"
ZSH_CUSTOM="$HOME/.oh-my-zsh/custom"
ZSH_THEME="jonathan"

# Disable OMZ update check (saves ~22ms)
zstyle ':omz:update' mode disabled

# Skip compfix security audit (saves ~70ms from compaudit)
ZSH_DISABLE_COMPFIX=true

plugins=(git tmux zsh-autosuggestions fzf-tab zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# ── Shell options ──────────────────────────────────────────────────────────────
setopt autocd
setopt interactivecomments
setopt magicequalsubst
setopt nonomatch
setopt notify
setopt numericglobsort
setopt promptsubst

WORDCHARS=${WORDCHARS//\/}
PROMPT_EOL_MARK=""

# ── Key bindings ───────────────────────────────────────────────────────────────
bindkey -e
bindkey ' ' magic-space
bindkey '^U' backward-kill-line
bindkey '^[[3;5~' kill-word
bindkey '^[[3~' delete-char
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey '^[[5~' beginning-of-buffer-or-history
bindkey '^[[6~' end-of-buffer-or-history
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[Z' undo
zle -N toggle_oneline_prompt
bindkey ^P toggle_oneline_prompt

# ── Completion ─────────────────────────────────────────────────────────────────
# Defer compinit entirely until first TAB press; zero startup cost.
# First TAB in a session has a brief pause; every subsequent TAB is instant.
# Replace the lazy compinit block with this
autoload -Uz compinit
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
  compinit -d ~/.cache/zcompdump
else
  compinit -C -d ~/.cache/zcompdump
fi

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' rehash true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# fzf-tab — must be configured after compinit has run
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa --color=always $realpath'
zstyle ':fzf-tab:*' switch-group ',' '.'

# ── History ────────────────────────────────────────────────────────────────────
HISTFILE=/home/$USER/.zsh_history
HISTSIZE=200000
SAVEHIST=200000
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
alias history="history 0"

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S\ncpu\t%P'

# ── Colours ────────────────────────────────────────────────────────────────────
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
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

precmd() {
    print -Pnr -- "$TERM_TITLE"
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
    export LS_COLORS="$LS_COLORS:ow=30;44:"

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias diff='diff --color=auto'
    alias ip='ip --color=auto'

    export LESS_TERMCAP_mb=$'\E[1;31m'
    export LESS_TERMCAP_md=$'\E[1;36m'
    export LESS_TERMCAP_me=$'\E[0m'
    export LESS_TERMCAP_so=$'\E[01;33m'
    export LESS_TERMCAP_se=$'\E[0m'
    export LESS_TERMCAP_us=$'\E[1;32m'
    export LESS_TERMCAP_ue=$'\E[0m'

    zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
    zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
fi

# ── Autosuggestions ────────────────────────────────────────────────────────────
# Loaded via plugin above; source system copy only as fallback
if [[ ! -f $ZSH_CUSTOM/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]] \
   && [[ -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
    source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#999'

# command-not-found
[[ -f /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found

# ── Environment ────────────────────────────────────────────────────────────────
export EDITOR='emacs'
export PATH=$PATH:/home/kali/.local/bin

# pnpm
export PNPM_HOME="/home/kali/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# pyenv — lazy: only initialises when pyenv/python/pip/etc. are first called
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
_pyenv_lazy_init() {
  unfunction pyenv python python3 pip pip3 2>/dev/null
  eval "$(pyenv init - zsh)"
  # Re-dispatch the original command
  "$0" "$@"
}
for _cmd in pyenv python python3 pip pip3; do
  functions[$_cmd]="_pyenv_lazy_init"
done
unset _cmd

# ── HTB / Engagement vars ──────────────────────────────────────────────────────
export box="10.129.19.42"
export machine="DC"
export domain="ancoats.htb"
export PH1=""
export PH2=""

# ── Aliases ────────────────────────────────────────────────────────────────────
alias ls='exa -T -L=1 -a -B -h -l -g --icons'
alias lsl='exa -T -L=2 -a -B -h -l -g --icons'
alias lss='exa -T -L=1 -B -h -l -g --icons'
alias cat='batcat'
alias doom='~/.config/emacs/bin/doom'
alias dt='~/.dotfiles'
alias blog='~/Blog'
alias urldecode='python3 -c "import sys, urllib.parse as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias urlencode='python3 -c "import sys, urllib.parse as ul; print(ul.quote_plus(sys.argv[1]))"'
alias bx='/home/kali/VMShare/Master\ Notes/labs\ \&\ courses/htb/_in-progress/Overwatch'
alias en='cd "/home/kali/VMShare/Work/Tests/2026"'
alias wt='~/windowsTools'
alias lt='~/linuxTools'
alias pws='python3 -m http.server 9000'
alias lgu='sudo ip tuntap add user kali mode tun ligolo && sudo ip link set ligolo up'
alias rustscan='docker run -it --rm --name rustscan rustscan/rustscan:2.1.1'
alias bh='docker compose -f ~/.dotfiles/bloodhound/docker-compose.yml up && echo "bh starting"'

# ── Functions ──────────────────────────────────────────────────────────────────
txtlog2md() {
  setopt localoptions nullglob
  local files=( *.txt *.log )
  (( ${#files} )) || { echo "No .txt or .log files found."; return 1; }
  for f in $files; do
    mv -- "$f" "${f%.*}.md"
  done
}

# Update a variable in ~/.zshrc in-place
update_var() {
  sed -i "s/^export $1=.*/export $1=\"$2\"/" ~/.zshrc
  source ~/.zshrc
}

# ── Tmux auto-logging ──────────────────────────────────────────────────────────
if [ -n "$TMUX_PANE" ] && [ "$TMUX_PANE_LOGGING" != "1" ]; then
  export TMUX_PANE_LOGGING=1
  LOGS=$HOME/tmux_logs/$(date +%Y-%m-%d)
  mkdir -p $LOGS
  LOG_PATH="$LOGS/pane${TMUX_PANE//[^0-9]/}.log"
  tmux pipe-pane -o "ansifilter >> $LOG_PATH"
fi

# ── Deferred heavy init (after prompt is ready) ────────────────────────────────
# pip argcomplete backgrounded — harmless since pip won't be called at startup
(( $+commands[pip] )) && eval "$(register-python-argcomplete pip)" &!
eval "$(atuin init zsh)"

