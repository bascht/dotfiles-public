# Directly start sway if we're on tty1
if [ "$(tty)" = "/dev/tty1" ]; then
	  exec sway
fi

# Back out if we're surrounded by Emacs
[ "$TERM" = "eterm-color" ] && exec bash
[ "$TERM" = "xterm-termite" ] && export TERM=xterm-256color

# Back out if we are in a Emacs tramp session
if [[ $TERM == "eterm-color " ]] || [[ $TERM == "dumb" ]]; then
    unsetopt zle
    PS1='$ '
    return
fi

plugins=(fzf fasd dotenv tmuxinator)
fpath=( ~/.kubectx/completion "${fpath[@]}" )

ZSH_CUSTOM="${HOME}/.zsh-custom"
ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_DOTENV_FILE=".direnv"
FZF_MARKS_FILE="${HOME}/.config/fzf/${HOST}"

export ZSH=/home/bascht/.oh-my-zsh

source $ZSH/oh-my-zsh.sh
source $ZSH_CUSTOM/plugins/fzf-marks/fzf-marks.plugin.zsh

alias va="vagrant"
alias ta="tm a -t"
alias bi="bundle install"
alias be="bundle exec"
alias ber="bundle exec rake"
alias e=$EDITOR
alias ssh-yop="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias docker-rm-stopped="docker rm (docker ps -a -q)"
alias docker-rm-dangling="docker rmi (docker images -q -f dangling=true)"
alias dco="docker-compose"
alias dcorrm="docker-compose run --rm"
alias emacseval="emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "a")'"
alias drri="docker run --rm -ti"
alias dry="drri -v /var/run/docker.sock:/var/run/docker.sock skanehira/docui"
alias k=kubectl
alias g=git
alias git-cleanup-merged-branches="git fetch -va && git branch --merged | egrep -v '(^\*|master)' | xargs git branch -d"
alias wttr="curl v2.wttr.in/Bogenhausen"
timestamp() { date +%Y-%m-%d-%H%M%S }
letterup() { take $1 && cp -a ~/Documents/Personal/Brief-Vorlage/2017-LaTeX/* .; }

# Override the tmux ssh auth sock
SSH_TMUX_SOCK="${HOME}/.ssh/ssh_auth_sock"
if [ "${SSH_AUTH_SOCK}" != "${SSH_TMUX_SOCK}" ]; then
    ln -sf "$SSH_AUTH_SOCK" "$SSH_TMUX_SOCK"
fi

# Pretty up FZF
export FZF_DEFAULT_OPTS='
  --prompt=" "
  --color=bg+:#f0f0f1,bg:#fafafa,spinner:#0184bc,hl:#4078f2
  --color=fg:#696c77,header:#4078f2,info:#c18401,pointer:#0184bc
  --color=marker:#0184bc,fg+:#202227,prompt:#c18401,hl+:#4078f2
'
#source /usr/share/zsh/site-functions/tmuxinator.zsh

# Via @dohq
# https://gist.github.com/dohq/1dc702cc0b46eb62884515ea52330d60
function fzf-ssh () {
    local selected_host=$(grep "Host " ~/.ssh/config | grep -v '*' | cut -b 6- | fzf --reverse --height=20 --query "$LBUFFER")

    if [ -n "$selected_host" ]; then
        BUFFER="ssh ${selected_host}"
        zle accept-line
    fi
    zle reset-prompt
}

# Only load kubectl completion when needed. I's bog-slow
if [ -f $KUBECONFIG ]; then
    source <(kubectl completion zsh);
fi;

zle -N fzf-ssh
bindkey '\es' fzf-ssh

function fzf-vm () {
    local selected_vm=$(grep "Host " ~/.ssh/config | grep -oP "vm-(\w+)" | sort -u | cut -b 4-)
    vm $selected_vm
}
zle -N fzf-ssh
bindkey '\ev' fzf-vm

# Via @leahneukirchen
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word

if [ -f ~/Code/architecture/bin/ia ]; then
  source <(~/Code/architecture/bin/ia completion)
fi;

source ~/.zsh-custom/plugins/
source ~/.p10k.zsh
