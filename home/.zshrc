export ZSH=/home/bascht/.oh-my-zsh

# Back out if we're surrounded by Emacs
[ "$TERM" = "eterm-color" ] && exec bash

# Back out if we are in a Emacs tramp session
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

source ~/.zplug/init.zsh
source $ZSH/oh-my-zsh.sh

zplug "plugins/fasd", from:oh-my-zsh
zplug "plugins/tmuxinator", from:oh-my-zsh
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "junegunn/fzf", use:shell/key-bindings.zsh
zplug "urbainvaes/fzf-marks"
zplug "Tarrasch/zsh-autoenv"
zplug "MichaelAquilina/zsh-emojis"

AUTOENV_FILE_ENTER=".direnv"
ZSH_THEME="spaceship"

# Only show kubecontext on demand
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_PROMPT_ORDER=(
  user
  dir
  host
  git
  ruby
  docker
  aws
  kubecontext
  exec_time
  line_sep
  jobs
  exit_code
  char
)

alias va="vagrant"
alias tm="tmux -2"
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
alias dry="docker run -it -v /var/run/docker.sock:/var/run/docker.sock moncho/dry"
alias k=kubectl
timestamp() { date +%Y-%m-%d-%H%M%S }
letterup() { take $1 && cp -a ~/Documents/Personal/Brief-Vorlage/2017-LaTeX/* .; }

[ "$TERM" = "xterm-termite" ] && export TERM=xterm-256color

zplug load

# Override the tmux ssh auth sock
SSH_TMUX_SOCK="${HOME}/.ssh/ssh_auth_sock"
if [ "${SSH_AUTH_SOCK}" != "${SSH_TMUX_SOCK}" ]; then
    ln -sf "$SSH_AUTH_SOCK" "$SSH_TMUX_SOCK"
fi

source /usr/share/zsh/site-functions/tmuxinator.zsh

# Via @dohq
# https://gist.github.com/dohq/1dc702cc0b46eb62884515ea52330d60
function fzf-ssh () {
    local selected_host=$(grep "Host " ~/.ssh/config | grep -v '*' | cut -b 6- | fzf --query "$LBUFFER")

    if [ -n "$selected_host" ]; then
        BUFFER="ssh ${selected_host}"
        zle accept-line
    fi
    zle reset-prompt
}

zle -N fzf-ssh
bindkey '\es' fzf-ssh

# Work around a broken autocompletion https://github.com/gopasspw/gopass/issues/585
source <(gopass completion zsh | head -n -1 | tail -n +2)
compdef _gopass gopass

