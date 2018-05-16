export ZSH=/home/bascht/.oh-my-zsh

# Back out if we're surrounded by Emacs
[ "$TERM" = "eterm-color" ] && exec bash

source ~/.zplug/init.zsh
source $ZSH/oh-my-zsh.sh

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/fasd", from:oh-my-zsh
zplug "plugins/rails", from:oh-my-zsh
zplug "plugins/bundler", from:oh-my-zsh
zplug "plugins/ruby", from:oh-my-zsh
zplug "plugins/cp", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh
zplug "plugins/docker-compose", from:oh-my-zsh
zplug "plugins/dotenv", from:oh-my-zsh
zplug "plugins/emacs", from:oh-my-zsh
zplug "plugins/gem", from:oh-my-zsh
zplug "plugins/npm", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh
zplug "plugins/rake", from:oh-my-zsh
zplug "plugins/tmux", from:oh-my-zsh
zplug "plugins/tmuxinator", from:oh-my-zsh
zplug "plugins/tig", from:oh-my-zsh
zplug "plugins/vagrant", from:oh-my-zsh
zplug "plugins/kubectl", from:oh-my-zsh
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf, use:"*linux*amd64*"
zplug "junegunn/fzf", use:shell/key-bindings.zsh
zplug "urbainvaes/fzf-marks"
zplug "Tarrasch/zsh-autoenv"
zplug "MichaelAquilina/zsh-emojis"
zplug "stedolan/jq", as:command, from:gh-r, rename-to:jq

AUTOENV_FILE_ENTER=".direnv"
ZSH_THEME="spaceship"

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
alias emacseval="emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "a")'"
alias dry="docker run -it -v /var/run/docker.sock:/var/run/docker.sock moncho/dry"
letterup() { take $1 && cp -a ~/Documents/Personal/Brief-Vorlage/2017-LaTeX/* .; }

[ "$TERM" = "xterm-termite" ] && export TERM=xterm-256color

zplug load
