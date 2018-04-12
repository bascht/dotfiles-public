export ZSH=/home/bascht/.oh-my-zsh

# Back out if we're surrounded by Emacs
[ "$TERM" = "eterm-color" ] && exec bash

ZSH_THEME="wedisagree"

plugins=(
  git
  fasd
  rails
  bundler
  ruby
  cp
  jump
  docker
  docker-compose
  dotenv
  emacs
  gem
  npm
  pip
  rake
  tmux
  tmuxinator
  tig
  vagrant
)

source $ZSH/oh-my-zsh.sh
eval "$(direnv hook zsh)"

alias va="vagrant"
alias tm="tmux -2"
alias ta="tm a -t"
alias bi="bundle install"
alias be="bundle exec"
alias ber="bundle exec rake"
alias e=$EDITOR
alias vi=$EDITOR #going emacs
alias vim=$EDITOR
alias er="e --eval '(writeroom-mode)'"
alias dvm="cd ~/DevVM; and vagrant ssh -- -A"
alias ssh-yop="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias docker-rm-stopped="docker rm (docker ps -a -q)"
alias docker-rm-dangling="docker rmi (docker images -q -f dangling=true)"
alias dco="docker-compose"
alias emacseval="emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "a")'"
alias dry="docker run -it -v /var/run/docker.sock:/var/run/docker.sock moncho/dry"
letterup() { take $1 && cp -a ~/Documents/Personal/Brief-Vorlage/2017-LaTeX/* .; }

[ "$TERM" = "xterm-termite" ] && export TERM=xterm-256color

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
