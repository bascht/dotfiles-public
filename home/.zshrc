export PATH=$HOME/.local/bin:$HOME/bin:$HOME/bin/go/bin:$PATH
export ZSH=/home/bascht/.oh-my-zsh

ZSH_THEME="funky"

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
alias letterup="cp -a ~/Documents/Personal/Brief-Vorlage/2017-LaTeX/ \$argv; and cd"
