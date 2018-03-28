export ZSH=/home/bascht/.oh-my-zsh

# Back out if we're surrounded by Emacs
[ "$TERM" = "eterm-color" ] && exec bash

ZSH_THEME="powerlevel9k/powerlevel9k" # previously: juanghurtado
POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_FOLDER_ICON='blub'
POWERLEVEL9K_HOME_ICON=' '
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(dir root_indicator background_jobs time)

POWERLEVEL9K_PUBLIC_IP_ICON=" "
POWERLEVEL9K_OK_ICON=" "

POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR="\uE0C6 "
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR="\uE0C7"
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR="%F{$(( $DEFAULT_BACKGROUND - 2 ))}•%f"

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
