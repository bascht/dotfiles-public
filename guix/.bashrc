# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

HISTCONTROL=ignoredups:ignorespace
HISTFILESIZE=80000
HISTSIZE=10000
GPG_TTY="$(tty)"
export GPG_TTY

shopt -s histappend
shopt -s checkwinsize
shopt -s extglob
shopt -s globstar
shopt -s checkjobs




~/.guix-profile/bin/gpg-connect-agent updatestartuptty /bye > /dev/null

eval "$(~/.guix-profile/bin/zoxide init bash )"

if [[ $TERM != "dumb" ]]; then
  eval "$(~/.guix-profile/bin/starship init bash --print-full-init)"
fi

if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
  eval "$(~/.guix-profile/bin/atuin init bash '--disable-up-arrow')"
fi

for f in ~/.config/bash/*.sh; do source $f; done
source /home/bascht/.guix-profile/etc/bash_completion.d/*

eval "$(direnv hook bash)"
