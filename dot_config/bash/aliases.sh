#!/usr/bin/env bash

alias va="vagrant"
alias bi="bundle install"
alias be="bundle exec"
alias ber="bundle exec rake"
alias e=$EDITOR
alias vim=em
alias ssh-yop="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scp-yop="scp -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias docker-stop-all="docker stop \$(docker ps -a -q)"
alias docker-rm-stopped="docker rm \$(docker ps -a -q)"
alias docker-rm-dangling="docker rmi \$(docker images -q -f dangling=true)"
alias dcorrm="docker-compose run --rm"
alias emacseval="emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "a")'"
alias drri="docker run --rm -ti"
alias dry="drri -v /var/run/docker.sock:/var/run/docker.sock skanehira/docui"
alias g=git
alias git-cleanup-merged-branches="git fetch -va && git branch --merged | egrep -v '(^\*|master)' | xargs git branch -d"
alias wttr="curl 'wttr.in/Neuwied?1pQF&lang=de'"
alias termbin="nc termbin.com 9999"
alias gp="gopass"
alias hl="hledger"
alias cm="chezmoi"
alias cm-private="chezmoi -S ~/.local/share/chezmoi-private"

alias alfaview-logfile="/usr/bin/env ls -d -- ~/.local/share/alfaview-staging/logs/*.log|tail -n 1"
alias ta="tmux attach -t \$(tmux ls -F '#{session_name}' | fzf)"
alias ta-if-in-code='if [[ "${PWD}" == "${HOME}/Code"* ]]; then ta; fi'
alias gf="git fetch -a && git rebase && git --no-pager today-all;  commandline --insert ''"
alias gcm="git checkout \$(git branch --format='%(refname:short)' | grep -E '^main|^master')"
alias gbb="set BRANCH=(git for-each-ref refs/heads/ --format='%(refname:short)' | fzf); git fetch origin {$BRANCH} && git checkout {$BRANCH}"
alias tdir="take \$(mktemp -d --suffix=.\$(basename \$(pwd)))"
alias ag="rg"
alias dco="docker compose"
alias docker-compose='echo -e "$(tput bold)Please slowly migrate your muscle memory off of docker-compose! 2 second time penalty!$(tput sgr0)";sleep 2;docker compose'
alias k="kubectl"
alias kgp="kubectl get pods"
alias krs="kubectl rollout status"
alias kubens='kubectl config set-context --current --namespace="$(kubectl get ns -o json| jq -r .items[].metadata.name | fzf)"'
alias kubectx='kubectl config use-context "$( yq .contexts[].name $KUBECONFIG  | fzf)"'
alias kn="kubens"
alias package="yay -Slq | fzf --preview-window=80% --preview 'cat <(yay -Si {1}) <(yay --getpkgbuild --print {1})' | xargs --no-run-if-empty --open-tty yay -S"
# Make sure completions still work for aliases
complete -o default -F __start_kubectl k

# Abbreviations

alias osslx="openssl x509 -noout -text"
alias osslc="openssl s_client -connect"
alias osslb64="base64 -d | openssl x509 -noout -text"
alias wlposslx="wl-paste|openssl x509 -noout -text"

alias oyaml="-o yaml"
alias baml="|bat -l yaml"

fzf-bash-aliases-widget() {
    set -xv
    local selected="$(launcher-bash-aliases "$@")"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
    notify-send "Readline" "${READLINE_LINE}"
    READLINE_POINT=$((READLINE_POINT + ${#selected}))
    notify-send "Readline Point" "${READLINE_POINT}"
}

__fzf_bash_aliases__() {
    local output opts script
    opts="--height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} -n2..,.. --scheme=history --bind=ctrl-r:toggle-sort ${FZF_CTRL_R_OPTS-} +m --read0"
    script='BEGIN { getc; $/ = "\n\t"; $HISTCOUNT = $ENV{last_hist} + 1 } s/^[ *]//; print $HISTCOUNT - $. . "\t$_" if !$seen{$_}++'
    match=$(echo "$READLINE_LINE" | awk '{ print $NF }')
    previous_line=$(echo "$READLINE_LINE" | sed s/'\w*$'//)
    echo "${READLINE_LINE}"
    echo $match
    echo $previous_line
    output=$(launcher-bash-aliases $(echo "$READLINE_LINE" | awk '{print $NF }')) || return
    READLINE_LINE=$previous_line${output#*$'\t'}
    if [[ -z "$READLINE_POINT" ]]; then
        echo "$READLINE_LINE"
    else
        READLINE_POINT=0x7fffffff
    fi
}

# Create & enter folder (borrowed from zsh's take)
function take {
    mkdir -p $1
    cd $1
}

# return the basename of the current folder or an argument
# Used to supply the kubernetes pods with the correct app name
function k_app {
    if [[ $# -ne 0 ]]; then
        echo "${*}"
    else
        echo $(basename $(pwd))
    fi
}

function kfp {
    kpods ${*} | head -n 1
}

function kpods {
    if [[ $# -eq 0 ]]; then
        kubectl get pod --no-headers=true -o custom-columns=:metadata.name
    else
        kubectl get pod -l app=$(k_app "${*}") --no-headers=true -o custom-columns=:metadata.name
    fi
}

# function kpe

#     set pod $argv[1]
#     if (count $argv) == 0;
# end
#     if set -q pod
#         set pod (kpods | fzf)
#         end
#     kubectl exec -ti (kfp $pod)] $argv[2]
# end

function kpb {
    kpe bash
}

function kgc {
    k get pod $(kfp) -o jsonpath="{.spec.containers[*].name}" | tr -s '[[:space:]]' '\n'
}

function kgl {
    kubectl logs -f $(kfp $(k_app)) $(kgc | fzf)
}

function cm-add-changed-file {
    for file in $(chezmoi status | awk '{ print $2 }'); do
        chezmoi diff --pager=delta --reverse "${HOME}/${file}"
        a=$(echo -e "yes\nno" | fzf --height=3)
        if [ "$a" == "yes" ]; then
            chezmoi add "~/$file"
        fi
        clear
    done
}

function ddir {
    take "$(datestamp)-${*}"
}

function letter {
    base=$(basename "${1}" ".tex")
    podman run -e VERBOSE=false -i --rm letter <"${1}" >"${base}.pdf"
}

function ripgrepfzf {
  RELOAD='reload:rg --column --color=always --smart-case {q} || :'
  OPENER='if [[ $FZF_SELECT_COUNT -eq 0 ]]; then
            em {1} +{2}     # No selection. Open the current line in Vim.
          else
            em +cw -q {+f}  # Build quickfix list for the selected items.
          fi'
  fzf --disabled --ansi --multi \
      --bind "start:$RELOAD" --bind "change:$RELOAD" \
      --bind "enter:become:$OPENER" \
      --bind "ctrl-o:execute:$OPENER" \
      --bind 'alt-a:select-all,alt-d:deselect-all,ctrl-/:toggle-preview' \
      --delimiter : \
      --preview 'bat --style=full --color=always --highlight-line {2} {1}' \
      --preview-window '~4,+{2}+4/3,<80(up)' \
      --query "$*"
}

# Keep muscle memory & stick with eza
function ls {
  args="--no-user --no-permissions"
  case "${1}" in
    "-l")
      args="-l"
      ;;
    "-ls")
      args="-G --symlinks"
      ;;
    "-lha")
      args="-l --time-style long-iso"
      ;;
    esac

  eza --icons ${args} ${*}
}
