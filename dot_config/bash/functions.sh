#!/usr/bin/env bash

# Make sure completions still work for aliases
complete -o default -F __start_kubectl k

fzf-bash-aliases-widget() {
    set -xv
  local selected="$(launcher-bash-aliases "$@")"
  READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
  notify-send "Readline" "${READLINE_LINE}"
  READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
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

function kpb {
    kpe bash
}

function kgc {
    k get pod $(kfp) -o jsonpath="{.spec.containers[*].name}" | tr -s '[[:space:]]' '\n'
}

function kgl {
    kubectl logs -f $(kfp $(k_app)) $(kgc|fzf)
}

function cm-add-changed-file {
for file in $(chezmoi status|awk '{ print $2 }'); do
	chezmoi --no-pager diff ~/$file
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
    podman run -e VERBOSE=false -i --rm letter < "${1}" > "${base}.pdf"/
}
