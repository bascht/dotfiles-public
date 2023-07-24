alias va="vagrant"
alias bi="bundle install"
alias be="bundle exec"
alias ber="bundle exec rake"
alias e=$EDITOR
alias vim=nvim
alias ssh-yop="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias scp-yop="scp -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias docker-rm-stopped="docker rm (docker ps -a -q)"
alias docker-rm-dangling="docker rmi (docker images -q -f dangling=true)"
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
alias ls="exa --icons"
alias alfaview-logfile="/usr/bin/env ls -d -- ~/.local/share/alfaview-staging/logs/*.log|tail -n 1"
alias ta="tmux attach -t \$(tmux ls -F '#{session_name}' | fzf)"
alias gf="git fetch -a && git rebase && git --no-pager today-all;  commandline --insert ''"
alias gcm="git checkout (git branch --format='%(refname:short)' | grep -E '^main|^master')"
alias gbb="set BRANCH=(git for-each-ref refs/heads/ --format='%(refname:short)' | fzf); git fetch origin {$BRANCH} && git checkout {$BRANCH}"
alias tdir="take \$(mktemp -d --suffix=.\$(basename \$(pwd)))"
alias ag="rg"
alias dco="docker-compose"
alias k="kubectl"
alias kgp="kubectl get pods"
alias krs="kubectl rollout status"
alias kubens='kubectl config set-context --current --namespace="$(kubectl get ns -o json| jq -r .items[].metadata.name | fzf)"'
alias kubectx='kubectl config use-context "$( yq .contexts[].name $KUBECONFIG  | fzf)"'
alias kn="kubens"

# Abbreviations

alias osslx="openssl x509 -noout -text"
alias osslc="openssl s_client -connect"
alias osslb64="base64 -d | openssl x509 -noout -text"

# Create & enter folder (borrowed from zsh's take)
function take {
    mkdir -p $1
    cd $1
}

# return the basename of the current folder or an argument
# Used to supply the kubernetes pods with the correct app name
function k_app {
    if set -q argv; then
        echo $argv
    else
        echo $(basename $(pwd))
    fi
}

function kfp {
    kpods $argv | head -n 1
}

function kpods {
    if set -q argv; then
        kubectl get pod --no-headers=true -o custom-columns=:metadata.name
    else
        kubectl get pod -l app=$(k_app $argv) --no-headers=true -o custom-columns=:metadata.name
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
