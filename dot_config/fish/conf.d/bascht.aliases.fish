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
alias dco="docker-compose"
alias dcorrm="docker-compose run --rm"
alias emacseval="emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "a")'"
alias drri="docker run --rm -ti"
alias dry="drri -v /var/run/docker.sock:/var/run/docker.sock skanehira/docui"
alias k=kubectl
alias kgp="kubectl get pods"
alias krs="kubectl rollout status"
alias g=git
alias git-cleanup-merged-branches="git fetch -va && git branch --merged | egrep -v '(^\*|master)' | xargs git branch -d"
alias wttr="curl 'wttr.in/Neuwied?1pQF&lang=de'"
alias termbin="nc termbin.com 9999"
alias gp="gopass"
alias hl="hledger"
alias cm="chezmoi"
alias cm-add-changed-file="cm add (cm diff|grep ^install|cut -d ' ' -f 5|fzf)"
alias cm-private="chezmoi -S ~/.local/share/chezmoi-private"
alias ls="exa --icons"
alias kn="kubens"
alias alfaview-logfile="/usr/bin/env ls -d -- ~/.local/share/alfaview-staging/logs/*.log|tail -n 1"
alias ta="tmux attach -t (tmux ls -F '#{session_name}' | fzf)"
alias gf="git fetch -a && git rebase && git --no-pager today-all && echo -n '\n'"
alias gbb="set BRANCH=(git for-each-ref refs/heads/ --format='%(refname:short)' | fzf); git fetch origin {$BRANCH} && git checkout {$BRANCH}"
alias tdir="take (mktemp -d --suffix=.(basename (pwd)))"

# Create & enter folder (borrowed from zsh's take)
function take
    mkdir -p $argv
    cd $argv
end

# return the basename of the current folder or an argument
# Used to supply the kubernetes pods with the correct app name
function k_app
    if set -q argv
        echo $argv
    else
        echo (basename (pwd))
end
end

function kfp
    kpods $argv | head -n 1
end

function kpods
    if set -q argv
        kubectl get pod --no-headers=true -o custom-columns=:metadata.name
    else
        kubectl get pod -l app=(k_app $argv) --no-headers=true -o custom-columns=:metadata.name
end
end

# function kpe

#     set pod $argv[1]
#     if (count $argv) == 0;
# end
#     if set -q pod
#         set pod (kpods | fzf)
#         end
#     kubectl exec -ti (kfp $pod)] $argv[2]
# end

function kpb
    kpe bash
end

function kgc
    k get pod (kfp) -o jsonpath="{.spec.containers[*].name}" | tr -s '[[:space:]]' '\n'
end
function kgl
    kubectl logs -f (kfp (k_app)) (kgc|fzf)
end
