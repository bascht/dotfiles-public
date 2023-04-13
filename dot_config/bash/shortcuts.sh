#!/usr/bin/env bash

# export ATUIN_NOBIND=true
# eval "$(atuin init bash)"
# bind -x '"\C-r": __atuin_history'

# Old bash habits die hard
bind '"\ee" edit-and-execute-command'
bind '"-M" default \cn down-or-search'
bind '"\ekv" fish_vi_key_bindings'
bind '"\eke" fish_default_key_bindings'
bind '"\C-n" next-history'

# Jumper is now just zoxide
bind '"\a" "zi; \n"'

# Kubeshizzle
bind '"\egkk" ". <(_kubeconfig) && kubectx; \n "'
bind '"\egkx" "kubectx; \n"'
bind '"\egkn" "kubens; \n"'
bind '"\egkp" "kubectl get pods;"'

# Yaml Engineering
bind '"\egoy" "commandline -i -- \"-o yaml\""'
bind '"\egba" "commandline -i \"| bat -l yaml\""'
bind '"\egoba" "commandline -i -- \"-o yaml | bat -l yaml\""'

# FZF SSH

bind '"\es" fzf-ssh'
bind '"\egv" fzf-vm'

# Tmux quick access
bind '"\egta" "ta; \n"'
bind '"\egtn" "tn; \n"'

# Git quick access
bind '"\eggf" gf'
bind '"\egcm" gcm'
bind '"\eggbb" gbb'
bind '"\eggp" git push'
bind '"\eggs" "git status"'
bind '"\egpa" "for remote in $(git remote); git push ${remote}; end"'

# SSH Quick Jump
bind '"\egss" "z saltstack; \n./ssh -e "'

# k9s is such a handy tool
bind '"\e9" "k9s\n"'
