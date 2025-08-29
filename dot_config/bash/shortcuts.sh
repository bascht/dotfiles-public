#!/usr/bin/env bash

# Old bash habits die hard
bind '"\ee" edit-and-execute-command'
bind '"-M" default \cn down-or-search'
bind '"\ekv" fish_vi_key_bindings'
bind '"\eke" fish_default_key_bindings'
bind '"\C-n" next-history'

# Jumper is now just zoxide
bind '"\a" "zi; \n"'

# Kubeshizzle
bind '"\egkc" ". <(_kubeconfig)\n "'
bind '"\egkx" "kubectx; \n"'
bind '"\egkn" "kubens; \n"'
bind '"\egkk" "kubectl "'
bind '"\egks" "TERM=xterm-256color k9s\n"'

# Yaml Engineering
bind '"\egoy" "commandline -i -- \"-o yaml\""'
bind '"\egba" "commandline -i \"| bat -l yaml\""'
bind '"\egoba" "commandline -i -- \"-o yaml | bat -l yaml\""'

# Nix
bind '"\egnsp" "nix search nixpkgs "'

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
bind '"\eggs" "git status\n"'
bind '"\egpa" "for remote in $(git remote); git push ${remote}; end"'

# SSH Quick Jump
bind '"\es\es" "cd $(zoxide query bootstrap/saltstack);\n./ssh -e staging ?\t"'
bind '"\es\ep" "cd $(zoxide query bootstrap/saltstack);\n./ssh -e production ?\t"'

# Quote last word
bind '"\C-xq":"\eb"\ef\""'

# Bind my snippet launcher for both in-shell and GUI usage
bind '"\egs": " \C-u `launcher-snippets`\e\C-e\C-a\C-y\C-e"'

bind '"\er": "ripgrepfzf\C-j"'
