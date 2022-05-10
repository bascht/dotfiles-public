# Old bash habits die hard
bind \xe edit_command_buffer

# Jumper
bind \a "cd (z -l | sort -rn | cut -c 12- | fzf --query=(commandline --current-token) --no-sort --exact); commandline -f repaint"

# Kubeshizzle
bind \egkk "_kubeconfig|source; and kubectx; and kubens; commandline -f repaint"
bind \egkx "kubectx; commandline -f repaint"
bind \egkn "kubens; commandline -f repaint"
bind \egkp "kgp; commandline -f repaint"

# Yaml Engineering
bind \egoy "commandline -i -- '-o yaml'"
bind \egba "commandline -i '| bat -l yaml'"
bind \egoba "commandline -i -- '-o yaml | bat -l yaml'"

# FZF SSH

bind \es fzf-ssh
bind \ev fzf-vm

# Tmux quick access
bind \egta "ta; commandline -f repaint"
bind \egtn "tn; commandline -f repaint"

# Git quick access
bind \eggf gf
bind \egcm gcm
bind \eggbb gbb
bind \eggp git push
bind \eggs "git status"
