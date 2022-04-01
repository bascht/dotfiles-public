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
