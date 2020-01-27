#set fish_greeting
set -gx EDITOR 'emacsclient -t'
set -gx GIT_EDITOR $EDITOR
set -gx PATH $PATH ~/.local/bin ~/bin ~/bin/go/bin /usr/local/bin
set -gx ALTERNATE_EDITOR "" # Start emacs in every case!
set -gx WORKLOG /home/bascht/Documents/Freelance/Worklog/Worklog.md
set -gx Documents /home/bascht/Documents
set -gx Code $Documents/Code

# I need dis
set emflip '(╯°□°)╯︵ ┻━┻'
set emwhy 'џ(ºДºџ)'
set emshrug '¯\_(ツ)_/¯'
set emshrugdis '¯\_ಠ_ಠ_/¯'
set emdis 'ಠ_ಠ'

# Aliases
alias va "vagrant"
alias tm "tmux -2"
alias ta "tm a -t"
alias bi "bundle install"
alias be "bundle exec"
alias ber "bundle exec rake"
alias e $EDITOR
alias vi $EDITOR #going emacs
alias vim $EDITOR
alias er "e --eval '(writeroom-mode)'"
alias dvm "cd ~/DevVM; and vagrant ssh -- -A"
alias ssh-yop "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
alias docker-rm-stopped "docker rm (docker ps -a -q)"
alias docker-rm-dangling "docker rmi (docker images -q -f dangling=true)"
alias dco "docker-compose"

# Manually switch HIDPI
function scale
    /usr/bin/gsettings set org.gnome.desktop.interface scaling-factor $argv;
    /usr/bin/gsettings set org.gnome.settings-daemon.plugins.xsettings overrides "{'Gdk/WindowScalingFactor': <$argv>}";
    if math "$argv == 1"
            /usr/bin/xrandr --dpi 96
    else
            /usr/bin/xrandr --dpi 144
    end
end

# Fire up RVM
set -e GEM_PATH; set -e GEM_HOME

# Fishmarks
. ~/.fishmarks/marks.fish

# Host-specific .vagrant directories
set -gx VAGRANT_DOTFILE_PATH .vagrant-(hostname -s)
