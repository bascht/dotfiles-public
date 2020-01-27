export PATH := ~/bin:$(PATH)

all: update

update:
	chezmoi apply
	vim -c PlugUpdate -c qa
	vim -c GoInstallBinaries -c qa

symlink-systemd:
	mkdir -p ~/.config/systemd/user/default.target.wants
	mkdir -p ~/.config/systemd/user/timers.target.wants
	ln -s ~/.config/systemd/user/emacs.service ~/.config/systemd/user/default.target.wants/
	ln -s ~/.config/systemd/user/mopidy.service ~/.config/systemd/user/default.target.wants/
	ln -s ~/.config/systemd/user/ical2org.service ~/.config/systemd/user/timers.target.wants/
	ln -s ~/.config/systemd/user/maid.service ~/.config/systemd/user/timers.target.wants/
	ln -s ~/.config/systemd/user/worklogger.service ~/.config/systemd/user/timers.target.wants/

update-third-parties: update-zsh update-plug.vim update-spacemacs update-kubectx

update-spacemacs:
	curl -s -L -o spacemacs-develop.tar.gz https://github.com/syl20bnr/spacemacs/archive/develop.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.emacs.d spacemacs-develop.tar.gz

update-zsh:
	curl -s -L -o oh-my-zsh-master.tar.gz https://github.com/robbyrussell/oh-my-zsh/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.oh-my-zsh oh-my-zsh-master.tar.gz

	curl -s -L -o powerlevel10k-master.tar.gz https://github.com/romkatv/powerlevel10k/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.zsh-custom/themes/powerlevel10k powerlevel10k-master.tar.gz

	curl -s -L -o fzf-master.tar.gz https://github.com/junegunn/fzf/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.fzf fzf-master.tar.gz

	curl -s -L -o fzf-marks-master.tar.gz https://github.com/urbainvaes/fzf-marks/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.zsh-custom/plugins/fzf-marks fzf-marks-master.tar.gz

	curl -s -L -o zsh-completions-master.tar.gz https://github.com/zsh-users/zsh-completions/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.zsh-custom/plugins/zsh-completions zsh-completions-master.tar.gz

update-kubectx:
	curl -s -L -o kubectx-master.tar.gz https://github.com/ahmetb/kubectx/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.kubectx kubectx-master.tar.gz
 
update-plug.vim:
	curl -s -L https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim > $$(~/bin/chezmoi source-path ~/.vim/autoload/plug.vim)

update-homeschick:
	curl -s -L -o homeshick-master.tar.gz https://github.com/andsens/homeshick/archive/master.tar.gz
	chezmoi import --strip-components 1 --destination ${HOME}/.homesick/repos/homeshick homeshick-master.tar.gz
