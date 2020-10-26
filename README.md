Baschts dotfiles
================

Apart from my [Real Dotfiles](https://github.com/bascht/dotfiles), here
is some other stuff.

Quick Screenshot
----------------

![](./screenshot.jpg)

Installation via Chezmoi
------------------------

``` {.bash}
curl -sfL https://git.io/chezmoi | sh
mkdir -p ~/.local/share/
git clone https://git.bascht.space/bascht/dotfiles.git ~/.local/share/chezmoi
chmod 0700 ~/.local/share/cezmoi
~/bin/chezmoi apply
```
