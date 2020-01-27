# fzf-marks
This plugin can be used to create, delete, and navigate marks in *bash* and *zsh*.
It depends on Junegunn Choi's fuzzy-finder [fzf](https://github.com/junegunn/fzf).

![](https://raw.github.com/uvaes/fuzzy-zsh-marks/demo/demo.gif)
(This video was generated with `screenkey -g $(slop -n -f '%g')` and `simplescreenrecorder`.)

# Installation

If you use *zsh*, I recommend installing with a plugin manager.
In the case of [zgen](https://github.com/tarjoilija/zgen), for example,
simply add the following line to your plugin list:
```zsh
    zgen load urbainvaes/fzf-marks
```

If you use *bash*,
or if you use *zsh* without a plugin manager,
source the file `fzf-marks.plugin.bash` or `fzf-marks.plugin.zsh` from your shell startup file
to enable the plugin.

**Bash installation example**:
```bash
# Clone the git repository in the current directory
git clone https://github.com/urbainvaes/fzf-marks.git

# Add a line to ~/.bashrc to load the plugin whenever bash starts in interactive mode
echo "source $PWD/fzf-marks/fzf-marks.plugin.bash" >> ~/.bashrc

# Source the plugin now so we don't have to restart bash to start using it
source fzf-marks/fzf-marks.plugin.bash
```
# Usage
The script exposes two functions:

- **mark \<mark\>**, to register a new mark to the current directory;
- **fzm [\<optional-initial-query\>]**, to jump to or delete a mark using `fzf`.

Most of the keybindings in the search window are the default fzf ones.
The only additions are

- **ctrl-y**, to jump to a match;
- **ctrl-t**, to toggle a match for deletion;
- **ctrl-d**, to delete selected matches.

By default, the plugin binds the key `ctrl-g` to `fzm`.

# Customization

| Config                  | Default                         | Description                          |
| ------                  | -------                         | -----------                          |
| `FZF_MARKS_FILE`        | `${HOME}/.fzf-marks`            | File containing the marks data       |
| `FZF_MARKS_COMMAND`     | `fzf --height 40% --reverse`    | Command used to call `fzf`           |
| `FZF_MARKS_JUMP`        | `\C-g` (*bash*) or `^g` (*zsh*)     | Keybinding to `fzm`                  |
| `FZF_MARKS_COLOR_LHS`   | 39 (default)                    | ANSI color code of left-hand side    |
| `FZF_MARKS_COLOR_RHS`   | 36 (cyan)                       | ANSI color code of right-hand side   |
| `FZF_MARKS_COLOR_COLON` | 33 (yellow)                     | ANSI color code of separator         |
| `FZF_MARKS_NO_COLORS`   | 0                               | Set this to 1 to disable colors      |
| `FZF_MARKS_KEEP_ORDER`  | 0                               | Set this to 1 to keep order of marks |

See e.g. [here](http://pueblo.sourceforge.net/doc/manual/ansi_color_codes.html) for a description of ANSI color codes.

# License

MIT
