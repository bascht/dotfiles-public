config.load_autoconfig(False)
c.content.user_stylesheets = ['/home/bascht/.local/share/user-content.css']
c.fonts.default_family = "JetBrains Mono"
c.fonts.web.family.sans_serif = "IBM Plex Sans"
c.fonts.web.family.fixed = "JetBrains Mono"
c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"
c.spellcheck.languages = ["de-DE", "en-GB"]
c.editor.command = ["qutebrowser-edit", "{file}"]
c.fonts.tabs.selected = '10pt default_family'
c.fonts.tabs.unselected = '10pt default_family'
c.tabs.padding = {"bottom": 3, "left": 5, "right": 5, "top": 3}
c.scrolling.smooth = True
c.tabs.title.format = "{audio}{index} {current_title}({host})"
c.tabs.favicons.scale = 0.8
c.tabs.indicator.width = 2
c.session.lazy_restore = True
c.url.searchengines = {"DEFAULT": "https://search.brave.com/search?q={}"}

c.new_instance_open_target = "tab-bg"
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_load = False
c.prompt.filebrowser = False
c.completion.height = "40%"
c.downloads.location.directory = '/home/bascht/Downloads/'

c.qt.args = ['ignore-gpu-blocklist', 'untrusted-args', 'enable-zero-copy', 'enable-features=VaapiVideoEncoder,VaapiVideoDecoder,CanvasOopRasterization,RawDraw,WebGPU', 'use-gl=egl', 'enable-accelerated-video-decode', 'enable-gpu-rasterization', 'disable-gpu-driver-bug-workarounds']

#c.qt.args = ['untrusted-args','enable-features=VaapiVideoDecoder']
c.content.webgl = False
c.content.autoplay = False
c.content.geolocation = False
c.content.blocking.enabled = True
c.content.blocking.method = "adblock"
c.content.notifications.enabled = False
c.content.webrtc_ip_handling_policy = "default-public-interface-only"

c.hints.border = "1px solid #CCCCCC"
c.hints.chars = "jklöuiopm"
c.hints.min_chars = 1


config.unbind("<ctrl+tab>")
config.bind("<ctrl+tab>", "tab-next")
config.bind("<ctrl+shift+tab>", "tab-prev")

config.bind(',grl', 'greasemonkey-reload;; reload')
config.bind(',uu', 'spawn -u untrack-url -o {url}')
config.bind(',uf', 'hint links spawn -u untrack-url -O {hint-url}')
config.bind(',up', 'spawn -u untrack-url -p {clipboard}')
config.bind(',e', 'jseval document.querySelector("button.btn-edit").click()')

config.bind(',ttl', 'set tabs.position left')
config.bind(',ttt', 'set tabs.position top')

config.bind(',oc', 'spawn --userscript org-capture')

config.bind(',r', 'spawn --userscript readability')

config.bind(',dropr', 'spawn drop /home/bascht/.local/share/qutebrowser/userscripts/readability.html')

config.bind(',gtd', 'quickmark-load todo')
config.bind(',gca', 'quickmark-load calendar')

config.bind(',vu', 'spawn umpv {url}')
config.bind(',vf', 'hint links spawn umpv {hint-url}')
config.bind(',vF', 'hint --rapid links spawn umpv {hint-url}')
config.bind(',mu', 'open http://localhost:6680/youtube?url={url}')
config.bind(',mf', 'hint links open http://localhost:6680/youtube?url={hint-url}')
config.bind(',su', 'spawn -u send-url')
config.bind(',sp', 'spawn -u send-url baschtfon')

config.bind(',aw', 'set content.webgl true')
config.bind(',ac', 'set content.javascript.can_access_clipboard true')

config.bind(',b', 'open https://read.yakshed.org/bookmarklet?url={url}')
config.bind(',y', 'open https://social.yakshed.org/authorize_interaction?uri={url}')


# GitLab quick-actions
config.bind(',glc', ':scroll-to-perc 100;; jseval document.querySelector("textarea#note-body").focus()')
config.bind(',glp0', ':scroll-to-perc 100;; jseval document.querySelector("textarea#note-body").focus() ;; insert-text /label ~priority::0')
config.bind(',glp1', ':scroll-to-perc 100;; jseval document.querySelector("textarea#note-body").focus() ;; insert-text /label ~priority::1')
config.bind(',glp2', ':scroll-to-perc 100;; jseval document.querySelector("textarea#note-body").focus() ;; insert-text /label ~priority::2')
config.bind(',glp3', ':scroll-to-perc 100;; jseval document.querySelector("textarea#note-body").focus() ;; insert-text /label ~priority::3')

config.bind('<z><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)"')
config.bind('<z><u><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --username-only')
config.bind('<z><p><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --password-only')
config.bind('<z><o><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --otp-only')

config.bind('ygs', 'spawn get-gitlab-shortcode {url}')
config.bind('ygm', 'spawn get-gitlab-shortcode-as-markdown {url}')

# per Domain settings

## WebGL
for domain in [
        'www.windy.com', 
        "experience.arcgis.com", 
        "www.figma.com", 
        "figma.com",
        "globe.adsbexchange.com",
        "www.komoot.de",
        ]:
    config.set('content.webgl', True, domain)

## JS Clipboard Access
for domain in ['wordle.at', 'https://www.nytimes.com/games/wordle/index.html']:
    config.set('content.javascript.can_access_clipboard', True, domain)

# base16-qutebrowser (https://github.com/theova/base16-qutebrowser)
# Base16 qutebrowser template by theova and Daniel Mulford
# Flat scheme by Chris Kempson (http://chriskempson.com)

base00 = "#2C3E50"
base01 = "#34495E"
base02 = "#7F8C8D"
base03 = "#95A5A6"
base04 = "#BDC3C7"
base05 = "#e0e0e0"
base06 = "#f5f5f5"
base07 = "#ECF0F1"
base08 = "#E74C3C"
base09 = "#E67E22"
base0A = "#F1C40F"
base0B = "#2ECC71"
base0C = "#1ABC9C"
base0D = "#3498DB"
base0E = "#9B59B6"
base0F = "#be643c"

# set qutebrowser colors

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
c.colors.completion.fg = base05

# Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = base00

# Background color of the completion widget for even rows.
c.colors.completion.even.bg = base00

# Foreground color of completion widget category headers.
c.colors.completion.category.fg = base0D

# Background color of the completion widget category headers.
c.colors.completion.category.bg = base00

# Top border color of the completion widget category headers.
c.colors.completion.category.border.top = base00

# Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = base00

# Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = base05

# Background color of the selected completion item.
c.colors.completion.item.selected.bg = base02

# Top border color of the selected completion item.
c.colors.completion.item.selected.border.top = base02

# Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = base02

# Foreground color of the matched text in the selected completion item.
c.colors.completion.item.selected.match.fg = base05

# Foreground color of the matched text in the completion.
c.colors.completion.match.fg = base09

# Color of the scrollbar handle in the completion view.
c.colors.completion.scrollbar.fg = base05

# Color of the scrollbar in the completion view.
c.colors.completion.scrollbar.bg = base00

# Background color of disabled items in the context menu.
c.colors.contextmenu.disabled.bg = base01

# Foreground color of disabled items in the context menu.
c.colors.contextmenu.disabled.fg = base04

# Background color of the context menu. If set to null, the Qt default is used.
c.colors.contextmenu.menu.bg = base00

# Foreground color of the context menu. If set to null, the Qt default is used.
c.colors.contextmenu.menu.fg =  base05

# Background color of the context menu’s selected item. If set to null, the Qt default is used.
c.colors.contextmenu.selected.bg = base02

#Foreground color of the context menu’s selected item. If set to null, the Qt default is used.
c.colors.contextmenu.selected.fg = base05

# Background color for the download bar.
c.colors.downloads.bar.bg = base00

# Color gradient start for download text.
c.colors.downloads.start.fg = base00

# Color gradient start for download backgrounds.
c.colors.downloads.start.bg = base0D

# Color gradient end for download text.
c.colors.downloads.stop.fg = base00

# Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = base0C

# Foreground color for downloads with errors.
c.colors.downloads.error.fg = base08

# Font color for hints.
c.colors.hints.fg = base00

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
c.colors.hints.bg = base0A

# Font color for the matched part of hints.
c.colors.hints.match.fg = base05

# Text color for the keyhint widget.
c.colors.keyhint.fg = base05

# Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = base05

# Background color of the keyhint widget.
c.colors.keyhint.bg = base00

# Foreground color of an error message.
c.colors.messages.error.fg = base00

# Background color of an error message.
c.colors.messages.error.bg = base08

# Border color of an error message.
c.colors.messages.error.border = base08

# Foreground color of a warning message.
c.colors.messages.warning.fg = base00

# Background color of a warning message.
c.colors.messages.warning.bg = base0E

# Border color of a warning message.
c.colors.messages.warning.border = base0E

# Foreground color of an info message.
c.colors.messages.info.fg = base05

# Background color of an info message.
c.colors.messages.info.bg = base00

# Border color of an info message.
c.colors.messages.info.border = base00

# Foreground color for prompts.
c.colors.prompts.fg = base05

# Border used around UI elements in prompts.
c.colors.prompts.border = base00

# Background color for prompts.
c.colors.prompts.bg = base00

# Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = base02

# Foreground color for the selected item in filename prompts.
c.colors.prompts.selected.fg = base05

# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = base05

# Background color of the statusbar.
c.colors.statusbar.normal.bg = base00

# Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = base00

# Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = base0C

# Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = base0A

# Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = base00

# Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = base0E

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = base00

# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = base04

# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = base01

# Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = base0E

# Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = base01

# Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = base0D

# Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = base00

# Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = base0D

# Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = base00

# Background color of the progress bar.
c.colors.statusbar.progress.bg = base0D

# Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = base05

# Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = base08

# Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = base09

# Foreground color of the URL in the statusbar on successful load
# (http).
c.colors.statusbar.url.success.http.fg = base0B

# Foreground color of the URL in the statusbar on successful load
# (https).
c.colors.statusbar.url.success.https.fg = base0B

# Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = base0E

# Background color of the tab bar.
c.colors.tabs.bar.bg = base00

# Color gradient start for the tab indicator.
c.colors.tabs.indicator.start = base0D

# Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = base0C

# Color for the tab indicator on errors.
c.colors.tabs.indicator.error = base08

# Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = base05

# Background color of unselected odd tabs.
c.colors.tabs.odd.bg = base00

# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = base05

# Background color of unselected even tabs.
c.colors.tabs.even.bg = "#2C1E20"

# Background color of pinned unselected even tabs.
c.colors.tabs.pinned.even.bg = base0B

# Foreground color of pinned unselected even tabs.
c.colors.tabs.pinned.even.fg = base00

# Background color of pinned unselected odd tabs.
c.colors.tabs.pinned.odd.bg = base0B

# Foreground color of pinned unselected odd tabs.
c.colors.tabs.pinned.odd.fg = base00

# Background color of pinned selected even tabs.
c.colors.tabs.pinned.selected.even.bg = base02

# Foreground color of pinned selected even tabs.
c.colors.tabs.pinned.selected.even.fg = base05

# Background color of pinned selected odd tabs.
c.colors.tabs.pinned.selected.odd.bg = base02

# Foreground color of pinned selected odd tabs.
c.colors.tabs.pinned.selected.odd.fg = base05

# Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = base05

# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = base02

# Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = base05

# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = base02

# Background color for webpages if unset (or empty to use the theme's
# color).
#c.colors.webpage.bg = base00

c.colors.completion.category.fg = base06
c.colors.completion.category.bg = base01
c.colors.completion.category.border.bottom = base02

c.colors.completion.even.bg = "#203040"
c.colors.completion.odd.bg = base01
