config.load_autoconfig(False)
c.content.user_stylesheets = ['/home/bascht/.local/share/user-content.css']
c.fonts.default_family = "JetBrains Mono"
c.fonts.web.family.sans_serif = "IBM Plex Sans"
c.fonts.web.family.fixed = "JetBrains Mono"
c.url.default_page = "about:blank"
c.url.start_pages = "about:blank"
c.spellcheck.languages = ["de-DE", "en-GB"]
c.editor.command = ["emacsclient", "--socket-name=default", "--create-frame", "{file}"]
c.fonts.tabs.selected = '10pt default_family'
c.fonts.tabs.unselected = '10pt default_family'
c.tabs.padding = {"bottom": 2, "left": 5, "right": 5, "top": 2}
c.scrolling.smooth = True
c.tabs.title.format = "{audio}{index} {current_title}({host})"

c.new_instance_open_target = "tab-bg"
c.input.insert_mode.auto_load = True
c.prompt.filebrowser = False
c.completion.height = "40%"
c.downloads.location.directory = '/home/bascht/Downloads/'

c.qt.args = ['--ignore-gpu-blocklist', '--enable-gpu-rasterization', '--enable-zero-copy', '--enable-features=VaapiVideoDecoder', '--enable-vulkan', '--use-gl=egl']

c.content.webgl = False
c.content.autoplay = False
c.content.geolocation = False
c.content.blocking.enabled = True
c.content.blocking.method = "adblock"
c.content.notifications.enabled = False
c.content.webrtc_ip_handling_policy = "default-public-interface-only"

c.hints.border = "1px solid #CCCCCC"
c.hints.chars = "jkluiom,."
c.hints.min_chars = 1


config.unbind("<ctrl+tab>")
config.bind("<ctrl+tab>", "tab-next")
config.bind("<ctrl+shift+tab>", "tab-prev")

config.bind(',grl', 'greasemonkey-reload')
config.bind(',uu', 'spawn -u untrack-url -o {url}')
config.bind(',uf', 'hint links spawn -u untrack-url -O {hint-url}')
config.bind(',up', 'spawn -u untrack-url -p {clipboard}')

config.bind(',oc', 'spawn --userscript org-capture')

config.bind(',r', 'spawn --userscript readability')

config.bind(',dropr', 'spawn drop /home/bascht/.local/share/qutebrowser/userscripts/readability.html')

config.bind(',gtd', 'quickmark-load todo')
config.bind(',gca', 'quickmark-load calendar')

config.bind(',vu', 'spawn umpv {url}')
config.bind(',vf', 'hint links spawn umpv {hint-url}')
config.bind(',vF', 'hint --rapid links spawn umpv {hint-url}')
config.bind(',su', 'spawn -u send-url')
config.bind(',sp', 'spawn -u send-url baschtfon')

config.bind(',b', 'open https://read.bascht.space/bookmarklet?url={url}')

config.bind('<z><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)"')
config.bind('<z><u><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --username-only')
config.bind('<z><p><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --password-only')
config.bind('<z><o><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --otp-only')

# per Domain settings

## WebGL
for domain in ['www.windy.com', "experience.arcgis.com", "www.figma.com"]:
    config.set('content.webgl', True, domain)

## JS Clipboard Access
for domain in ['wordle.at', 'https://www.nytimes.com/games/wordle/index.html']:
    config.set('content.javascript.can_access_clipboard', True, domain)
