config.load_autoconfig(False)
c.content.user_stylesheets = ['/home/bascht/.local/share/user-content.css']
c.fonts.default_family = "JetBrains Mono"
c.fonts.web.family.sans_serif = "IBM Plex Sans"
c.fonts.web.family.fixed = "JetBrains Mono"
c.qt.force_platform="xcb"
c.qt.force_software_rendering="none"

c.editor.command = ["emacsclient", "--create-frame", "{file}"]
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

c.content.webgl = False
c.content.autoplay = False
c.content.geolocation = False
c.content.blocking.enabled = True
c.content.blocking.method = "adblock"
c.content.notifications.enabled = False

c.hints.border = "1px solid #CCCCCC"
c.hints.chars = "jkluiom,."
c.hints.min_chars = 1

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
with config.pattern('*://windy.com') as p:
    p.content.webgl = True
with config.pattern('*://experience.arcgis.com') as p:
    p.content.webgl = True

