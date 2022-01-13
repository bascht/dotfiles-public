config.load_autoconfig(False)
c.content.user_stylesheets = ['/home/bascht/.local/share/user-content.css']
c.fonts.default_family = "JetBrains Mono"

c.editor.command = ["emacsclient", "--create-frame", "{file}"]
c.fonts.tabs.selected = '10pt default_family'
c.fonts.tabs.unselected = '10pt default_family'
c.tabs.padding = {"bottom": 2, "left": 5, "right": 5, "top": 2}
c.scrolling.smooth = True
c.tabs.title.format = "{audio}{index} {host}: {current_title}"

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
c.hints.chars = "qwertasdfyxcv"
c.hints.min_chars = 1

config.bind(',grl', 'greasemonkey-reload')
config.bind('<z><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)"')
config.bind('<z><u><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --username-only')
config.bind('<z><p><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --password-only')
config.bind('<z><o><l>', 'spawn --userscript qute-pass --mode gopass --username-target secret --username-pattern "user: (.+)" --otp-only')
