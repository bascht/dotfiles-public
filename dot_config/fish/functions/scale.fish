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
