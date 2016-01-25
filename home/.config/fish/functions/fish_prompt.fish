function fish_prompt
        switch (hostname -s)
                case tdhtest
                        set logo "🐮"
                case tivoli stormcage xeros
                        set logo "🐎"
                case '*'
                        set logo "?"
        end
        set_color blue
        echo -n $logo (pwd)
        echo -n " \$ "
        set_color normal
end
