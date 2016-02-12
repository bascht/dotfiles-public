function fish_prompt
        switch (hostname -s)
                case tdhtest
                        set logo "🐮"
                case tivoli stormcage xeros
                        set logo "🐎"
                case '*'
                        set logo "?"
        end

        if not [ -z $CLIENT ]
                echo -n "[$CLIENT] "
        end

        set_color blue
        echo -n $logo (pwd)
        echo -n " \$ "
        set_color normal
end
