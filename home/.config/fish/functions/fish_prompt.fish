function fish_prompt
	# Just calculate this once, to save a few cycles when displaying the prompt
        # (Stolen from vendor fish_prompt())
	if not set -q __fish_prompt_hostname
		set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
	end

        switch (hostname -s)
                case tdhtest vm
                        set logo "🐮"
                case tivoli stormcage xeros kandalingo
                        set logo "🐎"
                case '*'
                        set logo "?"
        end

        echo -n "[$__fish_prompt_hostname] "

        set_color blue
        echo -n $logo (pwd)
        echo -n " \$ "
        set_color normal
end
