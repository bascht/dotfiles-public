#!/bin/bash

exec find ~/.emacs.d -name "*.elc" -print0 -exec rm {} \;
