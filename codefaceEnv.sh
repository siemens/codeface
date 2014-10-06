#!/bin/bash

export PYTHONPATH="~/.local/lib64/python3.3/site-packages:~/.local/lib64/python3.3:$PYTHONPATH"
export PATH="~/.local/bin:$PATH"
export PS1="[codeface] $PS1"
echo "if you sourced this file you should be able to run:"
echo "codeface --loglevel debug --logfile codeface.log run -p conf/linux-kernel.conf -c codeface.conf ~/projects/codeface/res ~/projects/git-repos"
