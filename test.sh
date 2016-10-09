#!/bin/bash
if [ -e snippets ]; then
    (cd snippets; git pull)
else
    git clone https://github.com/aheui/snippets
fi
(cd snippets && AHEUI="../_build/src/Aheui.native" bash test.sh standard)
