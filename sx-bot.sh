#!/usr/bin/bash

git branch gh-pages &&
    git pull &&
    emacs -Q --batch -L "./" -l sx-bot -f sx-bot-fetch-and-write-tags &&
    git commit . &&
    git push &&
    echo SUCCESS
