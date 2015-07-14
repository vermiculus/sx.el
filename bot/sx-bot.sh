#!/usr/bin/env bash

[[ -z "$EMACS" ]] && EMACS="emacs";

DESTINATION_BRANCH=data
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

function notify-done {
    local title
    local message
    title="SX Tag Bot"
    message="Finished retrieving tag lists"
    case $(uname | tr '[[:upper:]]' '[[:lower:]]') in
        darwin)
            terminal-notifier \
                -message ${message} \
                -title ${title} \
                -sound default
            ;;
        *)
            echo ${message}
    esac
}

function generate-tags {
    $EMACS -Q --batch \
           -L "./" -L "./bot/" -l sx-bot \
           -f sx-bot-fetch-and-write-tags
    ret=$?
    notify-done
    return ${ret}
}

git checkout ${DESTINATION_BRANCH} &&
    git pull &&
    generate-tags &&
    git stage data/ &&
    git commit -m "Update tag data" &&
    git push &&
    echo 'Bot finished.'

git checkout ${CURRENT_BRANCH}
