#!/usr/bin/env bash

set -eu

# @getoptions
parser_definition() {
  setup REST help:usage -- "Usage: emacsctl [options]... [arguments]..." ''
  msg                                              -- 'Options:'
  flag  INSERT                 -i --insert         -- "Perform insertion"
  flag  REPLACE                -r --replace        -- "Perform replacement"
  param BUFFER                 -b --buffer         -- "Buffer for insertion or replacement. Current buffer by default"
  flag  SAVE                   -s --save           -- "Save the buffer after insertion or replacement"
  param INSERT_POSITION        -p --position       -- "Position of insertion"
  param INSERT_LINE            -l --line           -- "Line of insertion"
  param INSERT_COLUMN          -c --column         -- "Column of insertion"
  param REPLACE_START_POSITION    --start-position -- "Start position of replacement"
  param REPLACE_END_POSITION      --end-position   -- "End position of replacement"
  param REPLACE_START_LINE        --start-line     -- "Start line of replacement"
  param REPLACE_END_LINE          --end-line       -- "End line of replacement"
  disp  :usage                 -h --help
}
# @end

# @gengetoptions parser -i parser_definition parse
# @end

parse "$@"
eval "set -- $REST"

set +u

PORT=33865
WAIT=30
CONN_FAULT='Cannot connect to Emacs. Ask user to setup Emacs first.'

PLIST="("
REPLACE_RANGE=0

if [ -n "$BUFFER" ]; then
    PLIST="$PLIST :buffer \"$BUFFER\""
fi
if [ "$SAVE" = "1" ]; then
    PLIST="$PLIST :save-p t"
fi
if [ -n "$INSERT_POSITION" ]; then
    PLIST="$PLIST :position $INSERT_POSITION"
fi
if [ -n "$INSERT_LINE" ]; then
    PLIST="$PLIST :line $INSERT_LINE"
fi
if [ -n "$INSERT_COLUMN" ]; then
    PLIST="$PLIST :column $INSERT_COLUMN"
fi
if [ -n "$REPLACE_START_POSITION" ]; then
    PLIST="$PLIST :start-position $REPLACE_START_POSITION"
    REPLACE_RANGE=1
fi
if [ -n "$REPLACE_END_POSITION" ]; then
    PLIST="$PLIST :end-position $REPLACE_END_POSITION"
    REPLACE_RANGE=1
fi
if [ -n "$REPLACE_START_LINE" ]; then
    PLIST="$PLIST :start-line $REPLACE_START_LINE"
    REPLACE_RANGE=1
fi
if [ -n "$REPLACE_END_LINE" ]; then
    PLIST="$PLIST :end-line $REPLACE_END_LINE"
    REPLACE_RANGE=1
fi

PLIST="$PLIST )"

if [ "$INSERT" = "1" ] && [ "$REPLACE" = "1" ]; then
    echo "Cannot specify both --insert and --replace options." >&2
    exit 1
elif [ "$INSERT" = "1" ]; then
    if [ -e "$1" ]; then
        { printf "%s\23" "$PLIST"; cat "$1"; printf '\2'; } |
            { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    elif [ -n "$1" ]; then
        { printf "%s\23%s\2" "$PLIST" "$1"; } |
            { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    else
        { printf "%s\23" "$PLIST"; cat; printf '\2'; } |
            { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    fi
elif [ "$REPLACE" = "1" ]; then
    {
        printf "%s\23" "$PLIST"
        if [ -e "$1" ]; then
            cat "$1"
        elif [ -n "$1" ]; then
            printf "%s" "$1";
        elif [ $REPLACE_RANGE -eq 1 ]; then
            cat;
        else
            echo "Wrong format of --replace. Provide a file, a string, or a range to replace." >&2
            exit 1
        fi
        if [ $REPLACE_RANGE -eq 0 ]; then
            printf '\23'
            if [ -e "$2" ]; then
                cat "$2"
            elif [ -n "$2" ]; then
                printf "%s" "$2";
            else
                cat;
            fi
        fi
        printf '\32'
    } | { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
else
    if [ -e "$1" ]; then
        { cat "$1"; printf '\4'; } |
            { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    elif [ -n "$1" ]; then
        { printf "%s\4" "$1"; } |
            { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    else
        { cat; printf '\4'; } | { nc -w $WAIT localhost $PORT && echo || echo "$CONN_FAULT"; }
    fi
fi
