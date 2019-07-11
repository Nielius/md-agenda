#!/bin/bash
#
# Some functions to easily open my markdown agenda files
#
# TODO: function to open my activities files?

NIELIUSDOCDIR="${HOME}/doc/"

agenda() {
    # Open a specific agenda file (or open the working memory file).
    #
    # Usage:
    #
    # agenda		# interactively ask user for input
    # agenda [relative weeknum] [weekday] # relative weeknum is added to current week
    # agenda -a [absolute weeknum] [weekday]
    # agenda -t # to open today
    # agenda -w # to open working memory
    if (( $# < 1 ))
    then
        read -p "Week number (absolute):	" weeknum
        read -p "Day number:		" daynum
        $EDITOR "${NIELIUSDOCDIR}/notes/agenda/$(date +%G)-W${weeknum}-${daynum}.md"
    else
        if [[ $1 == "-a" ]]
        then
            weeknum=$2
            daynum=$3
            $EDITOR "${NIELIUSDOCDIR}/notes/agenda/$(date +%G)-W${weeknum}-${daynum}.md"
        elif [[ $1 == "-t" ]]
        then
             weeknum=$(date +%V)
             daynum=$(date +%u)
             $EDITOR "${NIELIUSDOCDIR}/notes/agenda/$(date +%G)-W${weeknum}-${daynum}.md"
        elif [[ $1 == "-w" ]]
        then
            $EDITOR "${NIELIUSDOCDIR}/notes/working-memory.md"

        else
            weeknum=$(( $(date +%V) + ${1} ))
            daynum=$2
            $EDITOR "${NIELIUSDOCDIR}/notes/agenda/$(date +%G)-W${weeknum}-${daynum}.md"
        fi
    fi

    # $EDITOR "${NIELIUSDOCDIR}/notes/agenda/$(date +%G)-W${weeknum}-${daynum}.md"
    # %G for iso year number (when used with %V)
    # %V for iso week number
    # %u for day of week
}

vimagenda() {
    # Function to open my agenda layout with vim.
    # I have similar functionality for emacs.
    #
    # Usage:
    #
    # vimagenda # open current week
    # vimagenda <weeknum> # open week <weeknum> (integer)
    #
    # Installation:
    # make sure the vimsesfile variable (see below) is set to the correct
    # file, and also make sure that that file points to the right doc directory.
    # (I see no good way of avoiding that requirement, although
    # maybe I could just use a second argument in the vim session.)
    vimsesfile="md-agenda-vim-session.vim"

    if (( $# < 1 ))
    then
        weeknum=$(date +%V)
        vim -S "$vimsesfile" $weeknum
    else
        vim -S "$vimsesfile" $1
    fi

    unset vimsesfile
}
