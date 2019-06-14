#!/bin/bash
#
# Some functions to easily open my markdown agenda files

NIELIUSDOCDIR="${HOME}/doc/"

agenda() {
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
