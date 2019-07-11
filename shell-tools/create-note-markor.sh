#!/bin/bash
AGENDAFOLDER="$HOME/storage/shared/Documents/doc/notes/agenda"
FL="${AGENDAFOLDER}/$(date +%Y-W%V-%u-%H%M%S).md"
touch $FL
termux-share -d $FL # -d is for the default
