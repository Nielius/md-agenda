#!/bin/bash
AGENDAFOLDER="$HOME/storage/shared/Documents/doc/notes/agenda"
FL="${AGENDAFOLDER}/$(date +%Y-W%V-%u-%H%M%S).md"
vim $FL
