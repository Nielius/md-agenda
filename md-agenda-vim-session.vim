" This is a script that opens a set of agenda files in a specific layout.
" Usage:
"    vim -S nameofthisfile <weeknum>
" where <weeknum> is an integer that denotes the number of the weekday
let weeknum = argv(0)
cd ~/doc/notes/agenda
set splitbelow splitright
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
2wincmd k
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd w
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd w
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
normal! 0
wincmd w
7wincmd w
exe '1resize ' . ((&lines * 17 + 27) / 54)
exe 'vert 1resize ' . ((&columns * 105 + 105) / 210)
exe '2resize ' . ((&lines * 17 + 27) / 54)
exe 'vert 2resize ' . ((&columns * 104 + 105) / 210)
exe '3resize ' . ((&lines * 17 + 27) / 54)
exe 'vert 3resize ' . ((&columns * 105 + 105) / 210)
exe '4resize ' . ((&lines * 17 + 27) / 54)
exe 'vert 4resize ' . ((&columns * 104 + 105) / 210)
exe '5resize ' . ((&lines * 16 + 27) / 54)
exe 'vert 5resize ' . ((&columns * 105 + 105) / 210)
exe '6resize ' . ((&lines * 16 + 27) / 54)
exe 'vert 6resize ' . ((&columns * 53 + 105) / 210)
exe '7resize ' . ((&lines * 16 + 27) / 54)
exe 'vert 7resize ' . ((&columns * 50 + 105) / 210)
" This seems to add the files to the buffers. Not sure why it is necessary.
" badd +0 2019-W${week}-1.md
" add +0 2019-W${week}-2.md
" badd +0 2019-W${week}-3.md
" add +0 2019-W${week}-4.md
" badd +0 2019-W${week}-5.md
" add +0 2019-W${week}-6.md
" badd +0 2019-W${week}-7.md
wincmd k
wincmd k
wincmd h
exec 'edit 2019-W' . weeknum . '-1.md'
wincmd j
exec 'edit 2019-W' . weeknum . '-2.md'
wincmd j
exec 'edit 2019-W' . weeknum . '-3.md'
wincmd k
wincmd k
wincmd l
exec 'edit 2019-W' . weeknum . '-4.md'
wincmd j
exec 'edit 2019-W' . weeknum . '-5.md'
wincmd j
" This always moves to the second last window:
wincmd l
wincmd l
wincmd h
exec 'edit 2019-W' . weeknum . '-6.md'
wincmd l
exec 'edit 2019-W' . weeknum . '-7.md'
