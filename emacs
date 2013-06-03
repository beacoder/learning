learning emacs

1.kill-ring
descrpition: contains the kill history, used to copy paste cut, without size limit
key-bindings: "C-w" "M-w" "C-k" "C-y" "M-y"

2.history-ring
descrpition: contains shell command history
key-bindings: "C-r" "M-r" "M-p" "M-n"

3.register
descrpition: use a char or number to represent a register, used to save positon text numer
note: R -> stands for your register name
key-bindings: "C-x r <SPC> R" "C-x r s R" "C-x r n R" "C-x r i R" "C-x r j R"

4.mark-ring
descrpition: used to save the cursor position
key-bindings: "C-<SPC> C-<SPC>" "C-u C-<SPC>"

5.keyboard macro-ring
descrpition: all defined keyboard macros are recorded in the keyboard macro ring
key-bindings: "<F3>" "<F4>" "C-x C-k C-k" "C-x C-k C-n" "C-x C-k C-p"

6.undo and redo
undo: "C-_"
redo: "C-g" "C-_"

7.deal with ispell error "no word lists can be found for the language "zh_cn"."
export LANG=en_US.UTF-8

