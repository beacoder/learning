learning emacs

1.kill-ring
descrpition: contains the kill history, used to copy paste cut, without size limit
key-bindings: "C-w" "M-w" "C-k" "C-y" "M-y"

rectangles: rectangle commands operate on rectangular areas of the text
key-bindings: 
"C-x r k" "C-x r M-w" "C-x r d" "C-x r c"
"C-x r y" "C-x r o" "C-x r N" "C-x r t"

2.history-ring
descrpition: contains shell command history
key-bindings: "C-r" "M-r" "M-p" "M-n"

3.register
descrpition: use a char or number to represent a register, used to save positon text numer
note: R -> stands for your register name
key-bindings:
"C-x r <SPC> R" "C-x r s R" "C-x r n R" "C-x r w R" "C-x r r R"
"C-x r i R" "C-x r j R"

4.mark-ring
descrpition: used to save the cursor position
key-bindings: "C-<SPC> C-<SPC>" "C-u C-<SPC>"

5.keyboard macro-ring
descrpition: all defined keyboard macros are recorded in the keyboard macro ring
key-bindings:
"<F3>" "<F4>" "C-x (" "C-x )" "C-x e"
"C-x C-k C-k" "C-x C-k C-n" "C-x C-k C-p"
"C-x C-k C-i" "C-x C-k C-c" "C-x C-k C-a" "C-x C-k C-f"
"C-x q"
"C-x C-k n" "C-x C-k b"
"C-x C-k C-e" "C-x C-k e name <RET>" "C-x C-k l"
"C-x C-k <SPC>"

6.undo and redo
undo: "C-_"
redo: "C-g" "C-_"

7.deal with ispell error "no word lists can be found for the language "zh_cn"."
export LANG=en_US.UTF-8
