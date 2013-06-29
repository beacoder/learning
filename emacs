learning emacs

1.kill-ring
descrpition: contains the kill history, used to copy paste cut, without size limit
key-bindings: "C-w" "M-w" "C-k" "C-y" "M-y"

2.rectangle
descrpition: commands operate on rectangular areas of the text
key-bindings: 
"C-x r k" "C-x r M-w" "C-x r d" "C-x r c"
"C-x r y" "C-x r o" "C-x r N" "C-x r t"

3.history-ring
descrpition: contains shell command history
key-bindings: "C-r" "M-r" "M-p" "M-n"

4.register
descrpition: use a char or number to represent a register, used to save positon text numer
note: R -> stands for your register name
key-bindings:
"C-x r <SPC> R" "C-x r s R" "C-x r n R" "C-x r w R" "C-x r r R"
"C-x r i R" "C-x r j R"

5.mark-ring
descrpition: used to save the cursor position
key-bindings: "C-<SPC> C-<SPC>" "C-u C-<SPC>"

6.keyboard macro-ring
descrpition: all defined keyboard macros are recorded in the keyboard macro ring
key-bindings:
"<F3>" "<F4>" "C-x (" "C-x )" "C-x e"
"C-x C-k C-k" "C-x C-k C-n" "C-x C-k C-p"

7.undo and redo
undo: "C-_"
redo: "C-g" "C-_"

8.deal with ispell error "no word lists can be found for the language "zh_cn"."
export LANG=en_US.UTF-8

9.magit 
descrpition: magit-mode is an excellent interface to the git version control system
commands and key-bindings:
"magit-status"  see git status
"s"             add the file under cursor to stage
"S"             add all tracked files to stage
"u"             unstaged the file under cursor
"U"             unstaged all staged files
"c"             write a commit message
"C-c C-c"       commit
"PP"            git-push
"magit-log"     see commit logs
"magit-pull"    pull updates from git repo
"magit-push"    push updates to git repo
