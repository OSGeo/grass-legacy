set home = /home/pasture5/rewerts
set history=30 noclobber ignoreeof
set prompt="\! GRASS4 $cwd-> "
alias cd 'set old=$cwd;chdir \!*;set prompt = "\! GRASS4 $cwd->  "'
alias bk 'set back=$old;set old=$cwd;chdir $back;set prompt = "\! GRASS4 $cwd-> "'
alias h history
alias j jobs
alias lr 'g.list rast'
alias lv 'g.list vect'
