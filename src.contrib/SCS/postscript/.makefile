N=tbl -TX man.version /develop/grass4.1/src/../man/utilities/man.header $? | nroff | col -b > $@
all: list
cat1/ps.icons: man1/ps.icons ;  $N
cat1/ps.map: man1/ps.map ;  $N
cat1/ps.select: man1/ps.select ;  $N
cat1/Gmakefile: man1/Gmakefile ;  $N
list:  cat1/ps.icons cat1/ps.map cat1/ps.select cat1/Gmakefile
