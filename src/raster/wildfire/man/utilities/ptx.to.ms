cat << EOF
.\" Process this file "permute.ms" with:
.\" cat ../permuted.index | ptx* | tbl | troff -ms | lpr -t
.TL
GRASS 4.1 Permuted Index
.AB
This permuted index references GRASS 4.1 main and alpha commands.
See your GRASS adminstrator if you require a command which has not been
compiled on your machine.
.AE
.LP
EOF
ptx -f -t -r | (
	echo .pl 1
	echo .nf
	echo .de xx
	echo '\\$2@\\$3@\\$5'
	echo ..
	cat
    ) | nroff | (
	echo ".nr PS -3"
	echo ".nr VS -3"
	echo ".ps -3"
	echo ".vs -3"
	echo .TS
	echo 'expand tab(@);'
	echo rlln.
	awk '{print $0; if(++i%5==0)print ".sp .5"}'
	echo .TE
	echo ".nr PS +3"
	echo ".nr VS +3"
	echo ".ps +3"
	echo ".vs +3"
    )
