:
if test $# = 0
then
	echo Usage: `basename $0` cellfiles >&2
	exit 1
fi
Gcovar "$@"  | awk '{
	n=NF
	for(i=1;i<=n;i++) matrix[i + n*(NR-1)] = $i}
	END {
	    for(j=1;j<=n;j++) {
		jj = matrix[j + n*(j-1)]
		for(i=1;i<=n;i++){
		    ii = matrix[i + n*(i-1)]
		    ij = matrix[i + n*(j-1)]
		    printf ("%lf", ij/sqrt(ii*jj))
		    if(i==n) printf("\n")
		    else printf(" ")
		}
	    }
	}'
