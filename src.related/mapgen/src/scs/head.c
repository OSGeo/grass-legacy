#include <stdio.h>

main(argc,argv)
int argc;
char *argv[];
{
char c;
int i=0,n=0;
if (argc != 2) {
	fprintf(stderr,"syntax error: head [-count]\n");
	exit(0);
	}
n = -1 * atoi(argv[1]);
while (i<n && c!=EOF) {
	c=getchar();
	putchar(c);
	if (c == '\n') ++i;
	}
}
