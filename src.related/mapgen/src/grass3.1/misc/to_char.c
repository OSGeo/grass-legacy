#include <stdio.h>
#define ERR -1

main(argc,argv)
int argc;
char *argv[];

{
int num;
char c;

if (argc != 2){
	fprintf(stderr,"USAGE: to_char num\n");
	exit(ERR);
	}
sscanf(argv[1],"%d",&num);
c = (char)num;
printf("%c",c);
}
