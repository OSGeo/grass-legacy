#include <stdio.h>

main(argc,argv)
int argc;
char *argv[];

{
char start[256];
int i;

i = argc;
while (--i > 0){
	strcat(start,argv[i]);
	if (start[0] == '+')
	  if (start[1] == 'p')
	    if (start[2] == 'r')
	     if (start[3] == 'o')
		if (start[4] == 'j')
		  if (start[5] == '='){
			printf("%s\n",&start[6]);
			exit(0);
			}
	start[0]=NULL;
	}
}
