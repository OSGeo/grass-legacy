#include <stdio.h>

main()
{
char c;
int linenum=0,k;
FILE *fd, *fopen(), *fclose();

	fd = fopen("hello","r");

	k = random(fd);
	printf("k : %d\n", k);
	
	while((c = getc(fd)) != EOF) 
		if(c == '\n')
			linenum++;

	printf("%d\n",linenum);
}
