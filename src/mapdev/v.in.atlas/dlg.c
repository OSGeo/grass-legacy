/* Single program that converts atlas lat/long files to grass albers files. */

/* dlg.c */

#include <stdio.h>
#include <string.h>

FILE *inp;

main(argc,argv)
int argc;
char *argv[];
{
	char c;
	int ctr=0;

	if((inp=fopen(argv[1],"r"))==NULL)
		exit(-1);

	while (c!=EOF) 
	{
		c=getc(inp);
		if(c == 'L')
			ctr++;
	} 

	putchar(ctr);
	fclose(inp);
	exit(0);
}
