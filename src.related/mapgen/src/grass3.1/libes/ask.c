#include <stdio.h>
/*************** ask the user a question and read response ***********/
ask(x,c,ptr)
char *x,c;
char *ptr;
{
char line[128];

if (x[0] != ':') fprintf(stderr,"\n");
fprintf(stderr,"%s",x);
if (c != NULL) {
	gets(line);
	switch(c) {
		case 'c':
			*ptr = line[0];
			break;
		case 'f':
			if (line[0] == NULL) sprintf(line,"0.0");
			sscanf(line,"%f",ptr);
			break;
		case 'd':
			if (line[0] == NULL) sprintf(line,"0.0");
			sscanf(line,"%lf",ptr);
			break;
		case 's':
			sscanf(line,"%s",ptr);
			break;
		}
	}
}


