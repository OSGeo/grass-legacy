
#include <stdio.h>
#include <ctype.h>
#include "mdef.h"




p_get_cord(str,x,y,xr,yr,p)
char str[];
double *x,*y;
char *xr,*yr;
int p;

{
char line[100],c;
float F;

c = NULL;
sprintf(line,"Are the x coordinates relative to left, right or middle of the %s\n",str);

while (c != 'l' && c != 'r' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter [l], r, or m: ",'c',&c);
	if (c == NULL) c = 'l';
	if (isupper(c)) c = tolower(c);
	}

switch (c) {
	case 'l':
		*xr = NULL;
		break;
	case 'r':
		*xr = '>';
		break;
	case 'm':
		*xr = '|';
		break;
	}
ask("Enter the x distance: ",'f',&F);
if (p) printf("-x %c%.3f\n",*xr,F);
*x = F;
c = NULL;
sprintf(line,"Are the y coordinates relative to bottom, top or middle of the %s\n",str);

while (c != 't' && c != 'b' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter [b], t, or m: ",'c',&c);
	if ( c == NULL) c = 'b';
	if (isupper(c)) c = tolower(c);
	}

switch (c) {
	case 't':
		*yr = '>';
		break;
	case 'b':
		*yr = NULL;
		break;
	case 'm':
		*yr = '|';
		break;
	}
ask("Enter the y distance: ",'f',&F);
if (p) printf("-y %c%.3f\n",*yr,F);
*y = F;

}
/**************** END p_get_cord *************************/
