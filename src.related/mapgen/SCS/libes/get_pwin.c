
/*  %W%  %G%  */


#define ERR -1
#include <stdio.h>
#include <ctype.h>

char get_pwin()
{
char c,w,line[100];
int i;

while ( w != 'p' && w != 'd') {
	fprintf(stderr,"\nDo you want the coordinates relative to the plot\n");
	fprintf(stderr,"or relative to the data area ([p] for plot (d) for data area): ");
        gets(line);
        if (line[0] == NULL) w = 'p';
        else w = line[0];
        if (isupper(w)) tolower(w);
        }
return(w);
}
