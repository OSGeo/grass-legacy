#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "mdef.h"
#define MAPDEF "map.def"
#define OVER "ovm/`basename $0`"

/* GLOBALS VARIABLES */
float PEN=0.0, SIZE=0.2, LEAD=0.3;
char mapdef[50], FONT[15]="",str[10];




/******* get the window and the coordinate location *************/
getcord()
{
char c,xr,yr;
double x,y,x1,y1;

while ( c != 'y' && c != 'n') {
	ask("Do you want to select the coordinates on the screen ([y]es or (n)o): ",'c',&c);
	if (c == NULL) c = 'y';
	if (isupper(c)) c = tolower(c);
	}
if (c == 'y') {
	if (str[0] == 'd'){
 		get_margin(&x,&y,mapdef);
  		d_get_cord(x,y,&x1,&y1,1);
  		}
	else
    		d_get_cord(0.0,0.0,&x1,&y1,1);
	}
else
	p_get_cord(str,&x1,&y1,&xr,&yr,1);
while ( c != 'l' && c != 'r' && c != 'c') {
	ask("The text can be left, right, or center justified at this coordinate.\nEnter [l] r or c: ",'c',&c);
	if (c == NULL) c = 'l';
	}
printf("-j %c\n",c);
}
/**************** END getcord *************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults(n)
int n;
{
char c='y',line[128];
float FL;

while (c != NULL) {
if (n != 0)  {
	sprintf(line,"Defaults:\nwindow:\t\t%s\npen:\t\t%1.0f\nfont:\t\t%s\nsize:\t\t%.3f\nleading:\t%.3f\n",str,PEN,FONT,SIZE,LEAD);
	ask(line,NULL,NULL);
	ask("Type return to continue or \nTo change a default enter (w)indow (p)en (f)ont (s)ize (l)eading: ",'c',&c);
	}
else c='a';
	switch (c) {
		case 'a':
		case 'p':
			ask("Enter the pen number to use for this text [1]: ",'f',&PEN);
			if (PEN == 0) PEN = 1.0;
			printf("-p %1.0f\n",PEN);
			if (n != 0) break;
		case 'f':
			ask("Enter the font to use [sr]: ",'s',FONT);
			if (FONT[0] == NULL) sprintf(FONT,"sr");
			printf("-f -%s\n",FONT);
			if (n != 0) break;
		case 's':
			ask("Enter the size of the text [.2]: ",'f',&SIZE);
			if (SIZE == 0) SIZE = 0.2;
			if (SIZE < 0.5) LEAD = SIZE + .1;
			else LEAD = SIZE * 1.2;
			printf("-s %.3f\n",SIZE);
			printf("-l %.3f\n",LEAD);
			if (n != 0) break;
		case 'l':
			sprintf(line,"Enter the leading for the text [%.2f]: ",LEAD);
			ask(line,'f',&FL);
			if (FL > 0.0) LEAD = FL;
			printf("-l %.3f\n",LEAD);
			if (n != 0) break;
		case 'w':
			c = get_pwin();
			printf("-w %c\n",c);
			if (c == 'p') sprintf(str,"plot");
			else sprintf(str,"data");
			if (n == 0) c=NULL;
		}
	}
}
/***************** END setdefaults ******************************/

main (argc,argv)
int argc;
char *argv[];

{
int cnt;
char overlay[50];
char c, line[120];
float F;

sprintf(mapdef,MAPDEF);
sprintf(overlay,OVER);
/* which arguement */
for (cnt=1; cnt < argc; cnt++)
	{
	if (*argv[cnt] == '-') {
		c = *(argv[cnt]+1);
		switch(c) {
			case 'm':
				sscanf(argv[++cnt],"%s",mapdef);
				break;
			default:
				fprintf(stderr,"Error: make.text [-m mapdef]\n");
				exit(ERR);
				break;
		
			}
		}
	}
printf("legend %s -o %s << EOF\n",mapdef,overlay);



setdefaults(0);
cnt = 1;
while (cnt)
	{

	ask("\nNow enter the information for the block of text \n",NULL,NULL);

	getcord();

	ask("\nEnter the text one line at a time as you want it on the plot.\n",NULL,NULL);
	ask("Enter . as the first character of the line to end this block of text.\n\n",NULL,NULL);
	printf("-t\n");
	line[0]='0';
	while (line[0] != '.') {
		ask(":",NULL,NULL);
		gets(line);
		printf("%s\n",line);
		}
	c = NULL;
	while (c != 'y' && c != 'n') {
		ask("MORE([y]es or (n)o):",'c',&c);
		if (c == NULL) c = 'y';
		if (isupper(c)) tolower(c);
		}
		if (c != 'y') cnt = 0;
		else setdefaults(1);

	}
printf("EOF\n");
}
