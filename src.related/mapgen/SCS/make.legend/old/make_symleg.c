
/*  @(#)fill_leg.c     1.0  10/03/89   
 *  created by:         M.L.HOLKO, SCS
 *
 * Program will read standard input and create an ascii coordinate file
 * suitable for input to the mapgen programs proj and lines for
 * generating area fill paterns. This program was adapted from
 * snoop written by R. Glenn, SCS and plotmap written by P. Carlson, SCS.
 */
#include <stdio.h>
#include "pattern.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#define ERR -1

/* GLOBALS VARIABLES */
FILE *fopen(), *fp;

struct loc {
	float start,offset,size;
	} LX,LY;
float PEN=0.0, SIZE=0.2, LEAD=0.3;
float mult=1.0;
char FONT[15]="",str[10];

/*************** ask the user a question and read response ***********/
static void ask(x,c,ptr)
char *x,c;
char *ptr;
{
char line[128];
int i;

fprintf(stderr,"%s",x);
if (c != NULL) {
	gets(line);
	switch(c) {
		case 'c':
			*ptr = line[0];
			break;
		case 'f':
			sscanf(line,"%f",ptr);
			break;
		case 's':
			sscanf(line,"%s",ptr);
			break;
		case 'l':
			for (i=0;line[i] != NULL; i++)
				*(ptr++) = line[i];
			*ptr = NULL;
			break;
		}
	}
}



/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults()
{
char line[128], c;

	ask("\nEnter the pen number to use for the legend text: ",'f',&PEN);
	printf("-p %1.0f\n",PEN);
/*	fprintf(fp,"PEN=%1.0f\n",PEN);*/
		
	ask("Enter the font to use for the legend text [sr]: ",'s',FONT);
	printf("-f -%s\n",FONT);
/*	fprintf(fp,"FONT=%s\n",FONT);*/

	ask("\nThe coordinates can be given in centimeters or inches. \n",NULL,NULL);
	while (c != 'i' && c != 'c') {
		ask("Enter i for inches c for centimeters: ",'c',&c);
		}

	if (c == 'i') mult = 2.54;

	ask("Enter the size of the legend text: ",'f',&SIZE);
	SIZE = SIZE * mult;
	printf("-s %.3f\n",SIZE);
	printf("-l %.3f\n",SIZE * 1.25);
/*	fprintf(fp,"CHAR_SIZE=%-.3f\n",SIZE);*/

}
/***************** END setdefaults ******************************/

/******* get the starting cords and offsets *************/
getcord()
{
char line[100],c;


ask("\nAre the coordinates relative to the map or relative to the data area.\n",NULL,NULL);
while (c != 'p' && c != 'd') {
	ask("Enter p for map d for data: ",'c',&c);
	}
/*fprintf(fp,"window=%c\n",c);*/
printf("-w %c\n",c);

if (c == 'm') sprintf(str,"map");
else sprintf(str,"data");


ask("\nEnter the x position: ",'f',&LX.start);
/*fprintf(fp,"x=%-.3lf\n",LX.start);*/
LX.start = LX.start * mult;

ask("Enter the distance between the symbol and the legend text: ",'f',&LX.offset);
/*fprintf(fp,"x_offset=%-.3lf\n",LX.offset);*/
LX.offset = LX.offset * mult;

ask("\n\nEnter the y position: ",'f',&LY.start);
/*fprintf(fp,"y=%-.3lf\n",LY.start);*/
LY.start = LY.start * mult;
ask("Enter the vertical distance between legend entries: ",'f',&LY.offset);
/*fprintf(fp,"y_offset=%-.3lf\n",LY.offset);*/
LY.offset = LY.offset * mult;

}
/**************** END getcord *************************/


/************ start fill_leg *********/
main (argc,argv)
int argc;
char *argv[];

{
int cnt;
char c, line[120], sfont[10];
float x, y, s;



setdefaults();
getcord();
y=LY.start;
printf("-j l\n");
/* -------------- Get list of symbols to include -------------------------*/
s = 1;
while (s != 0) {
	ask("Enter the symbol number to use 0 to end: ",'f',&s);
	if (s == 0) continue;
	c = (int)s;
	x = LX.start;
	printf("-x %.3f\n-y %.3f\n",x,y);
	ask("Enter the font to use for this symbol: ",'s',sfont);
	printf("-f -%s\n",sfont);
	ask("Enter the pen number to use: ",'f',&s);
	printf("-p %.0f\n",s);
	ask("Enter the size for the symbol in centimeters: ",'f',&s);
	printf("-s %.3f\n",s);
	printf("-t\n%c\n.\n",c);
	x = x + LX.offset + SIZE;
	printf("-x %.3f\n-y %.3f\n",x,y);
	printf("-p %.0f\n",PEN);
	printf("-f -%s\n",FONT);
	ask("Enter one line of text for this symbol\n:",'l',line);
	printf("-s %.3f\n",SIZE);
	printf("-t\n%s\n.\n",line);
	y = y - LY.offset - SIZE;
	}
	
	
	

}
/*_______________________________________________________________________*/

