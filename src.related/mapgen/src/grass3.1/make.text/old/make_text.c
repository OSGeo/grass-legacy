#include <stdio.h>
#include <string.h>
#define ERR 1
#define MAPDEF "map.def"
#define OVER "ovm/text"

/* GLOBALS VARIABLES */
float PEN=0.0, SIZE=0.2, LEAD=0.3;
float mult=1.0;
char FONT[15]="",str[10];

/*************** ask the user a question and read response ***********/
static void ask(x,c,ptr)
char *x,c;
char *ptr;
{
char line[128];

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
		}
	}
}

/******* get the window and the coordinate location *************/
getcord()
{
char line[100],c,xr,yr;
float F;

sprintf(line,"Are the y coordinates relative to top, bottom or middle of the %s\n",str);

while (c != 't' && c != 'b' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter t, b, or m: ",'c',&c);
	}

switch (c) {
	case 't':
		yr = '>';
		break;
	case 'b':
		yr = NULL;
		break;
	case 'm':
		yr = '|';
		break;
	}
ask("Enter the y distance: ",'f',&F);
printf("-y %c%.3f\n",yr,F * mult);

sprintf(line,"Are the x coordinates relative to left, right or middle of the %s\n",str);

while (c != 'l' && c != 'r' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter l, r, or m: ",'c',&c);
	}

switch (c) {
	case 'l':
		xr = NULL;
		break;
	case 'r':
		xr = '>';
		break;
	case 'm':
		xr = '|';
		break;
	}
ask("Enter the x distance: ",'f',&F);
printf("-x %c%.3f\n",xr,F * mult);
ask("The text can be left, right, or center justified at this coordinate.\nEnter l r or c: ",'c',&c);
printf("-j %c\n",c);
}
/**************** END getcord *************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults(n)
int n;
{
char c='y',line[128];

while (c != NULL) {
if (n != 0)  {
	sprintf(line,"Defaults:\nwindow:\t\t%s\npen:\t\t%1.0f\nfont:\t\t%s\nsize:\t\t%.3f\nleading:\t%.3f\n",str,PEN,FONT,SIZE,LEAD);
	ask(line,NULL,NULL);
	ask("To change a default enter (w)indow (p)en (f)ont (s)ize (l)eading: ",'c',&c);
	}
else c='a';
	switch (c) {
		case 'a':
		case 'p':
		ask("Enter the pen number to use for this text: ",'f',&PEN);
		printf("-p %1.0f\n",PEN);
		if (n != 0) break;
		case 'f':
		ask("Enter the font to use [sr]: ",'s',FONT);
		printf("-f -%s\n",FONT);
		if (n != 0) break;
		case 's':
		ask("Enter the size of the text: ",'f',&SIZE);
		printf("-s %.3f\n",SIZE * mult);
		if (n != 0) break;
		case 'l':
		ask("Enter the leading for the text: ",'f',&LEAD);
		printf("-l %.3f\n",LEAD * mult);
		if (n != 0) break;
		case 'w':
		ask("Are the coordinates for this text relative to the map or \nrelative to the data area.\n",NULL,NULL);
		while (c != 'p' && c != 'd') {
			ask("Enter p for map d for data: ",'c',&c);
			}
		printf("-w %c\n",c);
		if (c == 'p') sprintf(str,"map");
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
char mapdef[50], overlay[50];
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
			case 'o':
				sscanf(argv[++cnt],"%s",overlay);
				break;
			default:
				fprintf(stderr,"Error: make.text [-m mapdef] [-o overlay]\n");
				exit(ERR);
				break;
		
			}
		}
	}
printf("legend %s -o %s << EOF\n",mapdef,overlay);

ask("The coordinates can be given in centimeters or inches. \n",NULL,NULL);
while (c != 'i' && c != 'c') {
	ask("Enter i for inches c for centimeters: ",'c',&c);
	}
if (c == 'i') mult = 2.54;


setdefaults(0);
while (cnt)
	{

	ask("\nNow enter the information for the block of text \n\n",NULL,NULL);

	getcord(mult);

	ask("\nEnter the text one line at a time as you want it on the plot.\n",NULL,NULL);
	ask("Enter . as the first character of the line to end this block of text.\n\n",NULL,NULL);
	printf("-t\n");
	line[0]='0';
	while (line[0] != '.') {
		ask(":",NULL,NULL);
		gets(line);
		printf("%s\n",line);
		}

	ask("MORE(y or n):",'c',&c);
	if (c != 'y') cnt = 0;
	else setdefaults(1);
	}
printf("EOF\n");
}
