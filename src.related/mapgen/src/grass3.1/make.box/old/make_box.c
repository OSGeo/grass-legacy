#include <stdio.h>
#include <string.h>
#define ERR 1
#define MAPDEF "map.def"
#define OVER "ovm/box"

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
char line[100],c,xr[2],yr[2];
float x[2],y[2];
int i;

for (i=0;i<2;i++) {
if (i == 0)
	sprintf(line,"\nEnter the information for the lower left corner of the box\n\n");
else 
	sprintf(line,"Enter the information for the upper right corner of the box\n");
ask(line,NULL,NULL);
sprintf(line,"Is the x coordinate relative to left, right or middle of the %s\n",str);

while (c != 'l' && c != 'r' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter l, r, or m: ",'c',&c);
	}

switch (c) {
	case 'l':
		xr[i] = NULL;
		break;
	case 'r':
		xr[i] = '>';
		break;
	case 'm':
		xr[i] = '|';
		break;
	}
ask("Enter the x coordinate: ",'f',&x[i]);

sprintf(line,"Are the y coordinates relative to top, bottom or middle of the %s\n",str);

while (c != 't' && c != 'b' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter t, b, or m: ",'c',&c);
	}

switch (c) {
	case 't':
		yr[i] = '>';
		break;
	case 'b':
		yr[i] = NULL;
		break;
	case 'm':
		yr[i] = '|';
		break;
	}
ask("Enter the y coordinate: ",'f',&y[i]);
}
printf("%c%.3f %c%.3f\n",xr[0],x[0] * mult,yr[0],y[0] * mult);
printf("%c%.3f %c%.3f\n",xr[1],x[1] * mult,yr[0],y[0] * mult);
printf("%c%.3f %c%.3f\n",xr[1],x[1] * mult,yr[1],y[1] * mult);
printf("%c%.3f %c%.3f\n",xr[0],x[0] * mult,yr[1],y[1] * mult);
printf("%c%.3f %c%.3f\n",xr[0],x[0] * mult,yr[0],y[0] * mult);
printf(".\n");
}
/**************** END getcord *************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults(n)
int n;
{
char c;
	ask("Are the coordinates for this box relative to the map or \nrelative to the data area.\n",NULL,NULL);
	while (c != 'p' && c != 'd') {
		ask("Enter p for map d for data: ",'c',&c);
		}
	printf("-w %c\n",c);
	if (c == 'm') sprintf(str,"map");
	else sprintf(str,"data");
	ask("Enter the pen number to use for box: ",'f',&PEN);
	printf("-L %1.0f\n",PEN);
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
				fprintf(stderr,"Error: make.box [-m mapdef] [-o overlay]\n");
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
getcord(mult);
printf("EOF\n");
}
