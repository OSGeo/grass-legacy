
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#define ERR 1
#define MAPDEF "map.def"
#define OVERLAY "ovm/`basename $0`"
#define MAXUNIT 4

/* GLOBALS VARIABLES */
float PEN=0.0, SIZE=0.2, LEAD=0.3, LENGTH=3.0;
double x,y;
char xr,yr;
char mapdef[50],FONT[15]="";
struct STR {
	char *name;
	float val;
	};

struct STR unit[] = { 
			"ft",30.48,
			"mi",160934.40,
			"km",100000,
			"m",100,};



/******* get the window and the coordinate location *************/
getcord()
{
char line[100],c,w,str[10];
double x1,y1;

w = get_pwin();
printf("-w %c\n",w);
if (w == 'p') sprintf(str,"plot");
else sprintf(str,"data");

ask("Enter the coordinates for the CENTER of the bar scale\n",NULL,NULL);
while ( c != 'y' && c != 'n') {
        ask("Do you want to select the cordinates on the screen ([y]es or (n)o): ",'c',&c);
        if (c == NULL) c = 'y';
        if (isupper(c)) c = tolower(c);
        }
if (c == 'y') {
        if (w == 'd'){
                get_margin(&x1,&y1,mapdef);
                d_get_cord(x1,y1,&x,&y,0);
                }
        else
                d_get_cord(0.0,0.0,&x,&y,0);
        }
else {
        p_get_cord(str,&x,&y,&xr,&yr,0);
        }
}

/**************** END getcord *************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults(n)
int n;
{
char c;
char str[10], cmd[80];

sprintf(str,"cm");



ask("Enter the pen number to use for the scale [1]: ",'f',&PEN);
if (PEN == 0.0) PEN = 1.0;
printf("-p %1.0f\n",PEN);
ask("Enter the font to use [sr]: ",'s',FONT);
printf("-f -%s\n",FONT);
ask("Enter the size of the text for the barscale [.2]: ",'f',&SIZE);
if (SIZE == 0.0) SIZE = 0.2;
printf("-s %.3f\n",SIZE);
if (SIZE < 0.5) LEAD = SIZE + 0.1;
else LEAD = SIZE * 1.2;
sprintf(cmd,"Enter the leading for the text [%.2f]: ",LEAD);
ask(cmd,'f',&LEAD);

printf("-l %.3f\n",LEAD);

}
/***************** END setdefaults ******************************/

main (argc,argv)
int argc;
char *argv[];

{
int cnt, j;
char line[50],cmd[50],cmd2[100];
char c;
double scale = 100000.0, dist, u, v, size, rep, left, px;

sprintf(mapdef,MAPDEF);
sprintf(line,OVERLAY);

/* which arguement */
for (cnt=1; cnt < argc; cnt++)
	{
	if (*argv[cnt] == '-') {
		c = *(argv[cnt]+1);
		switch(c) {
			case 'm':
				sscanf(argv[++cnt],"%s",mapdef);
				break;
			case 's':
				sscanf(argv[++cnt],"%lf",&scale);
				break;
			default:
				fprintf(stderr,"Error: make_bscale [-m mapdef] [-s scale]\n");
				exit(ERR);
				break;
		
			}
		}
	}
printf("legend -m %s -o %s << EOF\n",mapdef,line);

setdefaults();

getcord();

sprintf(cmd,"Enter the unit for this bar scale ");
for (cnt=0;cnt<MAXUNIT;cnt++) {
	strcat(cmd,unit[cnt].name);
	if (cnt != MAXUNIT) strcat(cmd,", ");
	}
ask(cmd,NULL,NULL);
line[0] = 's';
while (line[0] != '.') {
	ask("\n(ft mi km m or . to quit)\n: ",'s',line);
	if (line[0] != '.') {
	for (cnt=0;cnt<MAXUNIT;cnt++) {
		if ( !strncmp(unit[cnt].name,line,2) ) {
			dist = unit[cnt].val/scale;
			sprintf(cmd2,"Each %s measures %.4lf centimeters.\nHow many %s do you want in each unit [1]: ",line,dist,line);
			ask(cmd2,'d',&size);
			if (size == 0.0) size = 1.0;
			dist = dist * size;
			sprintf(cmd2,"Each unit measures %.2lf centimeters.\nHow many units do you want to the right of 0 [1]: ",dist);
			ask(cmd2,'d',&rep);
			if (rep == 0.0) rep = 1.0;
			printf("Each unit measures %.2lf centimeters.\nYou can have one unit drawn to the left\nof the zero point with subdivisions\n",dist);
			sprintf(cmd2,"How many subdivisions for the unit to the left of zero [no units to the left]: ");
			ask(cmd2,'d',&left);
			px = x - (dist * (rep +1.0))/2.0 + dist;
			printf("-x %c%.3f\n-y %c%.3f\n",xr, px ,yr, y );
			printf("-b %.2lf,%s,%.0lf,%.0lf\n",size,line,rep,left);
			}
		}
	y = y - (2.2 * LEAD);
	}
	}
printf("EOF");
}
