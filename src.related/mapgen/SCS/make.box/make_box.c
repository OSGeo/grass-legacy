#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define ERR 1
#define MAPDEF "map.def"
#define OVER "ovm/`basename $0`"

/* GLOBALS VARIABLES */
float PEN=0.0, SIZE=0.2, LEAD=0.3;
char mapdef[50], FONT[15]="",str[10];



/******* get the window and the coordinate location *************/
getcord()
{
char line[100],c,xr[2],yr[2];
double x[2],y[2],xm,ym;
int i;

for (i=0;i<2;i++) {
xr[i] = yr[i] = NULL;
x[i] = y[i] = 0.0;
if (i == 0)
	sprintf(line,"\nEnter the information for the lower left corner of the box\n");
else 
	sprintf(line,"Enter the information for the upper right corner of the box\n");
ask(line,NULL,NULL);

while ( c != 'y' && c != 'n') {
        ask("Do you want to select the cordinates on the screen ([y]es or (n)o): ",'c',&c);
        if (c == NULL) c = 'y';
        if (isupper(c)) c = tolower(c);
        }
if (c == 'y') {
        if (str[0] == 'd'){
                get_margin(&xm,&ym,mapdef);
                d_get_cord(xm,ym,&x[i],&y[i],0);
                }
        else
                d_get_cord(0.0,0.0,&x[i],&y[i],0);
        }
else
        p_get_cord(str,&x[i],&y[i],&xr[i],&yr[i],0);
}
 
printf("%c%.3f %c%.3f\n",xr[0],x[0],yr[0],y[0]);
printf("%c%.3f %c%.3f\n",xr[1],x[1],yr[0],y[0]);
printf("%c%.3f %c%.3f\n",xr[1],x[1],yr[1],y[1]);
printf("%c%.3f %c%.3f\n",xr[0],x[0],yr[1],y[1]);
printf("%c%.3f %c%.3f\n",xr[0],x[0],yr[0],y[0]);
printf(".\n");
}
/**************** END getcord *************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults(n)
int n;
{
char c;
	c = get_pwin();
	printf("-w %c\n",c);
	if (c == 'p') sprintf(str,"plot");
	else sprintf(str,"data");
	ask("Enter the pen number to use for box [1]: ",'f',&PEN);
	if (PEN == 0.0) PEN = 1.0;
	printf("-L %1.0f\n",PEN);
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
				fprintf(stderr,"Error: make.box [-m mapdef]\n");
				exit(ERR);
				break;
		
			}
		}
	}
printf("legend %s -o %s << EOF\n",mapdef,overlay);


setdefaults(0);
getcord();
printf("EOF\n");
}
