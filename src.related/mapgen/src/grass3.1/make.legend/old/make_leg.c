
/*  @(#)fill_leg.c     1.0  10/03/89   
 *  created by:         M.L.HOLKO, SCS
 *
 * Program will read standard input and create an ascii coordinate file
 * suitable for input to the mapgen programs proj and lines for
 * generating area fill paterns. This program was adapted from
 * snoop written by R. Glenn, SCS and plotmap written by P. Carlson, SCS.
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include "pattern.h"
#include "mdef.h"

#define LEGEND "legend/par/legend"
#define MAXPTS 50
#define QUOTE 34

/* GLOBALS VARIABLES */
FILE *fopen(), *fp;
int BOX=0,LINE=0,SYM=0;
struct strng {
	char name[15];
	};
struct loc {
	double start,offset,size;
	} LX,LY;
struct DBOX {
	float x, y;
	} box;
char xr,yr;
float PEN=0.0, SIZE=0.2, LEAD=0.3;
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
		case 'd':
			sscanf(line,"%lf",ptr);
			break;
		case 's':
			sscanf(line,"%s",ptr);
			break;
		}
	}
}


/***** get the points of the polygon ******************/
int getpts(x,y,n,sp)
double x[], y[];
int n;
double sp;
{
n--;
x[0]=x[3]=LX.start;
y[0]=y[1]=y[0] - ( box.y + LEAD + sp);
x[1]=x[0] + box.x;
x[2]=x[1];
y[2]=y[3]=y[0] + box.y;
return(4);
/*              x[3],y[3]	x[2],y[2]
		x[0],y[0]	x[1],y[1]
*/
}
/******** end getpts ***********************************/

/**** setdefaults sets some plotting pararmeters that seldom change *****/
static void setdefaults()
{
char line[128];

	ask("Enter the pen number to use for this text [1]: ",'f',&PEN);
        if (PEN == 0) PEN = 1.0;
                printf("-p %1.0f\n",PEN);

	ask("Enter the font to use [sr]: ",'s',FONT);
	printf("-f -%s\n",FONT);

	ask("Enter the size of the text [.2]: ",'f',&SIZE);
	if (SIZE == 0) SIZE = 0.2;
	printf("-s %.3f\n",SIZE);

	ask("Enter the leading for the text [.3]: ",'f',&LEAD);
	if (SIZE == 0) SIZE = 0.3;
	printf("-l %.3f\n",LEAD);

}
/***************** END setdefaults ******************************/

/******* get the starting cords and offsets *************/
getcord()
{
char line[100],c;
double x,y;

c = get_pwin();
printf("-w %c\n",c);
fprintf(fp,"win=%c\n",c);

if (c == 'p') sprintf(str,"plot");
else sprintf(str,"data");

ask("Enter the coordinates for the TOP LEFT corner of the legend\n",NULL,NULL);
while ( c != 'y' && c != 'n') {
        ask("Do you want to select the cordinates on the screen ([y]es or (n)o): ",'c',&c);
        if (c == NULL) c = 'y';
        if (isupper(c)) c = tolower(c);
        }
if (c == 'y') {
        if (str[0] == 'd'){
                get_margin(&x,&y);
                d_get_cord(x,y,&LX.start,&LY.start);
                }
        else
                d_get_cord(0.0,0.0,&LX.start,&LY.start);
        }
else {
	ask("\nEnter the x position: ",'d',&LX.start);
	ask("\n\nEnter the y position: ",'d',&LY.start);
	}


fprintf(fp,"x=%-.3lf\n",LX.start);
fprintf(fp,"y=%-.3lf\n",LY.start);
fprintf(fp,"y_offset=%-.3lf\n",LEAD);

sprintf(line,"Enter the distance between the pattern and the legend text [%.3lf]: ",SIZE);
ask(line,'d',&LX.offset);
if (LX.offset == 0.0) LX.offset = SIZE;
fprintf(fp,"x_offset=%-.3lf\n",LX.offset);


sprintf(line,"\n\nEnter the horizontal size of the pattern box [%.3lf]: ",SIZE);
ask(line,'f',&box.x);
if (box.x == 0.0) box.x = SIZE;
fprintf(fp,"h_size=%-.3lf\n",box.x);
box.x = box.x;

sprintf(line,"Enter the vertical size of the pattern box [%.3lf]: ",LEAD);
ask(line,'f',&box.y);
if (box.y == 0.0) box.y = LEAD;
fprintf(fp,"v_size=%-.3lf\n",box.y);
box.y = box.y;
}
/**************** END getcord *************************/

/***************** Set fill pattern *****************************/
int getfilpat(pfile,pat)
char pfile[];
struct pattern *pat;
{
char *c,line[20];
double F;
int n=19,p;
FILE *ffp;

while ((ffp = fopen(pfile,"r")) == NULL) {
	ask("ERROR: unable to open fill pattern file\n",NULL,NULL);
	line[0] = 0;
	return(ERR);
	}

while (fgets(line,n,ffp) != NULL) {
c = strchr(line,'=');

	switch (line[0]) {
/* --------------- pattern spacing --------------------*/
			case 's':
				sscanf(++c,"%lf",&F);
				pat->spacing = F;
				break;
/* --------------- pattern line --------------------*/
			case 'l':
				sscanf(++c,"%s",pat->line);
				break;
/* --------------- pattern angle --------------------*/
			case 'a':
				sscanf(++c,"%lf",&F);
				pat->angle = F;
				break;
			default:
				fprintf(stderr,"ERROR: unknown pattern %s",line);
				return(ERR);
			}
	}
fclose(ffp);
return(GOOD);
}
/******************* END getfilpat *********************************/
/****************** dosym *****************************/
static void dosym(pfp,x,y)
FILE *pfp;
double x, y;
{
char *p,line[20],font[15];
int n=19,i,pen;
char c;
double size;
while (fgets(line,n,pfp) != NULL) {
p = strchr(line,'=');

if (line[0] == 's')
	switch (line[1]) {
/* --------------- symbol size --------------------*/
		case 'i':
			sscanf(++p,"%lf",&size);
			break;
/* --------------- symbol number --------------------*/
		case 'y':
			sscanf(++p,"%d",&i);
			c = i;
fprintf(stderr,"%d %d %c\n",i,c,c);
			break;
		}
else
	switch (line[0]) {
/* --------------- symbol pen --------------------*/
		case 'p':
			sscanf(++p,"%d",&pen);
			break;
/* --------------- symbol font --------------------*/
		case 'f':
			sscanf(++p,"%s",font);
			break;
		}
	}
printf("-x %.3f\n-y %.3f\n",x + (box.x/2.0),y);
printf("-f -%s\n",font);
printf("-p %d\n",pen);
printf("-s %.3f\n",size);
printf("-t\n%c\n.\n",c);
}
/****************** END dosym *************************/

/************ start fill_leg *********/
main (argc,argv)
int argc;
char *argv[];

{
int cnt;
char c, *s, *p, *tp, line[120], pfile[50], tmp[15], outfil[50];
float x, y;
double  espace;
struct strng fills[50];

double X[MAXPTS], Y[MAXPTS];
int i, j, k, n_points=0, scale;
struct pattern pattern;
FILE *pfp;


/*-------------- INIT --------------------------------*/
sprintf(outfil,LEGEND);
pattern.spacing = 1.0;
pattern.color = 1;
pattern.angle = 0.0;
/*----------- END INIT -----------------------------*/


/*--------------- Get parameter output file ---------------*/
sscanf(argv[1],"%s",outfil);
if ((fp = fopen(outfil,"w")) == NULL) {
	fprintf(stderr,"make_leg: Unable to open output file %s\n",outfil);
	exit (ERR);
	}
/*----------------- Get pattern file directory -------------*/
sscanf(argv[2],"%s",pfile);
/*-------------------- END argument list ----------------*/




setdefaults();
getcord();
i = -1;
/* -------------- Get list of fills to include -------------------------*/
ask("Enter the fills or lines to include in this legend one per line, end with .\n",NULL,NULL);
while (fills[i++].name[0] != '.') {
	ask(": ",'s',fills[i].name);
	fprintf(fp,"fillname=%s\n",fills[i].name);
	sprintf(line,"fills/%s",fills[i].name);
	if ((pfp = fopen(line,"r")) == NULL) {
		sprintf(line,"lines/%s",fills[i].name);
		if ((pfp = fopen(line,"r")) == NULL) {
			sprintf(line,"symbols/%s",fills[i].name);
			if ((pfp = fopen(line,"r")) == NULL) {
				fprintf(stderr,"Error can not open fill, line or symbol file %s\n",fills[i].name);
				i--;
				}
			}
		else fclose(pfp);
		}
	else fclose(pfp);

	}
/*_______________________________________________________________________*/

/*------ procees each fill ------------------------------------------------*/

X[0] = LX.start;
Y[0] = LY.start;
espace = -LEAD;
for (cnt=0;fills[cnt].name[0] != '.';cnt++)
	{
	BOX=LINE=SYM=0;
	sprintf(line,"fills/%s",fills[cnt].name);
	if ((pfp = fopen(line,"r")) != NULL) 
		BOX = 1;	/************ using a fill pattern ********/
	else
		{ sprintf(line,"lines/%s",fills[cnt].name);
		if ((pfp = fopen(line,"r")) != NULL) 
		LINE = 1;      /*********** using a line pattern **********/
		else
			{ sprintf(line,"symbols/%s",fills[cnt].name);
			if ((pfp = fopen(line,"r")) != NULL) 
			SYM = 1;      /************* using symbol ****************/
			else {
				fprintf(stderr,"make_leg: Error can not open file for %s\n",line);
				exit(ERR);
				}
			}
		}

	while (fgets(line,119,pfp)) {
		if ((i = strspn(line,"pat")) == 3) s = strdup(line);
		if ((i = strspn(line,"pen")) == 3) p = strdup(line);
		}

	n_points = getpts(X,Y,cnt,espace);
	tp = strchr(p,'=');
	sscanf(++tp,"%d",&(pattern.color));

	if (BOX) {/*1*/
/*------------- Do each pattern for the fill ------------------------*/

	k = strlen(s);
	j=0;
	for (i=0;i<=k;i++)
		if (s[i] != '"') {
			s[j] = s[i]; j++;
			}
	while((s = strpbrk(s,"= \t")) != NULL) {/*2*/
	sscanf(++s,"%s",tmp);
	s += strlen(tmp);
	sprintf(outfil,"%s/%s",pfile,tmp);
	if (getfilpat(outfil,&pattern) != GOOD) {/*3*/
		fprintf(stderr,"make_leg: Error in reading fill pattern %s\n",outfil);
		exit(ERR);
		}/*3*/
	else {/*4*/
		if (pattern.spacing >= 1 || pattern.spacing == 0) scale=1;
		else if (pattern.spacing >= .1) scale = 100;
		else if (pattern.spacing >= .01) scale = 1000;
		else if (pattern.spacing >= .001) scale = 10000;
		else {/*5*/
			fprintf(stderr,"Error unable to work at fractional spacings less than .001\n");
			fprintf(stderr,"Using a spacing of 0\n");
			pattern.spacing = 0; scale=1;
			}/*5*/
		pattern.spacing = pattern.spacing * scale;
		printf("-p %d\n",pattern.color);
		for (i=0;i<n_points;i++) {
			X[i] = X[i] * scale;
			Y[i] = Y[i] * scale;
			}


		leg_fill(n_points,X,Y,&pattern,0,0,0,0,scale);
		for (i=0;i<n_points;i++) {/*6*/
			X[i] = X[i]/scale;
			Y[i] = Y[i]/scale;
			}/*6*/
		printf("-L %1.0f\n",PEN);
		printf("%.3lf %.3lf\n",X[0],Y[0]);
		printf("%.3lf %.3lf\n",X[1],Y[1]);
		printf("%.3lf %.3lf\n",X[2],Y[2]);
		printf("%.3lf %.3lf\n",X[3],Y[3]);
		printf("%.3lf %.3lf\n",X[0],Y[0]);
		printf(".\n");
		LINE = 0;
		}/*4*/

	}/*2*/

/*-------------- End of pattern processing ----------------------------*/
	}/*1*/
	else if (LINE){/*7*/
/*----------------- Do line work --------------------------------*/

		printf("-p %d\n",pattern.color);
		printf("-L %d\n",pattern.color);
		printf("%.3lf %.3lf\n",X[0],Y[2] );
		printf("%.3lf %.3lf\n",X[1],Y[2] );
		printf(".\n");
		LINE = 1;
/*---------------- End of line work ------------------------*/
		}
	else if (SYM) {
/*---------------- Start SYM work ------------------------*/
		fclose(pfp);
		sprintf(line,"symbols/%s",fills[cnt].name);
		pfp = fopen(line,"r");
		dosym(pfp,X[0],Y[2]);
/*---------------- End SYM work ------------------------*/
		}

	fclose(pfp);

	x = X[1] + LX.offset;
	if (!BOX) y = Y[2];
	else y = Y[2] - LEAD/2.0;
	printf("-x %.3f\n-y %.3f\n",x,y);
	printf("-f -%s\n",FONT);
	printf("-p %1.0f\n",PEN);
	sprintf(line,"legend/%s",fills[cnt].name);
	if ((pfp = fopen(line,"r")) == NULL) {
		fprintf(stderr,"make_leg: Error can not open legend text for %s\n",fills[cnt].name);
		fprintf(stderr,"Leaving the text blank. To correct create file %s and run redo.\n",line);
		}
	espace = 0.0;
	printf("-t\n");
	while (fgets(line,119,pfp)) {
		printf("%s",line);
		espace += LEAD;
		}
	printf(".\n",line);
	if (LINE) espace = espace - box.y;
	else {
		espace = espace - (box.y );
		if (espace < 0.0) espace = 0.0;
		}
	fclose(pfp);
	}
}

