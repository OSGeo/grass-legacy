
/*  @(#)make_leg.c     1.0  10/03/89   
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
#define PENWIDTH .03	/*pen width .3 MM */

/* GLOBALS VARIABLES */
FILE *fopen(), *fp;
int BOX=0,LINE=0,SYM=0,BLANK=0;
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



spask(ptr)
char *ptr;
{
char line[128], cmd[50];

line[0] = NULL;
while (line[0] == NULL) {
	fprintf(stderr,": ");
	gets(line);
	if (strcmp(line,"list") == 0) line[0] = NULL;
	if (line[0] == NULL) {
		sprintf(cmd,"ls -C legend | sed -e \"s/cmd//;s/par//\" 1>&2");
		system(cmd);
		line[0] = NULL;
		}
	}
sscanf(line,"%s",ptr);
 }




/***** get the points of the polygon ******************/
/* box is	3	2
		0	1
*/
int getpts(x,y,sp)
double x[], y[];
double sp;
{
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
float lead;

	ask("Enter the pen number to use for this text [1]: ",'f',&PEN);
        if (PEN == 0) PEN = 1.0;
                printf("-p %1.0f\n",PEN);
                fprintf(fp,"pen=%1.0f\n",PEN);

	ask("Enter the font to use [sr]: ",'s',FONT);
	if (FONT[0] == NULL) sprintf(FONT,"sr");
	printf("-f -%s\n",FONT);
	fprintf(fp,"font=%s\n",FONT);

	ask("Enter the size of the text [.2]: ",'f',&SIZE);
	if (SIZE == 0) SIZE = 0.2;
	printf("-s %.3f\n",SIZE);
	fprintf(fp,"size=%.3f\n",SIZE);
	if (SIZE < .5) LEAD = SIZE + 0.1;
	else LEAD = SIZE * 1.2;

	sprintf(line,"Enter the leading for the text [%.2f]: ",LEAD);
	ask(line,'f',&lead);
	if (lead != 0) LEAD = lead;
	printf("-l %.3f\n",LEAD);
	fprintf(fp,"lead=%.3f\n",LEAD);

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
fprintf(fp,"screen=n\n");
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
                get_margin(&x,&y,NULL);
                d_get_cord(x,y,&LX.start,&LY.start,1);
                }
        else
                d_get_cord(0.0,0.0,&LX.start,&LY.start,1);
        }
else {
	ask("Enter the x position: ",'d',&LX.start);
	ask("Enter the y position: ",'d',&LY.start);
	}


fprintf(fp,"x=%-.3lf\n",LX.start);
fprintf(fp,"y=%-.3lf\n",LY.start);

sprintf(line,"Enter the distance between the pattern and the legend text [%.2lf]: ",SIZE);
ask(line,'d',&LX.offset);
if (LX.offset == 0.0) LX.offset = SIZE;
fprintf(fp,"x_offset=%-.3lf\n",LX.offset);


sprintf(line,"Enter the horizontal size of the pattern box [%.2lf]: ",SIZE);
ask(line,'f',&box.x);
if (box.x == 0.0) box.x = SIZE;
fprintf(fp,"h_size=%-.3lf\n",box.x);
if (SIZE < .5) LEAD = SIZE + 0.1;
else LEAD = SIZE * 1.2;

sprintf(line,"Enter the vertical size of the pattern box [%.2lf]: ",LEAD);
ask(line,'f',&box.y);
if (box.y == 0.0) box.y = LEAD;
fprintf(fp,"v_size=%-.3lf\n",box.y);
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
printf("-j c\n");
printf("-s %.3f\n",size);
printf("-t\n%c\n.\n",c);
printf("-j l\n");
}
/****************** END dosym *************************/
/****************** doline *****************************/
static void doline(pfp,x,y,x1,y1)
FILE *pfp;
double x, y,x1,y1;
{
char *p,line[20],pattern[10];
int n=19,i,pen;
char c;
double size;
while (fgets(line,n,pfp) != NULL) {
p = strchr(line,'=');

switch (line[0]) {
/* --------------- pen number for line --------------------*/
	case 'p':
		sscanf(++p,"%d",&pen);
		break;
/* --------------- dash pattern size --------------------*/
	case 'd':
		sscanf(++p,"%lf",&size);
		break;
/* ---------------  line type --------------------*/
	case 'l':
		sscanf(++p,"%c",&c);
		break;
	}
}


		if (c == 's') printf("-L %d\n",pen);
		else {
		switch (c) {
			case 'a': sprintf(pattern,"0x5555");break;
			case 'b': sprintf(pattern,"0x7777");break;
			case 'c': sprintf(pattern,"0xFCFC");break;
			case 'd': sprintf(pattern,"0xFEFE");break;
			case 'e': sprintf(pattern,"0xFFFC");break;
			case 'f': sprintf(pattern,"0xFFFE");break;
			case 'g': sprintf(pattern,"0xFAFA");break;
			case 'h': sprintf(pattern,"0xFFFA");break;
			case 'i': sprintf(pattern,"0xFFF6");break;
			case 'j': sprintf(pattern,"0xFFEA");break;
			case 'k': sprintf(pattern,"0xFFB6");break;
			}
			printf("-L %d,%s,%.3lf\n",pen,pattern,size);
		}

		printf("%.3lf %.3lf\n",x,y );
		printf("%.3lf %.3lf\n",x1,y1 );
		printf(".\n");


}
/****************** END doline *************************/


/************ start make_leg *********/
main (argc,argv)
int argc;
char *argv[];

{
int cnt;
char c, *s, *p, *tp, line[120], pfile[50], tmp[15], outfil[50];
float x, y;
double  espace;
struct strng fills[50];

double X[MAXPTS], Y[MAXPTS], fX[MAXPTS], fY[MAXPTS];
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
ask("Enter the fills symbols or lines to include in this legend",NULL,NULL);
ask("one per line, end with .  [list]\n",NULL,NULL);
while (fills[i++].name[0] != '.') {
	spask(fills[i].name);
	if (strcmp(fills[i].name,"blank") != 0)
	{
	sprintf(line,"fills/%s",fills[i].name);
	if ((pfp = fopen(line,"r")) == NULL) {
		sprintf(line,"lines/%s",fills[i].name);
		if ((pfp = fopen(line,"r")) == NULL) {
			sprintf(line,"symbols/%s",fills[i].name);
			if ((pfp = fopen(line,"r")) == NULL) {
				fprintf(stderr,"Error can not open fill, line or symbol file %s\n",fills[i].name);
				fills[i].name[0] = NULL;
				}
			}
		else fclose(pfp);
		}
	else fclose(pfp);
	}
	if (fills[i].name[0] == NULL) i--;
	else 
		fprintf(fp,"fillname=%s\n",fills[i].name);

	}
/*_______________________________________________________________________*/

/*------ process each fill ------------------------------------------------*/

X[0] = LX.start;
Y[0] = LY.start;
espace = -LEAD;
for (cnt=0;fills[cnt].name[0] != '.';cnt++)
	{
	BOX=LINE=SYM=BLANK=0;
	if (strcmp(fills[cnt].name,"blank") == 0) BLANK = 1;
	else {
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

	tp = strchr(p,'=');
	sscanf(++tp,"%d",&(pattern.color));
	}
	n_points = getpts(X,Y,espace);

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
			if (i == 0 || i == 3) fX[i] = (X[i] + PENWIDTH) * scale;
			else  fX[i] = (X[i] - PENWIDTH) * scale;
			if (i == 2 || i == 3) fY[i] = (Y[i] - PENWIDTH) * scale;
			else  fY[i] = (Y[i] + PENWIDTH) * scale;
			}


		leg_fill(n_points,fX,fY,&pattern,0,0,0,0,scale);
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
		fclose(pfp);
		sprintf(line,"lines/%s",fills[cnt].name);
		pfp = fopen(line,"r");
		doline(pfp,X[0],Y[2],X[1],Y[2]);
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
/*-------------- Do BLANK -------------------------------*/
	else if (BLANK) {
		printf("-L %1.0f\n",PEN);
		printf("%.3lf %.3lf\n",X[0],Y[0]);
		printf("%.3lf %.3lf\n",X[1],Y[1]);
		printf("%.3lf %.3lf\n",X[2],Y[2]);
		printf("%.3lf %.3lf\n",X[3],Y[3]);
		printf("%.3lf %.3lf\n",X[0],Y[0]);
		printf(".\n");
/*-------------- END BLANK -------------------------------*/
		}

	if (! BLANK) fclose(pfp);

	x = X[1] + LX.offset;
	if (!BOX && !BLANK) y = Y[2];
	else y = Y[2] - LEAD/2.0;
	printf("-x %.3f\n-y %.3f\n",x,y);
	printf("-f -%s\n",FONT);
	printf("-p %1.0f\n",PEN);
	printf("-s %.3f\n",SIZE);
	printf("-l %.3f\n",LEAD);
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
