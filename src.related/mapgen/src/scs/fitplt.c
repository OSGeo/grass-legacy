#include <stdio.h>

#define NPLT 10
#define BAD -1
#define GOOD 0

struct PLOTTER {
	char name[10];
	int x;
	int y;
	char rev;	/* PLOTTER CAPABLE of rotating axis y else n */
	};

struct PLOTTER plt[NPLT];
int t,i=0,x=0,y=0,px=0,py=0,type,nx, ny;
double by, tx, ty, scale, s[4];
char r,code[10],side[10];

int fitplt(argc,argv)
int argc;
char *argv[];
{
if (argc != 4) {
	fprintf(stderr,"fitplt: syntax error\nfitplt plotter x y\n");
	return(BAD);
	}

x = atoi(argv[2]);
y = atoi(argv[3]);

while ((t = strspn(plt[i].name,argv[1])) != strlen(argv[1])) {
	if (++i == NPLT) {
		fprintf(stderr,"ERROR: no such plotter as %s\n",argv[1]);
		return(BAD);
		}
	}
px = plt[i].x;
py = plt[i].y;
r = plt[i].rev;

sprintf(code,"NORM");
if ((px-x < 0) || (py-y < 0)) {
	if ((px-y < 0) || (py-x < 0)) return(BAD);
	else  if (r == 'y') {sprintf(code,"REVERSE"); i=px; px=py; py=i;}
	}

if (px-x < py-y) sprintf(side,"BOTTOM");
else sprintf(side,"RIGHT");

printf("%s %s %d %d %d %d\n",code,side,x,y,px,py);
return(GOOD);
}
/*######################  FITSCALE VERSION ######################*/
int fitscale(argc,argv)
int argc;
char *argv[];
{

if (argc != 8) {
	fprintf(stderr,"fitscale: syntax error\nfitscale plotter scale x y bottom side_width side_length\n");
	return(BAD);
	}
scale = (double) atol(argv[2]);
x = atoi(argv[3]);
y = atoi(argv[4]);
by = (double) atol(argv[5])/100;
tx = (double) atol(argv[6])/100;
ty = (double) atol(argv[7])/100;

while ((t = strspn(plt[i].name,argv[1])) != strlen(argv[1])) {
	if (++i == NPLT) {
		fprintf(stderr,"ERROR: no such plotter as %s\n",argv[1]);
		return(BAD);
		}
	}
px = plt[i].x;
py = plt[i].y;
r = plt[i].rev;

if (ty > py) {printf("0"); return(BAD);}
/*DEBUG fprintf(stderr,"%lf %d %d %lf %lf %lf %d %d %d %d\n",scale,x,y,by,tx,ty,px,py,nx,ny);*/

tx=x+tx+.5; ty=y+ty+.5;

s[0]=(double) px/tx;
s[1]=(double) py/ty;
if (s[0] > s[1]) type =1;
else type=0;
if (r == 'y'){
	s[2]=(double) px/ty;
	s[3]=(double) py/tx;
	if (s[2] > s[3]) s[2]=s[3];
	if (s[type] < s[2]) type=2;
	}

scale=scale/s[type];
/* DEBUG  fprintf(stderr,"DEBUG %d %lf\n",type,s[type]);*/
printf("%.0lf\n",scale);
return(GOOD);
}


/* Start Main Program */

main(argc,argv)
int argc;
char *argv[];
{
strcpy(plt[0].name,"7475a");plt[0].x=27;plt[0].y=20;plt[0].rev='y';
strcpy(plt[1].name,"orchid");plt[1].x=40;plt[1].y=30;plt[1].rev='n';;
strcpy(plt[2].name,"HPGLd");plt[2].x=86;plt[2].y=55;plt[2].rev='y';

if (strcmp(argv[0],"fitplt") == 0)
	return(fitplt(argc,argv));
else return(fitscale(argc,argv));
}
