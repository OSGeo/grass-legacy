#include <stdio.h>

#define NPLT 10
#define BAD -1
#define GOOD 0

struct PLOTTER {
	char name[10];
	int x;
	int y;
	};

main(argc,argv)
int argc;
char *argv[];
{
int t,i=0,x=0,y=0,px=0,py=0,type,nx, ny;
double by, tx, ty, scale, s[5];
char code[10],side[10];
struct PLOTTER plt[NPLT];

strcpy(plt[0].name,"7475a");plt[0].x=27;plt[0].y=20;
strcpy(plt[1].name,"orchid");plt[1].x=24;plt[1].y=19;
strcpy(plt[2].name,"HPGLd");plt[2].x=86;plt[2].y=55;
if (argc != 8) {
	fprintf(stderr,"fitscale: syntax error\nfitscale plotter scale x y bottom side_width side_length\n");
	exit(BAD);
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
		exit(BAD);
		}
	}
px = plt[i].x;
py = plt[i].y;
if (ty > py) {printf("0"); exit(BAD);}
/*DEBUG fprintf(stderr,"%lf %d %d %lf %lf %lf %d %d %d %d\n",scale,x,y,by,tx,ty,px,py,nx,ny);*/

s[0]=1.0;

if ((px-nx < 0) || (py-ny < 0)) {
	if ((px-ny < 0) || (py-nx < 0)) {
		s[0]=(double) px/x; s[1]=(double) px/x; s[2]=(double) px/y;
		s[3]=(double) py/y; s[4]=(double) py/x; type=1;
		for (i=2;i<5;i++) if (s[i] < s[i-1]) {s[0]=s[i]; type=i;}
	}
}
nx=s[0] * x;
ny=s[0] * y;
if ((px-nx < 0) || (py-ny < 0)) {
	if ((px-ny < 0) || (py-nx < 0)) exit(BAD);
	else { i=px; px=py; py=i;}
	}

px=px-tx-1;
py=py-by-1;
/*DEBUG fprintf(stderr,"%lf %d %d %lf %lf %lf %d %d %d %d\n",scale,x,y,by,tx,ty,px,py,nx,ny);*/

s[0]=(double) px/x; s[1]=(double) px/x; s[2]=(double) px/y;
s[3]=(double) py/y; s[4]=(double) py/x; type=1;
for (i=2;i<5;i++) if (s[i] < s[i-1]) {s[0]=s[i]; type=i;}
scale=scale/s[0];
printf("%.0lf\n",scale);
exit(GOOD);
}
