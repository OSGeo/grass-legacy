#include <stdio.h>
#include "mdef.h"


get_margin(x,y,m)
double *x,*y;
char *m;
{
char mapdef[130],mapdefp[24],c,sx[10],sy[10];
int i=0;
FILE *mapfp;

if (m == NULL) {
fprintf(stderr,"Enter the name of the map definition file [map.def]: ");
while ((c = getchar()) != '\n') mapdef[i++] = c;
mapdef[i] = NULL;
}
else sprintf(mapdef,"%s",m);

if (mapdef[0] == NULL) sprintf(mapdef,"map.def");
sprintf(mapdefp,"%s.par",mapdef);


if ((mapfp = fopen(mapdefp,"r")) == NULL){
	fprintf(stderr,"ERROR can not open %s\n",mapdefp);
	return(0);
	}

for (i=0;i<4;i++) {
	while ((c = getc(mapfp)) != '\n') ;
	}


fscanf(mapfp,"%s %s",sx,sy);
sscanf(sx,"%lf",x);
sscanf(sy,"%lf",y);
if (sx[0] == '-') *x = 3.0;
if (sy[0] == '-') *y = 3.0;
}
