#include <stdio.h>
#define ERR -1


double west, angle, lon;
int zone, deg;

main(argc,argv)
int argc;
char *argv[];

{

if (argc != 2)
	{
	fprintf(stderr,"Usage: get_angle degree \n");
	exit(ERR);
	}

sscanf(argv[1],"%lf",&lon);
lon += -.00011;
deg = (int)lon;
zone = (186.0 - lon)/6.0;
west = 186 - (zone * 6);

angle = (lon - west + 3.0) / 2.0;
printf("%3.3lf\n",angle);
}
