/* lookup.c	*/ 
/* written by James Ganong, REGIS, UC BERKELEY 1996 */
/* copyright UC Regents, 1996-1998 */
/* use at your own risk */

#include "gis.h"

struct Colors colors;
char *name;
char *mapset;
int cat,r,g,b;


main (int argc, char ** argv) {
	if (argc != 4 ) {
		fprintf(stderr, "usage: %s raster mapset category\n", argv[0]);
		exit(-2);
	}
	G_gisinit(argv[0]);
	name=argv[1];
	mapset=argv[2];
	cat=atoi(argv[3]);
	if (G_read_colors(name, mapset, &colors) != 1 ) {
		fprintf(stderr, "could not read color table\n") ;
		exit(-3);
	}
		
	G_get_color(cat, &r, &g, &b, &colors);
	printf("%d %d %d\n", r, g, b);
}



