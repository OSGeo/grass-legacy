/* PROGRAM:	gshhs.c
 * AUTHOR:	Paul Wessel (wessel@soest.hawaii.edu)
 * DATE:	JAN. 28, 1996
 * PURPOSE:	To extract ASCII data from binary shoreline data
 *		as described in the 1996 Wessel & Smith JGR Data Analysis Note.
 */

#include <stdio.h>
#include <math.h>
#include "gshhs.h"

/* For byte swapping if needed */

#define swabi2(i2) (((i2) >> 8) + (((i2) & 255) << 8))
#define swabi4(i4) (((i4) >> 24) + (((i4) >> 8) & 65280) + (((i4) & 65280) << 8) + (((i4) & 255) << 24))

main (argc, argv)
int	argc;
char **argv;
{
	double w, e, s, n, area, lon, lat;
	char source;
	FILE	*fp;
	int	k, max = 270000000;
	struct	POINT p;
	struct GSHHS h;
        
	if (argc != 2) {
		fprintf (stderr, "usage:  gshhs gshhs_[f|h|i|l|c].b > ascii.dat\n");
		exit(-1);
	}
	
	if ((fp = fopen (argv[1], "r")) == NULL ) {
		fprintf (stderr, "gshhs:  Could not find file %s.\n", argv[1]);
		exit(-1);
	}
		
	while (fread((void *)&h, (size_t)sizeof (struct GSHHS), (size_t)1, fp) == 1) {

#ifdef FLIP
		h.id = swabi4 ((unsigned int)h.id);
		h.n = swabi4 ((unsigned int)h.n);
		h.level = swabi4 ((unsigned int)h.level);
		h.west = swabi4 ((unsigned int)h.west);
		h.east = swabi4 ((unsigned int)h.east);
		h.south = swabi4 ((unsigned int)h.south);
		h.north = swabi4 ((unsigned int)h.north);
		h.area = swabi4 ((unsigned int)h.area);
		h.greenwich = swabi2 ((unsigned int)h.greenwich);
		h.source = swabi2 ((unsigned int)h.source);
#endif
		w = h.west  * 1.0e-6;
		e = h.east  * 1.0e-6;
		s = h.south * 1.0e-6;
		n = h.north * 1.0e-6;
		source = (h.source == 1) ? 'W' : 'C';
		area = 0.1 * h.area;

		printf ("P %6d%8d%2d%2c%13.3lf%10.5lf%10.5lf%10.5lf%10.5lf\n", h.id, h.n, h.level, source, area, w, e, s, n);

		for (k = 0; k < h.n; k++) {

			if (fread ((void *)&p, (size_t)sizeof(struct POINT), (size_t)1, fp) != 1) {
				fprintf (stderr, "gshhs:  Error reading file.\n");
				exit(-1);
			}
#ifdef FLIP
			p.x = swabi4 ((unsigned int)p.x);
			p.y = swabi4 ((unsigned int)p.y);
#endif
			lon = (h.greenwich && p.x > max) ? p.x * 1.0e-6 - 360.0 : p.x * 1.0e-6;
			lat = p.y * 1.0e-6;
			printf ("%10.5lf%10.5lf\n", lon, lat);
		}
		max = 180000000;	/* Only Eurasiafrica needs 270 */
	}
		
	fclose(fp);

	exit (0);
}
