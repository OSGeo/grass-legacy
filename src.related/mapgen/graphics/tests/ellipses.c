/* procedure to list font characters 
**
**	example of character posting
*/
#include <stdio.h>
#include <math.h>
#include <graphics.h>

/* set up for tektronic 4010 */
# define MAXARGS 20

main(argc, argv) int argc; char *argv[]; {
	double i, R, N, atof();
	long x, y;
	int nc, i, j;
	static char *parg[MAXARGS] ={0,0,"-i","."};

	i = atof(argv[1]);
	R = atof(argv[1]);
	N = atof(argv[1]);
	plotopen(parg);
	plotopt(ERASE);  /* clear screen */
	plotopt(NEWPEN,"PenA");  /* set up 'pen' */
	p = PENUP;
	for (phi = 0.; phi <= 360. ; phi += 6.) {
		pltopt(p, 512L + (long)((i * R / N) * cos(phi * )),
			350L + (long)((N - i) * R / N) * sin(phi * )) );
		p = 0;
	}
	plotend();
}
