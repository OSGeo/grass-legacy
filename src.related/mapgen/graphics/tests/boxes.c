/* procedure to make calibrated concentric boxes
**
** execute as: boxes plotter cm number_of plotter_name
*/
#include <stdio.h>
#include <graphics.h>

# define MAX_ARGS 10

static char *parg[5+MAX_ARGS] = {0, 0, "-i","."};

main(argc, argv) int argc; char **argv; {
	double atof();
	static char *usage = "boxes cnts number_of rept [plotter opts]\n";
	int size, number, repeat;
	int n, p;
	static long x, y;

	if (argc < 4) {
		puts(usage, stderr);
		exit(0);
	}

	size = atoi(argv[1]);
	number = atoi(argv[2]);
	repeat = atoi(argv[3]);

	for (n = 4; n < argc && n < MAX_ARGS; ++n)
		parg[n] = argv[n];

	plotopen(parg);

	plotopt(ERASE);  /* clear screen */

	plotopt(NEWPEN,"PenA");  /* set up 'pen' */
	plotopt(SPECIAL, "Boxes special test");
	do {
		x = y = 0;
		for (n = 0; n < number; n++) {
			p = (2*(number-n)-1)*size;
			plotopt(MPEN, n);
			moveto(x, y);
			lineto(x+p, y);
			lineto(x+p, y+p);
			lineto(x, y+p);
			lineto(x, y);
			x += size;
			y += size;
		}
	} while (--repeat > 0);

	plotend();
}
