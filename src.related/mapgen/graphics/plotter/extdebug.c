#ifndef lint
static char *SCCSID = "@(#)extdebug.c	USGS v.4.4";
#endif
# include <stdio.h>
#define EXTDEV
#define PLOTTER
# include "graphics.h"
/* debug routine which accepts output from plotter -d exdebug
** and displays operations and control in "English".
*/
# define MAX_CBUFF 60
#define IN(x) if ((x = getchar()) == EOF) goto bad_end
static char cbuff[MAX_CBUFF+1];

main(argc, argv) char **argv; {
	int cmd, i;
	long c, x, y;

	printf("ver. %s\n",SCCSID);
	for (i = 0; i < argc; ++i)
		printf("argv[%2d]:%s\n", i, argv[i]);
	if (getchar() != _BOP) {
		fprintf(stderr,"First char not Beginning of plot\n");
		exit(1);
	}
	printf("Beginning of plot stream\n");
	while ((cmd = getchar()) != EOF)
		switch (cmd) {
		case _EOP: /* clean ending */
			printf("End of plot stream\n");
			exit(0);
		case _PEN:
		case _PENL:
			IN(x);
			if (cmd == _PENL) {
				IN(c); x = (x << 8) + c;
				IN(c); x = (x << 8) + c;
			}
			printf("Pen %ld selected\n", x);
			break;
		case _DRAW:
		case _MOVE:
			printf("%s to:",cmd == _MOVE ? "Move" : "Draw");
			IN(c); x = c << 8; IN(c); x += c;
			IN(c); y = c << 8; IN(c); y += c;
			printf("%8ld,%8ld\n",x,y);
			break;
		case _SPCL:
			for (i = 0; i < MAX_CBUFF; ++i) {
				IN(c);
				cbuff[i] = c;
				if (!c) break;
			}
			printf("String:%s\n", cbuff);
			if (c) {
				printf("Possible String overflow\n");
				IN(c); while (c) IN(c); /* flush */
			}
			break;
		default:
			fprintf(stderr,"Invalid control code: %02x\n",c);
			exit(1);
		}
bad_end:
	fprintf(stderr,"EOF detected before End Of Plot\n");
	exit(1);
}
