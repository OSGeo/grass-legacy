#ifndef lint
static char *SCCSID = "@(#)extdebug.c	OEMG v.3.2";
#endif
# include <stdio.h>
#define EXTDEV
#define PLOTTER
# include <graphics.h>
/* debug routine which accepts output from plotter -d debug
** and displays operations and control in "English".
*/

#define IN(x) if ((x = getchar()) == EOF) goto bad_end

main() {
	int c;
	long x, y;

	while ((c = getchar()) != EOF)
		switch (c) {
		case _BOP:
			printf("Beginning of plot stream\n");
			break;
		case _EOP:
			printf("End of plot stream\n");
			exit(0);
		case _PEN:
			IN(c);
			printf("Pen %d selected\n",c);
			break;
		case _DRAW:
		case _MOVE:
			printf("%s to x,y:",c == _MOVE ? "Move" : "Draw");
			IN(c); x = c << 8;
			IN(c); x += c & 0xff;
			IN(c); y = c << 8;
			IN(c); y += c & 0xff;
			printf("%8ld,%8ld\n",x,y);
			break;
		default:
			fprintf(stderr,"Invalid control code: %02x\n",c);
			exit(1);
		}
bad_end:
	fprintf(stderr,"EOF detected before End Of Plot\n");
}
