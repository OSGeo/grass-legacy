/* test procedure for user graphics entries */
#include <stdio.h>
#include <ctype.h>
#include <graphics.h>

main(argc, argv)
int argc;
char *argv[];
{
	ANSWR *p;

	int c, cl;
	long sx, sy;
	static char *parg[] ={0,0,"-i",".","-s",".25",0};

	if (plotopen(parg) != 0) {
		printf("blew plot opening\n");
		exit(1);
	}

	plotopt(ERASE);
	plotopt(NEWPEN,"p");
	plotopt(DSIZE, 20L);
	plotopt(DMASK,0x3f32L);
	plotopt(SFONTS,"-");
	plotopt(SYM,3);
	plotopt(SSIZE,4L);
	plotopt(F_SIZE, 3L);
	plotopt(F_DIST, 100L);
	plotopt(FSYMS,"A\377");
	p = plotreq(ERROR);
	if (p->code != 0) {
		printf("error on setup:%d\n",p->cmd);
		exit(1);
	}

	do {
		p = plotreq(CURSOR);
		c = toascii(*p->str);
		if (isupper(c)) c = tolower(c);
		if (c == 'n' || c == 'c' || c == 'f' || c == 'g') {
			if (cl)
				lineto(sx, sy);
			if (c == 'c') {
				cl = 1;
				sx = p->x;
				sy = p->y;
				plotopt(DASH);
				plotopt(SYM,6);
			} else if (c == 'f') {
				plotopt(FPLOT);
				plotopt(SYM,8);
			} else if (c == 'g') {
				plotopt(FPLOTN);
				plotopt(SYM,9);
			} else {
				cl = 0;
				plotopt(SOLID);
				plotopt(SYM,3);
			}
			moveto(p->x, p->y);
		}
		else if (c == ' ')
			lineto(p->x, p->y);
	} while (c != 'd');

	if (cl)
		lineto(sx, sy);
	plotend();

	defclose();

	exit(0);
}
