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
	p = plotreq(ERROR);
	if (p->code != 0) {
		printf("error on setup:%d\n",p->cmd);
		exit(1);
	}

	do {
		p = plotreq(CURSOR);
		c = toascii(*p->str);
		if (isupper(c)) c = tolower(c);
		if (isdigit(c))
			plotopt(MPEN, (long)(c - '0'));
		else if (c >= 'a' && c <= 'f')
			plotopt(MPEN, (long)(c - 'a' + 10));
		if (c == 'n')
			moveto(p->x, p->y);
		lineto(p->x, p->y);
	} while (c != 'x');

	plotend();

	defclose();

	exit(0);
}
