/* test procedure for user graphics entries */
#include <stdio.h>
#include <ctype.h>
#include <graphics.h>

# define MAXARGS 40

main(argc, argv) int argc; char *argv[]; {
	ANSWR *p;
	int i, c, cl;
	long sx, sy;
	static char *parg[MAXARGS] ={0,0,"-i",".",0};

	/* take remaining arguements from run line and
		transfer to plotter */
	for (i = 1; i < argc && i < MAXARGS-4; i++)
		parg[i+3] = argv[i];

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

	exit(0);
}
