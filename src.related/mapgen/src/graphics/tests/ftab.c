/* procedure to list font characters 
**
**	example of character posting
*/
#include <stdio.h>
#include <graphics.h>

/* set up for tektronic 4014 */
#define XTITLE 4096L/2
#define YTITLE 3080L
#define XLEFT 440L
#define YTOP 2850L
#define XDEL 680L
#define YDEL 140L
#define XOFFB 140L
# define MAXARGS 20

main(argc, argv) int argc; char *argv[]; {
	double atof();
	static char *usage = "ftab fontname 'plotter options'\n";
	char sb[60];
	ANSWR *p;
	long x, y;
	int nc, i, j;
	static char *parg[MAXARGS] ={0,0,"-i","."};

	if (argc < 2) {
		puts(usage, stdout);
		exit(0);
	}

	/* take remaining arguements from run line and
		transfer to plotter */
	for (i = 2; i < argc && i < MAXARGS-4; i++)
		parg[i+2] = argv[i];

	plotopen(parg);
	p = plotreq(P_SIZE);
	if (p->x < 4096 || p->y < 3200) {
		char s[20];
		float fx, fy;

		fx = p->x / 4096.;
		fy = p->y / 3200.;
		sprintf(s, "%g", fy < fx ? fy : fx);
		plotopt(RESCALE, s);
	}
	plotopt(ERASE);  /* clear screen */

	plotopt(NEWPEN,"PenA");  /* set up 'pen' */
	plotopt(SFONT,"-");
	plotopt(SFONTS,argv[1]);
	plotopt(SIZE, 3L);
	plotopt(CENTER);
	moveto(XTITLE, YTITLE);
	plotopt(LEAD, 12L);
	sprintf(sb,"Digraph Listing of Font\n%s",argv[1]);
	plotopt(TEXT, sb);
	plotopt(JRIGHT);
	plotopt(SSIZE,4L);
	plotopt(SIZE,2L);
	plotopt(XOFF,-XOFFB); /* descriptive factors */

	nc = 0;
	x = XLEFT;
	for (i = 0; i < 6 ; i++) { /* column loop */
		y = YTOP;
		for (j = 0; j < 21 && nc < 126; j++) { /* row loop */

			plotopt(SYM, ++nc); /* plot symbol */
			moveto(x,y);
			if (nc > 31)
				sprintf(sb,"%c/%d/%03o",nc,nc,nc);
			else 
				sprintf(sb,"%d/%03o",nc,nc);
			plotopt(TEXT, sb);
			y -= YDEL;
		}
		x += XDEL;
	}
	plotend();
}
