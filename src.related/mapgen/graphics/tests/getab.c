/* procedure to list font characters 
**
**	example of character posting
*/
#include <stdio.h>
#include <graphics.h>

/* set up for tektronic 4014 */
#define XTITLE 4096/2
#define YTITLE 3085
#define XLEFT 440
#define YTOP 2850
#define XDEL 700
#define YDEL 140
#define XOFFB 140
# define MAXARGS 20

main(argc, argv)
int argc;
char *argv[];
{
	double atof();
	static char *usage = "ftab fontname 'plotter options'\n";
	char sb[60];
	int x, y, nc, i, j;
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

	plotopt(ERASE);  /* clear screen */

	plotopt(NEWPEN,"PenA");  /* set up 'pen' */
	plotopt(SFONT,"-");
	plotopt(SFONTS,argv[1]);
	plotopt(SIZE, 3);
	plotopt(CENTER);
	moveto(XTITLE, YTITLE);
	plotopt(LEAD, 14);
	sprintf(sb,"Listing of Font Symbols\nName: %s",argv[1]);
	plotopt(TEXT, sb);
	plotopt(JRIGHT);
	plotopt(SSIZE,4);
	plotopt(SIZE,2);
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
