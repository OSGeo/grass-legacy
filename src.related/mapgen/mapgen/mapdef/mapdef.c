#ifndef lint
static char *SCCSID = "@(#)mapdef.c	AMG v.3.2";
#endif
# define MAIN

# include <stdio.h>
/*
# include <ctype.h>
*/
# include <string.h>
# include <fcntl.h>
# include <math.h>
# include "mapgen.h"
# include "mapdef.h"
# include <signal.h>
	static
onintr() { /* interrupt trap */
	quit("interupted mapgen",1);
}
main(argc, argv) char **argv; {
	int C = 0, S = 0, P = 0;
	char *arg;

		/* set interupt trap */
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, onintr);

		/* run line process arguments */
	f_tempa = f_tempb = 0;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch((*++arg)) {
			case '\0': break;
			/* map definition file */
			case 'm':
				if (--argc > 0) {
					if (def_name)
						goto multi_name;
					def_name = *++argv;
				}
				continue;
			/* left-hand longitude */
			case 'v': verbose = 1; continue;
			/* coefficients only tag */
			case 'c': C = 1; continue;
			/* scale only tag */
			case 's': S = 1; continue;
			/* project only tag */
			case 'p': P = 1; continue;
			/* set resolution */
			case 'k':
				if (--argc > 0)
					cpc = atof(*++argv);
				continue;
			default:
				fprintf(stderr,"Unknown option -%c\n", *arg);
				exit(1);
				break;
			}
			break;
		} else if (! def_name)
			def_name = *argv;
		else {
multi_name:
			fprintf(stderr,"multiply defined master name\n");
			exit (1);
		}
	}
	if (! def_name) {
		fprintf(stderr,"No master file given\n");
		exit(1);
	}
	if ( C )
		if ((mapdef = creat(def_name, 0644)) < 0) {
			perror("master open");
			exit (1);
		} else
			close(mapdef);
	else if (loaddef(def_name))
		exit(1);
	prompt = isatty(fileno(stdin));
	if (verbose)
		headlst();
		/* set up approximating polynomials */
	if ( C ) {
		genrange();
		def.magic = MAGIC;
		Save();
	}
		/* setup scaling */
	if ( S ) {
		genscale();
		Save();
	}
		/* other functions */
	if ( P )
		pvalue();
	exit(0);
}
quit(s, force) char *s; {
	if (f_tempa) unlink(f_tempa);
	if (f_tempb) unlink(f_tempb);
	fprintf(stderr,"%s\n",s);
	if (!force && prompt)
		return;
	else
		exit(1);
}
Save() {
	mapdef = open(def_name, O_RDWR);
	write(mapdef, &def.magic, sizeof(def));
	close(mapdef);
}
