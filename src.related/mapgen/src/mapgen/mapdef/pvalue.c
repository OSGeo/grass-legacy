#ifndef lint
static char *SCCSID = "@(#)pvalue.c	AMG v.3.1";
#endif
/* compute and list coordinates for a map master file */
# include <stdio.h>
# include <string.h>
# include "mapgen.h"
# include "mapdef.h"

pvalue() {
	int i;
	char *s, *sv;
	double v[2], x, y, dmstor(), adjlon();
	char *getline();

	for (;;) {
		if (verbose)
			puts("enter longitude latitude");
		if (s = getline(line, MAXLINE, stdin)) {
			for (i = 0; i < 2 && (sv = strtok(s, " \t")); ++i) {
				if (strcmp(sv, "-"))
					v[i] = dmstor(sv, 0);
				s = NULL;
			}
			v[0] = adjlon(v[0] - def.cm) + def.cm;
			if (mproj(v[0], v[1], &x, &y))
				puts(verbose ? "value out of range" :
						"*\t*");
			else {
				if (verbose)
				printf("x, y cts: %d %d, x, y cm: %.2f %.2f\n",
					(long)x, (long)y,
					x / def.cts_cm, y / def.cts_cm);
				else
				printf("%.3f\t%.3f\n",
					x / def.cts_cm, y / def.cts_cm);
			}
		} else {
			if (verbose)
				puts("input done");
			return;
		}
	}
}
