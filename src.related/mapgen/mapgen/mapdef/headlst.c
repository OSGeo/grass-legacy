#ifndef lint
static char *SCCSID = "@(#)headlst.c	AMG v.3.1";
#endif
# include <stdio.h>
# include "mapgen.h"
# include "mapdef.h"

char *rtodms();

headlst() {
	double adjlon();

	printf("Mapgen - mapdef file initialization (Ver: %s\n",VERSION);
	fputs("   file name: ",stdout);
	puts(def_name);
	if (def.magic != MAGIC) {
		puts("not yet initialized");
		return;
	}
	fputs("\nprojection: ",stdout);
	fputs(def.proj,stdout);
	fputs("\nlongitude range: ",stdout);
	fputs(rtodms(line,adjlon(def.l_lon),'e','w'),stdout);
	putchar(' ');
	fputs(rtodms(line,adjlon(def.r_lon),'e','w'),stdout);
	fputs("\n latitude range: ",stdout);
	fputs(rtodms(line,def.b_lat,'n','s'),stdout);
	putchar(' ');
	fputs(rtodms(line,def.t_lat,'n','s'),stdout);
	if (def.scale == 0.) {
		puts("\nnot yet scaled\n");
		return;
	}
	printf("\nscale 1 : %.1f",def.scale);
	printf("\nmap size x: %.2f, y: %.2f\n",
		def.B.x_max / def.cts_cm,
		def.B.y_max / def.cts_cm);
	printf("data window x from %.2f to %.2f, y from %.2f to %.2f\n",
		def.D.x_min / def.cts_cm,
		def.D.x_max / def.cts_cm,
		def.D.y_min / def.cts_cm,
		def.D.y_max / def.cts_cm);
	if (def.S1.x_max)
		printf("Box A> x from %.2f to %.2f, y from %.2f to %.2f\n",
			def.S1.x_min / def.cts_cm,
			def.S1.x_max / def.cts_cm,
			def.S1.y_min / def.cts_cm,
			def.S1.y_max / def.cts_cm);
	if (def.S2.x_max)
		printf("Box B> x from %.2f to %.2f, y from %.2f to %.2f\n",
			def.S2.x_min / def.cts_cm,
			def.S2.x_max / def.cts_cm,
			def.S2.y_min / def.cts_cm,
			def.S2.y_max / def.cts_cm);
	printf("plotter counts/cm: %.2f\n",def.cts_cm);
}
