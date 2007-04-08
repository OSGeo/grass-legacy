/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    screen_left < screen_right
 *    screen_top  < screen_bottom 
 */

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <grass/gis.h>
#include "psdriver.h"

FILE *outfp;
int true_color;
int width, height;

static void write_prolog(void)
{
	char prolog_file[GPATH_MAX];
	FILE *prolog_fp;

	sprintf(prolog_file, "%s/etc/psdriver.ps", G_gisbase());

	prolog_fp = fopen(prolog_file, "r");
	if (!prolog_fp)
		G_fatal_error("Unable to open prolog file");

	while (!feof(prolog_fp))
	{
		char buf[256];

		if (!fgets(buf, sizeof(buf), prolog_fp))
			break;

		fputs(buf, outfp);
	}

	fclose(prolog_fp);
}

int PS_Graph_set(int argc, char **argv)
{
	const char *file_name;
	char *p;

	G_gisinit("PS driver") ;

	p = getenv("GRASS_PSFILE");
	if (!p || strlen(p) == 0)
		p = FILE_NAME;

	file_name = p;

	p = getenv("GRASS_TRUECOLOR");
	true_color = p && strcmp(p, "TRUE") == 0;

	G_message("PS: GRASS_TRUECOLOR status: %s",
		true_color ? "TRUE" : "FALSE");

	width = screen_right - screen_left;
	height = screen_bottom - screen_top;

	init_color_table();

	outfp = fopen(file_name, "w");

	if (!outfp)
		G_fatal_error("Unable to open output file: %s", file_name);

	write_prolog();

	output("0 %d translate 1 -1 scale\n", height);

	PS_Erase();

	G_message("PS: collecting to file: %s,\n     GRASS_WIDTH=%d, GRASS_HEIGHT=%d",
		file_name, width, height);

	fflush(outfp);

	return 0;
}

void output(const char *fmt, ...)
{
	va_list va;

	va_start(va, fmt);
	vfprintf(outfp, fmt, va);
	va_end(va);
}

