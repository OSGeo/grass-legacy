#include "cairodriver.h"
#include <cairo-ps.h>
#include <cairo-pdf.h>
#include <cairo-svg.h>

/* globals */
char *file_name;
int file_type;
int width, height;
int modified;
int auto_write;

/* background color */
double bgcolor_r, bgcolor_g, bgcolor_b, bgcolor_a;

/* cairo objects */
cairo_surface_t *surface;
cairo_t *cairo;

int Cairo_Graph_set(int argc, char **argv)
{
	char *c;

	G_gisinit("Cairo driver");
	G_debug(1, "Cairo_Graph_set");

	/* set image properties */
	width = screen_right - screen_left;
	height = screen_bottom - screen_top;

	/* TODO: find out why G_getenv doesn't work */

	/* get file name */
	c = getenv("GRASS_CAIROFILE");
	if (!c || strlen(c) == 0)
		c = DEFAULT_FILE_NAME;

	file_name = c;
	G_message("CAIRO: collecting to file: %s\n GRASS_WIDTH=%d, GRASS_HEIGHT=%d", file_name, width, height);

	/* get file type (from extension) */
	if (ends_with(c, ".png"))
		file_type = FTYPE_PNG;
#if CAIRO_HAS_PDF_SURFACE
	else if (ends_with(c, ".pdf"))
		file_type = FTYPE_PDF;
#endif
#if CAIRO_HAS_PS_SURFACE
	else if (ends_with(c, ".ps"))
		file_type = FTYPE_PS;
#endif
#if CAIRO_HAS_SVG_SURFACE
	else if (ends_with(c, ".svg"))
		file_type = FTYPE_SVG;
#endif
	else
		G_fatal_error("Unknown file extension: %s", c);
	G_debug(1, "File type: %s (%d)", c, file_type);

	c = getenv("GRASS_AUTO_WRITE");
	auto_write = c && strcmp(c, "TRUE") == 0;

	/* get background color */
	c = getenv("GRASS_BACKGROUNDCOLOR");
	if (c && *c)
	{
		unsigned int red, green, blue;

		if (sscanf(c, "%02x%02x%02x", &red, &green, &blue) == 3)
		{
			bgcolor_r = CAIROCOLOR(red);
			bgcolor_g = CAIROCOLOR(green);
			bgcolor_b = CAIROCOLOR(blue);
		}
		else
			G_fatal_error("Unknown background color: %s", c);
	}
	else
		bgcolor_r = bgcolor_g = bgcolor_b = 1.0;

	/* get background transparency setting */
	c = getenv("GRASS_TRANSPARENT");
	if (c && strcmp(c, "TRUE") == 0)
		bgcolor_a = 0.0;
	else
		bgcolor_a = 1.0;

	init_cairo();
	return 0;
}

void Cairo_Graph_close(void)
{
	G_debug(1, "Cairo_Graph_close");

	write_image();

	if (cairo)
	{
		cairo_destroy(cairo);
		cairo = NULL;
	}
	if (surface)
	{
		cairo_surface_destroy(surface);
		surface = NULL;
	}
}

void init_cairo(void)
{
	G_debug(1, "init_cairo");

	/* create cairo surface */
	switch (file_type)
	{
	case FTYPE_PNG:
		surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
		break;
#if CAIRO_HAS_PDF_SURFACE
	case FTYPE_PDF:
		surface = (cairo_surface_t *) cairo_pdf_surface_create(file_name, (double) width, (double) height);
		break;
#endif
#if CAIRO_HAS_PS_SURFACE
	case FTYPE_PS:
		surface = (cairo_surface_t *) cairo_ps_surface_create(file_name, (double) width, (double) height);
		break;
#endif
#if CAIRO_HAS_SVG_SURFACE
	case FTYPE_SVG:
		surface = (cairo_surface_t *) cairo_svg_surface_create(file_name, (double) width, (double) height);
		break;
#endif
	default:
		G_fatal_error("Unknown Cairo surface type");
		break;
	}

	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS)
		G_fatal_error("Failed to initialize Cairo surface");

	cairo = cairo_create(surface);

	Cairo_Erase();
}

/* Returns TRUE if string ends with suffix (case insensitive) */
int ends_with(const char *string, const char *suffix)
{
	if (strlen(string) < strlen(suffix))
		return FALSE;

	return G_strcasecmp(suffix, string + strlen(string) - strlen(suffix)) == 0;
}
