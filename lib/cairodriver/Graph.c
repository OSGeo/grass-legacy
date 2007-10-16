#include "cairodriver.h"
#include <cairo-ps.h>
#include <cairo-pdf.h>
#include <cairo-svg.h>

/* globals */
char *file_name;
int file_type;
int width, height;
int modified;

/* background color */
double bgcolor_r, bgcolor_g, bgcolor_b, bgcolor_a;

/* cairo objects */
cairo_surface_t *surface;
cairo_t *cairo;

int Cairo_Graph_set(int argc, char **argv)
{
	char *c;

	G_debug(1, "Cairo_Graph_set");
	G_gisinit("Cairo driver");

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
	else if (ends_with(c, ".pdf"))
		file_type = FTYPE_PDF;
	else if (ends_with(c, ".ps"))
		file_type = FTYPE_PS;
	else if (ends_with(c, ".svg"))
		file_type = FTYPE_SVG;
	else
		G_fatal_error("Unknown file extension: %s", c);
	G_debug(1, "File type: %s (%d)", c, file_type);

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

	finish_drawing_op();
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
	case FTYPE_PDF:
		surface = (cairo_surface_t *) cairo_pdf_surface_create(file_name, (double) width, (double) height);
		break;
	case FTYPE_PS:
		surface = (cairo_surface_t *) cairo_ps_surface_create(file_name, (double) width, (double) height);
		break;
	case FTYPE_SVG:
		surface = (cairo_surface_t *) cairo_svg_surface_create(file_name, (double) width, (double) height);
		break;
	default:
		G_fatal_error("Unknown Cairo surface type");
		break;
	}

	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS)
		G_fatal_error("Failed to initialize Cairo surface");

	/* create cairo context */
	cairo = cairo_create(surface);

	/*
	   Translate drawing operations 1/2 pixel for pixel-based formats.
	   This is because in cairo, the center coordinates of the top left
	   pixel are (0.5, 0.5), not (0, 0). Since grass monitors receive
	   integer coordinates, if we didn't do this all points would
	   fall between two screen pixels, creating a blurry look
	   when antialiasing is turned on.
	 */
	if (cairo_surface_get_type(surface) == CAIRO_SURFACE_TYPE_IMAGE)
		cairo_translate(cairo, 0.5, 0.5);

	/* TODO: set up line join and line cap style. Example: */
	/*
	   cairo_set_line_join(cairo, CAIRO_LINE_JOIN_ROUND);
	   cairo_set_line_cap(cairo, CAIRO_LINE_CAP_ROUND);
	 */

	/* clear background */
	Cairo_Erase();
}

/* Returns TRUE if string ends with suffix (case insensitive) */
int ends_with(const char *string, const char *suffix)
{
	if (strlen(string) < strlen(suffix))
		return FALSE;

	return !strcasecmp(suffix, string + strlen(string) - strlen(suffix));
}
