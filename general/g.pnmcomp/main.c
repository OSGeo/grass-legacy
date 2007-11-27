/*
 * MODULE:       g.pnmcomp
 * AUTHOR(S):    Glynn Clements
 * PURPOSE:      g.pnmcomp isn't meant for end users. It's an internal tool for use by
 * 		 a Tcl/Tk GUI.
 * 		 In essence, g.pnmcomp generates a PPM image by overlaying a series of
 * 		 PPM/PGM pairs (PPM = RGB image, PGM = alpha channel).
 * COPYRIGHT:    (C) 2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <grass/gis.h>
#include <grass/glocale.h>

static int width, height;
static char *in_buf;
static char *mask_buf;
static char *out_buf;

static void
erase(char *buf, const char *color)
{
	unsigned char *p = buf;
	int r, g, b;
	unsigned char bg[3];
	int row, col, i;

	if (sscanf(color, "%d:%d:%d", &r, &g, &b) != 3)
		G_fatal_error("Invalid color: %s", color);

	bg[0] = (unsigned char) r;
	bg[1] = (unsigned char) g;
	bg[2] = (unsigned char) b;

	for (row = 0; row < height; row++)
		for (col = 0; col < width; col++)
			for (i = 0; i < 3; i++)
				*p++ = bg[i];
}

static int
read_line(char *buf, int size, FILE *fp)
{
	for (;;)
	{
		if (!fgets(buf, size, fp))
			G_fatal_error("Error reading PPM file.");

		if (buf[0] != '#')
			return 0;
	}
}

static void
read_header(FILE *fp, char *magic, int *maxval)
{
	int ncols, nrows;
	char buf[80];

	read_line(buf, sizeof(buf), fp);

	if (sscanf(buf, "P%c", magic) != 1)
		G_fatal_error("Invalid PPM file.");

	read_line(buf, sizeof(buf), fp);

	if (sscanf(buf, "%d %d", &ncols, &nrows) != 2)
		G_fatal_error("Invalid PPM file.");

	if (ncols != width || nrows != height)
		G_fatal_error(
			"Expecting %dx%d image but got %dx%d image.",
			width, height, ncols, nrows);

	read_line(buf, sizeof(buf), fp);

	if (sscanf(buf, "%d", maxval) != 1)
		G_fatal_error("Invalid PPM file.");
}

static void
read_pnm(const char *filename, char *buf, int components)
{
	unsigned char magic;
	int maxval;
	unsigned char *p;
	int row, col, i;
	FILE *fp;

	fp = fopen(filename, "rb");
	if (!fp)
		G_fatal_error("File '%s' not found", filename);

	read_header(fp, &magic, &maxval);

	switch (magic)
	{
	case '2':
	case '5':
		if (components == 3)
			G_fatal_error("Expecting PPM but got PGM");
		break;
	case '3':
	case '6':
		if (components == 1)
			G_fatal_error("Expecting PGM but got PPM");
		break;
	default:
		G_fatal_error("Invalid magic number: 'P%c'.", magic);
		break;
	}

	p = buf;

	for (row = 0; row < height; row++)
	{
		switch (magic)
		{
		case '2':
			for (col = 0; col < width; col++)
			{
				int y;

				if (fscanf(fp, "%d", &y) != 1)
					G_fatal_error("Invalid PGM file.");
				*p++ = (unsigned char) y;
			}
			break;
		case '3':
			for (col = 0; col < width; col++)
			{
				int r, g, b;

				if (fscanf(fp, "%d %d %d", &r, &g, &b) != 3)
					G_fatal_error("Invalid PPM file.");
				*p++ = (unsigned char) r;
				*p++ = (unsigned char) g;
				*p++ = (unsigned char) b;
			}
			break;
		case '5':
			if (fread(p, 1, width, fp) != width)
				G_fatal_error("Invalid PGM file.");
			p += width;
			break;
		case '6':
			if (fread(p, 3, width, fp) != width)
				G_fatal_error("Invalid PPM file.");
			p += 3 * width;
			break;
		}
	}

	p = buf;

	if (maxval != 255)
		for (row = 0; row < height; row++)
			for (col = 0; col < width; col++)
				for (i = 0; i < components; i++)
				{
					*p = *p * 255 / maxval;
					p++;
				}

	fclose(fp);
}

static void
overlay(void)
{
	const unsigned char *p = in_buf;
	const unsigned char *q = mask_buf;
	unsigned char *r = out_buf;
	int row, col, i;

	for (row = 0; row < height; row++)
		for (col = 0; col < width; col++)
		{
			int c1 = *q++;
			int c0 = 255 - c1;

			switch (c1)
			{
			case 0:
				p += 3;
				r += 3;
				break;
			case 255:
				*r++ = *p++;
				*r++ = *p++;
				*r++ = *p++;
				break;
			default:
				for (i = 0; i < 3; i++)
				{
					*r = (*r * c0 + *p * c1) / 255;
					p++;
					r++;
				}
				break;
			}
		}
}

static void
overlay_alpha(float alpha)
{
	const unsigned char *p = in_buf;
	const unsigned char *q = mask_buf;
	unsigned char *r = out_buf;
	int row, col, i;

	for (row = 0; row < height; row++)
		for (col = 0; col < width; col++)
		{
			int c = *q++;
			int c1 = (int) (c * alpha);
			int c0 = 255 - c1;

			if (!c)
			{
				p += 3;
				r += 3;
				continue;
			}

			for (i = 0; i < 3; i++)
			{
				*r = (*r * c0 + *p * c1) / 256;
				p++;
				r++;
			}
		}
}

static void
write_ppm(const char *filename, const char *buf)
{
	const unsigned char *p = buf;
	FILE *fp;

	fp = fopen(filename, "wb");
	if (!fp)
		G_fatal_error("Unable to open file '%s'", filename);

	fprintf(fp, "P6\n%d %d\n255\n", width, height);

	if (fwrite(p, 3 * width, height, fp) != height)
		G_fatal_error("Error writing PPM file.");

	fclose(fp);
}

int 
main(int argc, char *argv[])
{
	struct GModule *module;
	struct {
		struct Option *in, *mask, *alpha, *out, *width, *height, *bg;
	} opt;
	int i;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->keywords = _("general");
    module->description =
		"Overlays multiple PPM image files.";

	opt.in = G_define_option();
	opt.in->key		= "input";
	opt.in->type		= TYPE_STRING;
	opt.in->required	= YES;
	opt.in->multiple	= YES;
	opt.in->description	= "Names of input files.";

	opt.mask = G_define_option();
	opt.mask->key		= "mask";
	opt.mask->type		= TYPE_STRING;
	opt.mask->multiple	= YES;
	opt.mask->description	= "Names of mask files.";

	opt.alpha = G_define_option();
	opt.alpha->key		= "opacity";
	opt.alpha->type		= TYPE_DOUBLE;
	opt.alpha->multiple	= YES;
	opt.alpha->description	= "Layer opacities.";

	opt.out = G_define_option();
	opt.out->key		= "output";
	opt.out->type		= TYPE_STRING;
	opt.out->required	= YES;
	opt.out->description	= "Name of output file.";

	opt.width = G_define_option();
	opt.width->key		= "width";
	opt.width->type		= TYPE_INTEGER;
	opt.width->required	= YES;
	opt.width->description	= "Image width.";

	opt.height = G_define_option();
	opt.height->key		= "height";
	opt.height->type	= TYPE_INTEGER;
	opt.height->required	= YES;
	opt.height->description	= "Image height.";

	opt.bg = G_define_option();
	opt.bg->key		= "background";
	opt.bg->type		= TYPE_STRING;
	opt.bg->description	= "Background color.";

	if (G_parser(argc, argv) < 0)
		return 1;

	width  = atoi(opt.width->answer);
	height = atoi(opt.height->answer);

	in_buf = G_malloc(width * height * 3);
	mask_buf = G_malloc(width * height);
	out_buf = G_malloc(width * height * 3);

	if (opt.bg->answer)
		erase(out_buf, opt.bg->answer);

	for (i = 0; opt.in->answers[i]; i++)
	{
		char *infile = opt.in->answers[i];
		char *maskfile = opt.mask->answer
			? opt.mask->answers[i]
			: NULL;

		if (!maskfile)
			opt.mask->answer = NULL;

		if (maskfile && *maskfile)
		{
			read_pnm(infile, in_buf, 3);
			read_pnm(maskfile, mask_buf, 1);
			if (opt.alpha->answer)
			{
				float alpha = atof(opt.alpha->answers[i]);

				if (alpha == 1.0)
					overlay();
				else
					overlay_alpha(alpha);
			}
			else
				overlay();
		}
		else
			read_pnm(infile, out_buf, 3);
	}

	write_ppm(opt.out->answer, out_buf);

	return 0;
}

