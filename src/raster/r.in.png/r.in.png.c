/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       r.in.png
 * AUTHOR(S):    Michael Shapiro - CERL
 *               Alex Shevlakov - sixote@yahoo.com
 *               Glynn Clements - glynn.clements@virgin.net
 * PURPOSE:      Import non-georeferenced Images in PNG format. 
 * COPYRIGHT:    (C) 2000-2002 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <png.h>

#include "gis.h"

#define C_Y 0
#define C_P 1
#define C_R 2
#define C_G 3
#define C_B 4
#define C_A 5

typedef struct
{
    int active;
    const char suffix[4];
    int fd;
    CELL *buf;
    CELL maxval;
    char name[256];
} channel;

static int Verbose;
static int Header;

static char *input, *output, *title;
static double dgamma, alpha;
  
static int intensity(double k, double gamma)
{
    return (int) (pow(k, 1.0 / gamma) * 255 + 0.5);
}

static int get_byte(png_bytep *pp)
{
    return *(*pp)++;
}

static CELL get_png_val(png_bytep *pp, int bit_depth)
{
    CELL c = (bit_depth == 16)
	? (get_byte(pp) << 8) | get_byte(pp)
	: get_byte(pp);

    return c;
}

static void init_channel(channel *c)
{
    c->active = 1;
    sprintf(c->name, "%s%s", output, c->suffix);
    c->fd = G_open_cell_new(c->name);
    c->buf = G_allocate_cell_buf();
}

static void read_png(void)
{
    channel channels[6] = {
	{0, ""},
	{0, ""},
	{0, ".r"},
	{0, ".g"},
	{0, ".b"},
	{0, ".a"}
    };
    char sig_buf[8];
    png_structp png_ptr;
    png_infop info_ptr;
    const char *type_string = "";
    const char *alpha_string = "";
    png_bytep png_buffer;
    png_bytep *png_rows;
    int linesize;
    struct Cell_head cellhd;
    int x, y, c;
    png_color_8p sig_bit;
    int sbit, interlace;
    double gamma;
    FILE *ifp;

    ifp = fopen(input, "rb");
    if (!ifp)
	G_fatal_error("unable to open PNG file %s", input);

    if (fread(sig_buf, sizeof(sig_buf), 1, ifp) != 1)
	G_fatal_error("input file empty or too short");

    if (png_sig_cmp(sig_buf, 0, sizeof(sig_buf)) != 0)
	G_fatal_error("input file not a PNG file");

    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
				     NULL, NULL, NULL);
    if (!png_ptr)
	G_fatal_error("cannot allocate PNG structure");

    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr)
	G_fatal_error("cannot allocate PNG structures");

    if (setjmp(png_jmpbuf(png_ptr)))
	G_fatal_error("PNG error");

    png_init_io(png_ptr, ifp);
    png_set_sig_bytes(png_ptr, sizeof(sig_buf));

    png_read_info(png_ptr, info_ptr);

    if (Header)
    {
	switch (info_ptr->color_type)
	{
	case PNG_COLOR_TYPE_GRAY:
	    type_string = "gray";
	    alpha_string = "";
	    break;

	case PNG_COLOR_TYPE_GRAY_ALPHA:
	    type_string = "gray";
	    alpha_string = "+alpha";
	    break;

	case PNG_COLOR_TYPE_PALETTE:
	    type_string = "palette";
	    alpha_string = "";
	    break;

	case PNG_COLOR_TYPE_RGB:
	    type_string = "truecolor";
	    alpha_string = "";
	    break;

	case PNG_COLOR_TYPE_RGB_ALPHA:
	    type_string = "truecolor";
	    alpha_string = "+alpha";
	    break;
	}

	fprintf(stderr, "%ld x %ld image, %d bit%s %s%s\n",
		info_ptr->width, info_ptr->height,
		info_ptr->bit_depth, info_ptr->bit_depth > 1 ? "s" : "",
		type_string, alpha_string);

	fclose(ifp);    
	exit(0);
    }

    if (Verbose)
    {
	char gamma_string[80] = "";

	switch (info_ptr->color_type)
	{
	case PNG_COLOR_TYPE_GRAY:
	    type_string = "gray";
	    alpha_string = "";
	    fprintf(stderr,"Type of png image is grey\n");
	    break;

	case PNG_COLOR_TYPE_GRAY_ALPHA:
	    type_string = "gray";
	    alpha_string = "+alpha";
	    fprintf(stderr,"Type of png image is grey with alpha\n");
	    break;

	case PNG_COLOR_TYPE_PALETTE:
	    type_string = "palette";
	    alpha_string = "";
	    fprintf(stderr,"Type of png image is palette\n");
	    break;

	case PNG_COLOR_TYPE_RGB:
	    type_string = "truecolor";
	    alpha_string = "";
	    fprintf(stderr,"Type of png image is rgb\n");
	    break;

	case PNG_COLOR_TYPE_RGB_ALPHA:
	    type_string = "truecolor";
	    alpha_string = "+alpha";
	    fprintf(stderr,"Type of png image is rgb with alpha\n");
	    break;
	}

	if (info_ptr->valid & PNG_INFO_tRNS)
	    alpha_string = "+transparency";

	if (info_ptr->valid & PNG_INFO_gAMA)
	    sprintf(gamma_string, ", image gamma = %4.2f", info_ptr->gamma);

	fprintf(stderr, "reading a %ld x %ld image, %d bit%s %s%s%s%s\n",
		info_ptr->width, info_ptr->height,
		info_ptr->bit_depth, info_ptr->bit_depth > 1 ? "s" : "",
		type_string, alpha_string, gamma_string,
		info_ptr->interlace_type ? ", Adam7 interlaced" : "");
    }

    if (png_get_bit_depth(png_ptr, info_ptr) < 8)
	png_set_packing(png_ptr);

    if (png_get_sBIT(png_ptr, info_ptr, &sig_bit))
    {
	sbit = 1;
        png_set_shift(png_ptr, sig_bit);
	channels[C_R].maxval = (1 << sig_bit->red  ) - 1;
	channels[C_G].maxval = (1 << sig_bit->green) - 1;
	channels[C_B].maxval = (1 << sig_bit->blue ) - 1;
	channels[C_Y].maxval = (1 << sig_bit->gray ) - 1;
	channels[C_A].maxval = (1 << sig_bit->alpha) - 1;
    }
    else
	sbit = 0;

    if (!png_get_gAMA(png_ptr, info_ptr, &gamma))
	gamma = 0.0;

    if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
	png_set_tRNS_to_alpha(png_ptr);

    png_read_update_info(png_ptr, info_ptr);

    interlace = (info_ptr->interlace_type != PNG_INTERLACE_NONE);

    if (!sbit)
    {
	channels[C_R].maxval = (1 << info_ptr->bit_depth) - 1;
	channels[C_G].maxval = (1 << info_ptr->bit_depth) - 1;
	channels[C_B].maxval = (1 << info_ptr->bit_depth) - 1;
	channels[C_Y].maxval = (1 << info_ptr->bit_depth) - 1;
	channels[C_A].maxval = (1 << info_ptr->bit_depth) - 1;
    }

    linesize = png_get_rowbytes(png_ptr, info_ptr);

    png_buffer = G_malloc(interlace
			  ? info_ptr->height * linesize
			  : linesize);

    if (interlace)
    {
	png_rows = G_malloc(info_ptr->height * sizeof(png_bytep));
	for (y = 0; y < info_ptr->height; y++)
	    png_rows[y] = png_buffer + y * linesize;
    }

    if(G_get_window(&cellhd) < 0)
	G_fatal_error("Unable to get window");

    cellhd.rows = info_ptr->height;
    cellhd.cols = info_ptr->width;
    cellhd.north = cellhd.rows;
    cellhd.south = 0.0;
    cellhd.east = cellhd.cols;
    cellhd.west = 0.0;
    cellhd.ns_res = 1;
    cellhd.ew_res = 1;

    if (G_set_window(&cellhd) < 0)
	G_fatal_error("Unable to set window");

    switch (info_ptr->color_type)
    {
    case PNG_COLOR_TYPE_GRAY:
	init_channel(&channels[C_Y]);
	break;

    case PNG_COLOR_TYPE_GRAY_ALPHA:
	init_channel(&channels[C_Y]);
	init_channel(&channels[C_A]);
	break;

    case PNG_COLOR_TYPE_PALETTE:
	init_channel(&channels[C_P]);
	break;

    case PNG_COLOR_TYPE_RGB:
	init_channel(&channels[C_R]);
	init_channel(&channels[C_G]);
	init_channel(&channels[C_B]);
	break;

    case PNG_COLOR_TYPE_RGB_ALPHA:
	init_channel(&channels[C_R]);
	init_channel(&channels[C_G]);
	init_channel(&channels[C_B]);
	init_channel(&channels[C_A]);
	break;
    }

    if (interlace)
	png_read_image(png_ptr, png_rows);

    for (y = 0; y < info_ptr->height; y++)
    {
	png_bytep p;

	if (interlace)
	    p = png_rows[y];
	else
	{
	    png_read_row(png_ptr, png_buffer, NULL);
	    p = png_buffer;
	}

	for (x = 0; x < info_ptr->width; x++)
	    for (c = 0; c < 6; c++)
		if (channels[c].active)
		    channels[c].buf[x] = get_png_val(&p, info_ptr->bit_depth);

	if (channels[C_A].active && alpha >= 0.0)
	    for (c = 0; c < 6; c++)
		if (c != C_A && channels[c].active)
		    for (x = 0; x < info_ptr->width; x++)
			if (channels[C_A].buf[x] <= alpha)
			    G_set_c_null_value(&channels[c].buf[x], 1);

	for (c = 0; c < 6; c++)
	    if (channels[c].active)
		G_put_c_raster_row(channels[c].fd, channels[c].buf);
    }

    png_read_end(png_ptr, NULL);

    fclose(ifp);

    if (Verbose)
	fprintf(stderr, "CREATING SUPPORT FILES FOR %s\n", output);

    if (gamma != 0.0 && dgamma != 0.0)
	gamma *= dgamma;

    for (c = 0; c < 6; c++)
    {
	channel *ch = &channels[c];
	CELL i0 = 0;
	CELL i1 = ch->maxval;
	struct Colors colors;
	int i;

	if (!ch->active)
	    continue;

	G_close_cell(ch->fd);

	if (title && *title)
	    G_put_cell_title(ch->name, title);

	G_init_colors(&colors);

	if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE)
	{
	    for (i = 0; i < info_ptr->num_palette; i++)
	    {
		png_colorp col = &info_ptr->palette[i];
		G_set_color((CELL) i, col->red, col->green, col->blue, &colors);
	    }
	}
	else if (c == C_A || dgamma == 0.0 || gamma == 0.0)
	    G_add_c_raster_color_rule(&i0,   0,   0,   0,
				      &i1, 255, 255, 255,
				      &colors);
	else
	    for (i = 0; i <= i1; i++)
	    {
		int v = intensity((double) i / i1, gamma);
		G_set_color((CELL) i, v, v, v, &colors);
	    }

	G_write_colors(ch->name, G_mapset(), &colors);
    }

    G_free(png_buffer);
    if (interlace)
	G_free(png_rows);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
}

int main(int argc, char *argv[])
{
    struct Option *inopt, *outopt, *titleopt, *gammaopt, *alphaopt;
    struct Flag *vflag, *hflag;
    struct GModule *module;
  
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = "Import non-georeferenced PNG format image.";

    inopt = G_define_option();
    inopt->key		= "input";
    inopt->type		= TYPE_STRING;
    inopt->required	= YES;
    inopt->description	= "Name of input PNG file.";

    outopt = G_define_option();
    outopt->key		= "output";
    outopt->type	= TYPE_STRING;
    outopt->required	= YES;
    outopt->gisprompt	= "new,cell,raster";
    outopt->description	= "Name of new raster file.";

    titleopt = G_define_option();
    titleopt->key	= "title";
    titleopt->type	= TYPE_STRING;
    titleopt->required	= NO;
    titleopt->description	= "Title for new raster file.";

    gammaopt = G_define_option();
    gammaopt->key	= "gamma";
    gammaopt->type	= TYPE_DOUBLE;
    gammaopt->required	= NO;
    gammaopt->description	= "Display gamma.";

    alphaopt = G_define_option();
    alphaopt->key	= "alpha";
    alphaopt->type	= TYPE_DOUBLE;
    alphaopt->required	= NO;
    alphaopt->description	= "Alpha threshold.";

    hflag = G_define_flag();
    hflag->key		= 'h';
    hflag->description	= "Output image file header only.";

    vflag = G_define_flag();
    vflag->key		= 'v';
    vflag->description	= "Verbose mode on.";

    if(G_parser(argc, argv))
	exit(1);

    input = inopt->answer;
    output = outopt->answer;
    title = titleopt->answer;
    dgamma = gammaopt->answer ? atof(gammaopt->answer) : 0.0;
    alpha = alphaopt->answer ? atof(alphaopt->answer) : -1.0;

    Verbose = vflag->answer;
    Header  = hflag->answer;

    read_png();

    return 0;
}

