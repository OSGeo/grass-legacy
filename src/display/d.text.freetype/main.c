/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.text2
*
* AUTHOR(S):    Huidae Cho <hdcho@geni.cemtlo.com>
*
* PURPOSE:      d.text with Freetype2 support
*               http://www.freetype.org
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iconv.h>
#include <freetype/freetype.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

#define	DEFAULT_CHARSET	"ISO-8859-1"
#define	DEFAULT_SIZE	"10"
#define	DEFAULT_COLOR	"gray"

#define	chr2hex(c)	((c) - (isdigit(c)? '0' : 'a' - 10))

static FT_Library	library;
static FT_Face		face;
static int		driver;

static void
error(const char *msg)
{
	if (face)
		FT_Done_Face(face);
	if (library)
		FT_Done_FreeType(library);
	if (driver)
		R_close_driver();
	G_fatal_error("%s", msg);
}

int
main(int argc, char **argv)
{
	struct	GModule	*module;
	struct	Option	*opt1, *opt2, *opt3, *opt4, *opt5, *opt6;
	unsigned char	*text, *font, tcolor[128], charset[32];
	int	size, color;
	double	east, north;
	int	i, j, k, l, x, y, ch, index;
	iconv_t	cd;
	int	ol;
	unsigned char	*p1, *p2;
	unsigned char	*out;

	module = G_define_module();
	module->description =
		"Displays text.";

	G_gisinit(argv[0]);

	opt1 = G_define_option();
	opt1->key        = "text";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "Text";

	opt2 = G_define_option();
	opt2->key        = "font";
	opt2->type       = TYPE_STRING;
	opt2->required   = YES;
	opt2->description= "Font file [with path]";

	opt3 = G_define_option();
	opt3->key        = "size";
	opt3->type       = TYPE_INTEGER;
	opt3->required   = NO;
	opt3->answer     = DEFAULT_SIZE;
	opt3->description= "Size";

	opt4 = G_define_option();
	opt4->key        = "color";
	opt4->type       = TYPE_STRING;
	opt4->required   = NO;
	opt4->answer     = DEFAULT_COLOR;
	/*
	opt4->options    = D_color_list();
	*/
	opt4->description= "Color";

	opt5 = G_define_option();
	opt5->key        = "east_north";
	opt5->type       = TYPE_DOUBLE;
	opt5->required   = NO;
	opt5->key_desc   = "east,north";
	opt5->description= "Coordinates";

	opt6 = G_define_option();
	opt6->key        = "charset";
	opt6->type       = TYPE_STRING;
	opt6->required   = NO;
	opt6->answer     = DEFAULT_CHARSET;
	opt6->description= "CharSet";

	if(G_parser(argc, argv))
		exit(-1);

	text = opt1->answer;
	font = opt2->answer;
	size = atoi(opt3->answer);
	l = strlen(opt4->answer);
	for(i=0; i<=l; i++)
		tcolor[i] = tolower(opt4->answer[i]);
	l = strlen(opt6->answer);
	for(i=0; i<=l; i++)
		charset[i] = toupper(opt6->answer[i]);

	if(R_open_driver() != 0)
		error("No graphics device selected");
	driver = 1;

	if(FT_Init_FreeType(&library))
		error("Unable to initialise FreeType");

	if(FT_New_Face(library, font, 0, &face))
		error("Unable to create face");

	if(FT_Set_Pixel_Sizes(face, size, 0))
		error("Unable to set size");

	l = strlen(text);

	ol = 3 * l + 1;

	if(!(out = (unsigned char *) G_malloc(ol)))
		error("Unable to allocate memory");

	p1 = text;
	p2 = (char *) out;
	i = ol;
	if((cd = iconv_open("UCS-2", charset)) < 0)
		error("Unable to create conversion context");

	if(iconv(cd, (const char **)&p1, &l, (char **)&p2, &i) < 0)
		error("Conversion error");

	iconv_close(cd);
	l = ol - i;

	D_setup(0);

	if(opt5->answer){
		east  = atof(opt5->answers[0]);
		north = atof(opt5->answers[1]);
		x = (int)D_u_to_d_col(east);
		y = (int)D_u_to_d_row(north);
	}else{
		fprintf(stdout, "Click!\n");
		fprintf(stdout, " Left:    Place text here\n");
		fprintf(stdout, " Right:   Quit\n");

		R_get_location_with_pointer(&x, &y, &i);
		i &= 0xf;
		if(i != 1){
			FT_Done_Face(face);
			FT_Done_FreeType(library);
			R_close_driver();
			G_free(out);
			exit(1);
		}
		east  = D_d_to_u_col((double)x);
		north = D_d_to_u_row((double)y);
	}

	fprintf(stdout, "%f(E) %f(N)\n", east, north);
	R_color_table_fixed();

	if(strstr(tcolor, "0x") == (char *)tcolor && strlen(tcolor) == 8){
		i = (chr2hex(tcolor[2]) << 4) | chr2hex(tcolor[3]);
		j = (chr2hex(tcolor[4]) << 4) | chr2hex(tcolor[5]);
		k = (chr2hex(tcolor[6]) << 4) | chr2hex(tcolor[7]);
		/*
		fprintf(stdout, "%s: %x %x %x\n", tcolor, i, j, k);
		*/
		color = 1;
		R_reset_color(i, j, k, color);
	}else
		color = D_translate_color(tcolor);

	if(!color){
		G_warning("%s: No such color", tcolor);
		color = D_translate_color(DEFAULT_COLOR);
	}

	for(i=0; i<l; i += 2){
		ch = (out[i] << 8) | out[i+1];

		if(!(index = FT_Get_Char_Index(face, ch)))
			continue;
		if(FT_Load_Glyph(face, index, 0))
			continue;
		if(FT_Render_Glyph(face->glyph, 0))
			continue;
		k = face->glyph->bitmap.rows * face->glyph->bitmap.width;
		for(j=0; j<k; j++)
			if(face->glyph->bitmap.buffer[j])
				face->glyph->bitmap.buffer[j] = color;
		x += face->glyph->bitmap_left;
		y -= face->glyph->bitmap_top;
		/*
		fprintf(stdout, "%d %d\n", face->glyph->bitmap.width, face->glyph->bitmap.rows);
		*/
		for(j=0; j<face->glyph->bitmap.rows; j++){
			R_move_abs(x, y+j);
			R_raster_char(face->glyph->bitmap.width,
					1, 0,
					face->glyph->bitmap.buffer
					+ face->glyph->bitmap.width * j);
		}
		x += face->glyph->advance.x >> 6;
		y += face->glyph->bitmap_top;
	}

	G_free(out);

	D_add_to_list(G_recreate_command());

	FT_Done_Face(face);
	FT_Done_FreeType(library);
	R_close_driver();

	exit(0);
}

