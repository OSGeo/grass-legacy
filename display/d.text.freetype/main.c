/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.text.freetype
*
* AUTHOR(S):    Huidae Cho <hdcho@geni.cemtlo.com>
*
* PURPOSE:      d.text with FreeType2 support
*               http://www.freetype.org
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif
#include <freetype/freetype.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

#define	DEFAULT_CHARSET		"ISO-8859-1"
#define	DEFAULT_SIZE		"30"
#define	DEFAULT_COLOR		"gray"

#define	DEFAULT_ROTATION	"0"

typedef struct {
	char	*font, *path, *charset, *color, *size;
} capinfo;

static capinfo	*fonts = NULL;
static int	fonts_count = 0;
static int	cur_font = -1;
static char	*font_names = NULL;

static FT_Library	library = NULL;
static FT_Face		face = NULL;
static int		driver = 0;

static void	error(const char *msg);
static void	read_capfile(void);
static int	find_font(const char *name);
static char 	*transform_string(char *str, int (*func)(int));


int
main(int argc, char **argv)
{
	struct	GModule	*module;
	struct	Option	*opt1, *opt2, *opt3, *opt4, *opt5, *opt6, *opt7, *opt8;
	struct	Flag	*flag1;
	unsigned char	*text, *path, *charset, *tcolor;
	int	size, color;
	double	east, north, rotation;
	int	i, j, k, l, r, g, b, ch, index;
#ifdef HAVE_ICONV_H
	iconv_t	cd;
#endif
	int	ol;
	unsigned char	*p1, *p2, *out, *buffer;
	FT_Matrix	matrix;
	FT_Vector	pen;


	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Draws text on the graphics monitor using TrueType fonts.";

	read_capfile();

	opt1 = G_define_option();
	opt1->key        = "text";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "Text";

	opt2 = G_define_option();
	opt2->key        = "east_north";
	opt2->type       = TYPE_DOUBLE;
	opt2->required   = NO;
	opt2->key_desc   = "east,north";
	opt2->description= "Coordinates";

	opt3 = NULL;
	if(fonts_count)
	{
	opt3 = G_define_option();
	opt3->key        = "font";
	opt3->type       = TYPE_STRING;
	opt3->required   = NO;
	if(cur_font >= 0)
	opt3->answer     = fonts[cur_font].font;
	opt3->options    = font_names;
	opt3->description= "Font name";
	}

	opt4 = G_define_option();
	opt4->key        = "path";
	opt4->type       = TYPE_STRING;
	opt4->required   = fonts_count ? NO : YES;
	opt4->description= "Font path";

	opt5 = G_define_option();
	opt5->key        = "charset";
	opt5->type       = TYPE_STRING;
	opt5->required   = NO;
	opt5->description= "Character encoding";

	opt6 = G_define_option();
	opt6->key        = "color";
	opt6->type       = TYPE_STRING;
	opt6->required   = NO;
	opt6->description= "Color";

	opt7 = G_define_option();
	opt7->key        = "size";
	opt7->type       = TYPE_INTEGER;
	opt7->required   = NO;
	opt7->description= "Size";

	opt8 = G_define_option();
	opt8->key        = "rotation";
	opt8->type       = TYPE_DOUBLE;
	opt8->required   = NO;
	opt8->answer     = DEFAULT_ROTATION;
	opt8->description= "Rotation angle";


	flag1 = G_define_flag();
	flag1->key       = 'r';
	flag1->description= "Rotation angle in radian";


	if(G_parser(argc, argv))
		exit(1);

	text = opt1->answer;

	if((opt3 && !opt3->answer && !opt4->answer) || (!opt3 && !opt4->answer))
		G_fatal_error("No font selected");

	charset = NULL;
	tcolor = NULL;
	size = 0;

	if(opt3 && opt3->answer)
	{
		cur_font = find_font(opt3->answer);
		if(cur_font < 0)
			G_fatal_error("Invalid font: %s", opt3->answer);

		path = fonts[cur_font].path;
		charset = transform_string(fonts[cur_font].charset, toupper);
		tcolor = transform_string(fonts[cur_font].color, tolower);
		size = atoi(fonts[cur_font].size);
	}

	if(opt4->answer)
		path = opt4->answer;

	if(opt5->answer)
		charset = transform_string(opt5->answer, toupper);
	if(opt6->answer)
		tcolor = transform_string(opt6->answer, tolower);
	if(opt7->answer)
		size = atoi(opt7->answer);

	if(!charset)
		charset = DEFAULT_CHARSET;
	if(!tcolor)
		tcolor = DEFAULT_COLOR;
	if(!size)
		size = atoi(DEFAULT_SIZE);

	fprintf(stdout, "Font=<%s:%s:%s:%d>\n\n", path, charset, tcolor, size);

	rotation = atof(opt8->answer);
	if(!flag1->answer)
		rotation *= M_PI / 180.0;

	if(R_open_driver() != 0)
		error("No graphics device selected");
	driver = 1;

	if(FT_Init_FreeType(&library))
		error("Unable to initialise FreeType");

	if(FT_New_Face(library, path, 0, &face))
		error("Unable to create face");

	if(FT_Set_Pixel_Sizes(face, size, 0))
		error("Unable to set size");

	l = strlen(text);

	ol = 4 * (l + 1);
	out = (unsigned char *) G_malloc(ol);

#ifdef HAVE_ICONV_H
	p1 = text;
	p2 = out;
	i = ol;
	if((cd = iconv_open("UCS-4", charset)) < 0)
		error("Unable to create conversion context");

	if(iconv(cd, (const char **)&p1, &l, (char **)&p2, &i) < 0)
		error("Conversion error");

	iconv_close(cd);
	ol -= i;
#else
	for(i = 0; i <= l; i++)
	{
		out[2*i+0] = 0;
		out[2*i+1] = 0;
		out[2*i+2] = 0;
		out[2*i+3] = text[i];
	}
	ol = l * 4;
#endif

	D_setup(0);

	if(opt2->answer)
	{
		east  = atof(opt2->answers[0]);
		north = atof(opt2->answers[1]);
		pen.x = (FT_Pos)D_u_to_d_col(east);
		pen.y = (FT_Pos)D_u_to_d_row(north);
	}
	else
	{
		fprintf(stdout, "Click!\n");
		fprintf(stdout, " Left:    Place text here\n");
		fprintf(stdout, " Right:   Quit\n");

		R_get_location_with_pointer((int *)&pen.x, (int *)&pen.y, &i);
		i &= 0x0f;
		if(i != 1)
		{
			FT_Done_Face(face);
			FT_Done_FreeType(library);
			R_close_driver();
			exit(1);
		}
		east  = D_d_to_u_col((double)pen.x);
		north = D_d_to_u_row((double)pen.y);
	}

	R_color_table_fixed();

	if(sscanf(tcolor, "0x%02x%02x%02x", &r, &g, &b) == 3)
	{
		color = 1;
		R_reset_color(r, g, b, color);
	}
	else
		color = D_translate_color(tcolor);

	if(!color)
	{
		G_warning("%s: No such color", tcolor);
		color = D_translate_color(DEFAULT_COLOR);
	}

	matrix.xx = (FT_Fixed)( cos(rotation)*0x10000);
	matrix.xy = (FT_Fixed)(-sin(rotation)*0x10000);
	matrix.yx = (FT_Fixed)( sin(rotation)*0x10000);
	matrix.yy = (FT_Fixed)( cos(rotation)*0x10000);

	pen.x *= 64;
	pen.y = - pen.y * 64;

	for(i=0; i<ol; i+=4)
	{
		FT_Set_Transform(face, &matrix, &pen);

		ch = (out[i+2] << 8) | out[i+3];

		if(!(index = FT_Get_Char_Index(face, ch)))
			continue;
		if(FT_Load_Glyph(face, index, FT_LOAD_DEFAULT))
			continue;
		if(FT_Render_Glyph(face->glyph, ft_render_mode_mono))
			continue;

		l = face->glyph->bitmap.rows * face->glyph->bitmap.width;

		buffer = (unsigned char *) G_malloc(l);
		memset(buffer, 0, l);

		k = face->glyph->bitmap.width / 8 +
			(face->glyph->bitmap.width % 8 ? 1 : 0);

		for(j=0; j<l; j++)
		{
			if(face->glyph->bitmap.buffer[
				(j / face->glyph->bitmap.width) * k +
				(j % face->glyph->bitmap.width) / 8
			   ] & (1 << (7 - (j % face->glyph->bitmap.width) % 8)))
				buffer[j] = color;
		}

		for(j=0; j<face->glyph->bitmap.rows; j++)
		{
			R_move_abs(face->glyph->bitmap_left,
					- face->glyph->bitmap_top + j);
			R_raster_char(face->glyph->bitmap.width,
					1, 0,
					buffer + face->glyph->bitmap.width * j);
		}

		G_free(buffer);

		pen.x += face->glyph->advance.x;
		pen.y += face->glyph->advance.y;
	}

	if(opt2->answer)
		D_add_to_list(G_recreate_command());

	FT_Done_Face(face);
	FT_Done_FreeType(library);
	R_close_driver();

	exit(0);
}

static void
error(const char *msg)
{
	if(face)
		FT_Done_Face(face);
	if(library)
		FT_Done_FreeType(library);
	if(driver)
		R_close_driver();
	G_fatal_error("%s", msg);

	return;
}

static void
read_capfile(void)
{
	char freetypecap[4096], buf[4096];
	char ifont[128], ipath[4096], icharset[32], icolor[128], isize[10];
	int font_names_size = 0;
	FILE *fp;
	int i;

	sprintf(freetypecap, "%s/etc/freetypecap", G_gisbase());
	fp = fopen(freetypecap, "r");

	if(!fp)
	{
		G_warning("%s: No FreeType definition file", freetypecap);
		return;
	}

	while(fgets(buf, sizeof(buf), fp) && !feof(fp))
	{
		capinfo *font;
		int offset;
		char *ptr;

		ptr = strchr(buf, '#');
		if(ptr)
			*ptr = 0;

		if(sscanf(buf, "%[^:]:%[^:]:%[^:]:%[^:]:%[^:]",
			  ifont, ipath, icharset, icolor, isize) != 5)
			continue;

		if(access(ipath, R_OK))
			continue;

		fonts = (capinfo *)
			G_realloc(fonts, (fonts_count + 1) * sizeof(capinfo));

		font = &fonts[fonts_count];

		offset = (ifont[0] == '*') ? 1 : 0;

		if(offset > 0 && cur_font < 0)
			cur_font = fonts_count;

		font->font    = G_store(ifont + offset);
		font->path    = G_store(ipath);
		font->charset = G_store(icharset);
		font->color   = G_store(icolor);
		font->size    = G_store(isize);

		fonts_count++;
	}

	fclose(fp);

	font_names_size = 0;
	for(i = 0; i < fonts_count; i++)
		font_names_size += strlen(fonts[i].font) + 1;

	font_names = (char *) G_malloc(font_names_size);
	font_names[0] = '\0';
	for(i = 0; i < fonts_count; i++)
	{
		if(i > 0)
			strcat(font_names, ",");
		strcat(font_names, fonts[i].font);
	}

	return;
}

static int
find_font(const char *name)
{
	int i;
	for(i = 0; i < fonts_count; i++)
		if(strcasecmp(fonts[i].font, name) == 0)
			return i;

	return -1;
}

static char *
transform_string(char *str, int (*func)(int))
{
	char *result = G_store(str);
	int i;

	for(i = 0; result[i]; i++)
		result[i] = (*func)(result[i]);

	return result;
}

