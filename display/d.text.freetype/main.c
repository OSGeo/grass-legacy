/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.text.freetype
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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif
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
	struct	Option	*opt1, *opt2, *opt3, *opt4, *opt5, *opt6, *opt7;
	unsigned char	*text, *path, charset[32], tcolor[128];
	int	size, color;
	double	east, north;
	int	i, j, k, l, x, y, ch, index;
#ifdef HAVE_ICONV_H
	iconv_t	cd;
#endif
	int	ol, cn, cf;
	unsigned char	*p1, *p2, *out;
	char	freetypecap[128], buf[512], *ptr,
		ifont[128], ipath[128], icharset[32], icolor[128], isize[10],
		**cfont, **cpath, **ccharset, **ccolor, **csize,
		*lfont;
	FILE	*fp;

	module = G_define_module();
	module->description =
		"Displays text.";

	G_gisinit(argv[0]);

	sprintf(freetypecap, "%s/etc/freetypecap", G_gisbase());

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

	lfont = NULL;
	cf = -1;
	cn = 0;

	if(!(fp=fopen(freetypecap, "r"))){
		G_warning("%s: No FreeType definition file", freetypecap);

		opt3 = NULL;

		opt4 = G_define_option();
		opt4->key        = "path";
		opt4->type       = TYPE_STRING;
		opt4->required   = YES;
		opt4->description= "FontPath";
	}else{
		while(fgets(buf, 512, fp) && !feof(fp)){
			if((ptr = strchr(buf, '#')))
				*ptr = 0;
			if(sscanf(buf, "%[^:]:%[^:]:%[^:]:%[^:]:%[^:]",
				ifont, ipath, icharset, icolor, isize) != 5)
				continue;
			if(!cn){
				cfont = (char **)G_malloc((cn+1)*sizeof(char *));
				cpath = (char **)G_malloc((cn+1)*sizeof(char *));
				ccharset = (char **)G_malloc((cn+1)*sizeof(char *));
				ccolor = (char **)G_malloc((cn+1)*sizeof(char *));
				csize = (char **)G_malloc((cn+1)*sizeof(char *));
			}else{
				cfont = (char **)G_realloc(cfont, (cn+1)*sizeof(char *));
				cpath = (char **)G_realloc(cpath, (cn+1)*sizeof(char *));
				ccharset = (char **)G_realloc(ccharset, (cn+1)*sizeof(char *));
				ccolor = (char **)G_realloc(ccolor, (cn+1)*sizeof(char *));
				csize = (char **)G_realloc(csize, (cn+1)*sizeof(char *));
			}
			if(!cfont || !cpath || !ccharset || !ccolor || !csize)
				error("Unable to allocate memory");
			cfont[cn] = (char *)G_malloc(strlen(ifont)+1);
			cpath[cn] = (char *)G_malloc(strlen(ipath)+1);
			ccharset[cn] = (char *)G_malloc(strlen(icharset)+1);
			ccolor[cn] = (char *)G_malloc(strlen(icolor)+1);
			csize[cn] = (char *)G_malloc(strlen(isize)+1);

			if(!cfont[cn] || !cpath[cn] || !ccharset[cn] ||
					!ccolor[cn] || !csize[cn])
				error("Unable to allocate memory");

			strcpy(cfont[cn], ifont);
			strcpy(cpath[cn], ipath);
			strcpy(ccharset[cn], icharset);
			strcpy(ccolor[cn], icolor);
			strcpy(csize[cn], isize);

			i = strlen(cfont[cn]) - (cfont[cn][0]=='*' ? 1 : 0);
			if(!lfont){
				l = 0;
				lfont = (char *)G_malloc(i+1);
			}else{
				l = strlen(lfont);
				lfont = (char *)G_realloc(lfont, l+i+2);
			}
			if(!lfont)
				error("Unable to allocate memory");
			lfont[l] = 0;
			if(l)
				strcat(lfont, ",");
			strcat(lfont, cfont[cn] + (cfont[cn][0]=='*' ? 1 : 0));

			if(cfont[cn][0] == '*' && cf < 0)
				cf = cn;

			cn++;
		}
		fclose(fp);

		opt3 = G_define_option();
		opt3->key        = "font";
		opt3->type       = TYPE_STRING;
		opt3->required   = NO;
		if(cf >= 0)
			opt3->answer     = cfont[cf] + 1;
		opt3->options    = lfont;
		opt3->description= "FontName";

		opt4 = G_define_option();
		opt4->key        = "path";
		opt4->type       = TYPE_STRING;
		opt4->required   = NO;
		if(cf >= 0)
			opt4->answer     = cpath[cf];
		opt4->description= "FontPath";
	}

	opt5 = G_define_option();
	opt5->key        = "charset";
	opt5->type       = TYPE_STRING;
	opt5->required   = NO;
	if(cf >= 0)
		opt5->answer     = ccharset[cf];
	else
		opt5->answer     = DEFAULT_CHARSET;
	opt5->description= "CharSet";

	opt6 = G_define_option();
	opt6->key        = "color";
	opt6->type       = TYPE_STRING;
	opt6->required   = NO;
	if(cf >= 0)
		opt6->answer     = ccolor[cf];
	else
		opt6->answer     = DEFAULT_COLOR;
	/*
	opt6->options    = D_color_list();
	*/
	opt6->description= "Color";

	opt7 = G_define_option();
	opt7->key        = "size";
	opt7->type       = TYPE_INTEGER;
	opt7->required   = NO;
	if(cf >= 0)
		opt7->answer     = csize[cf];
	else
		opt7->answer     = DEFAULT_SIZE;
	opt7->description= "Size";

	if(G_parser(argc, argv))
		exit(-1);

	text = opt1->answer;

	charset[0] = 0;
	tcolor[0] = 0;
	size = -1;

	if(opt3){
		if(!opt3->answer && !opt4->answer){
			if(cn){
				for(i=0; i<cn; i++){
					G_free(cfont[i]);
					G_free(cpath[i]);
					G_free(ccharset[i]);
					G_free(ccolor[i]);
					G_free(csize[i]);
				}
				G_free(cfont);
				G_free(cpath);
				G_free(ccharset);
				G_free(ccolor);
				G_free(csize);

				G_free(lfont);
			}
			error("No selected font");
		}
		if(opt4->answer)
			path = opt4->answer;
		else{
			for(cf=0; cf<cn; cf++){
				ptr = cfont[cf] + (cfont[cf][0]=='*' ? 1 : 0);
				if(!strcmp(opt3->answer, ptr)){
					path = cpath[cf];
					l = strlen(ccharset[cf]);
					for(i=0; i<=l; i++)
						charset[i] = toupper(ccharset[cf][i]);
					l = strlen(ccolor[cf]);
					for(i=0; i<=l; i++)
						tcolor[i] = tolower(ccolor[cf][i]);
					size = atoi(csize[cf]);
					break;
				}
			}
		}
	}else
	if(!opt4->answer)
		error("No font selected");

	if(opt5->answer){
		l = strlen(opt5->answer);
		for(i=0; i<=l; i++)
			charset[i] = toupper(opt5->answer[i]);
	}

	if(opt6->answer){
		l = strlen(opt6->answer);
		for(i=0; i<=l; i++)
			tcolor[i] = tolower(opt6->answer[i]);
	}

	if(opt7->answer)
		size = atoi(opt7->answer);

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

	ol = 2 * l;

	if(!(out = (unsigned char *) G_malloc(ol)))
		error("Unable to allocate memory");

#ifdef HAVE_ICONV_H
	p1 = text;
	p2 = (char *) out;
	i = ol;
	if((cd = iconv_open("UCS-2", charset)) < 0)
		error("Unable to create conversion context");

	if(iconv(cd, (const char **)&p1, &l, (char **)&p2, &i) < 0)
		error("Conversion error");

	iconv_close(cd);
	l = ol - i;
#else
	for (i = 0; i < l; i++)
	{
		out[2*i+0] = '\0';
		out[2*i+1] = text[i];
	}
	l *= 2;
#endif

	D_setup(0);

	if(opt2->answer){
		east  = atof(opt2->answers[0]);
		north = atof(opt2->answers[1]);
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

	R_color_table_fixed();

	if(strstr(tcolor, "0x") == (char *)tcolor && strlen(tcolor) == 8){
		i = (chr2hex(tcolor[2]) << 4) | chr2hex(tcolor[3]);
		j = (chr2hex(tcolor[4]) << 4) | chr2hex(tcolor[5]);
		k = (chr2hex(tcolor[6]) << 4) | chr2hex(tcolor[7]);

		color = 1;
		R_reset_color(i, j, k, color);
	}else
		color = D_translate_color(tcolor);

	if(!color){
		G_warning("%s: No such color", tcolor);
		color = D_translate_color(DEFAULT_COLOR);
	}

	for(i=0; i<l; i+=2){
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

	if(cn){
		for(i=0; i<cn; i++){
			G_free(cfont[i]);
			G_free(cpath[i]);
			G_free(ccharset[i]);
			G_free(ccolor[i]);
			G_free(csize[i]);
		}
		G_free(cfont);
		G_free(cpath);
		G_free(ccharset);
		G_free(ccolor);
		G_free(csize);

		G_free(lfont);
	}

	exit(0);
}

