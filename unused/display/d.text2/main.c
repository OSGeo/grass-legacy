#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <giconv.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "ft2build.h"
#include FT_FREETYPE_H

#define	CHARSET		"EUC-KR"
#define	DEFAULT_FONT	"/usr/X11R6/lib/X11/fonts/TrueType/gulim.ttf";
#define	DEFAULT_SIZE	"10"
#define	DEFAULT_COLOR	"gray"

#define	chr2hex(c)	((c) - (isdigit(c)? '0' : 'a' - 10))


int
main(int argc, char **argv)
{
	struct	GModule	*module;
	struct	Option	*opt1, *opt2, *opt3, *opt4, *opt5, *opt6;
	struct	Cell_head	cellhd;
	unsigned char	*text, *font, tcolor[128];
	int	size, color;
	double	east, north;
	FT_Library	library;
	FT_Face		face;
	int	i, j, k, l, x, y, ch, index;
#ifdef	CHARSET
	iconv_t	cd;
	int	ol;
	unsigned char	*out, *p1, *p2;
#endif

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
	opt2->required   = NO;
	opt2->answer     = DEFAULT_FONT;
	opt2->description= "Font";

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
	opt5->key        = "east";
	opt5->type       = TYPE_DOUBLE;
	opt5->required   = NO;
	opt5->description= "East";

	opt6 = G_define_option();
	opt6->key        = "north";
	opt6->type       = TYPE_DOUBLE;
	opt6->required   = NO;
	opt6->description= "North";

	if(G_parser(argc, argv))
		exit(-1);

	text = opt1->answer;
	font = opt2->answer;
	size = atoi(opt3->answer);
	l = strlen(opt4->answer);
	for(i=0; i<=l; i++)
		tcolor[i] = tolower(opt4->answer[i]);

	if(R_open_driver() != 0)
		G_fatal_error("No graphics device selected");

	if(FT_Init_FreeType(&library)){
		R_close_driver();
		G_fatal_error("FreeType error 1");
	}

	if(FT_New_Face(library, font, 0, &face)){
		FT_Done_FreeType(library);
		R_close_driver();
		G_fatal_error("FreeType error 2");
	}

	/*
	if(FT_Set_Char_Size(face, size*64, 0, 300, 300)){
		FT_Done_Face(face);
		R_close_driver();
		G_fatal_error("FreeType error 3");
	}
	*/

	/*
	for(i=0; i<face->num_fixed_sizes; i++)
		printf("%d %d\n",
				face->available_sizes[i].width,
				face->available_sizes[i].height);
	*/

	if(FT_Set_Pixel_Sizes(face, size, 0)){
		FT_Done_Face(face);
		FT_Done_FreeType(library);
		R_close_driver();
		G_fatal_error("FreeType error 3");
	}

	l = strlen(text);
#ifdef	CHARSET
	ol = 3 * l + 1;
	if(!(out = (char *)G_malloc(ol))){
		FT_Done_Face(face);
		FT_Done_FreeType(library);
		R_close_driver();
		G_fatal_error("G_malloc error 1");
	}
	p1 = text;
	p2 = out;
	i = ol;
	if((cd = iconv_open("UCS-2", CHARSET)) < 0){
		FT_Done_Face(face);
		FT_Done_FreeType(library);
		R_close_driver();
		G_free(out);
		G_fatal_error("iconv error 1");
	}

	if(iconv(cd, (const char **)&p1, &l, (char **)&p2, &i) < 0){
		FT_Done_Face(face);
		FT_Done_FreeType(library);
		R_close_driver();
		G_free(out);
		G_fatal_error("iconv error 2");
	}
	iconv_close(cd);
	l = ol - i;
#endif

	G_get_set_window(&cellhd);
	if(opt5->answer && opt6->answer){
		east  = atof(opt5->answer);
		north = atof(opt6->answer);
		x = G_easting_to_col(east, &cellhd);
		y = G_northing_to_row(north, &cellhd);
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
#ifdef	CHARSET
			G_free(out);
#endif
			exit(1);
		}
		east  = G_col_to_easting(x, &cellhd);
		north = G_row_to_northing(y, &cellhd);
	}

	fprintf(stdout, "%f(E) %f(N)\n", east, north);
	R_color_table_fixed();

	if(strstr(tcolor, "0x") == (char *)tcolor && strlen(tcolor) == 8){
		printf("%s\n", tcolor);
		i = (chr2hex(tcolor[2]) << 4) | chr2hex(tcolor[3]);
		j = (chr2hex(tcolor[4]) << 4) | chr2hex(tcolor[5]);
		k = (chr2hex(tcolor[6]) << 4) | chr2hex(tcolor[7]);
		printf("%x %x %x\n", i, j, k);
		color = 1;
		R_reset_color(i, j, k, color);
	}else
		color = D_translate_color(tcolor);

	if(!color){
		G_warning("%s: No such color", tcolor);
		color = D_translate_color(DEFAULT_COLOR);
	}

	for(i=0; i<l; i++){
#ifdef	CHARSET
		ch = (out[i] << 8) | out[++i];
#else
		ch = text[i];
#endif
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
		printf("%d %d\n", face->glyph->bitmap.width, face->glyph->bitmap.rows);
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

#ifdef	CHARSET
	G_free(out);
#endif
	FT_Done_Face(face);
	FT_Done_FreeType(library);

	R_close_driver();

	exit(0);
}

