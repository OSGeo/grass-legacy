#include <stdlib.h>
#include <string.h>

#include "config.h"
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif

#include <ft2build.h>
#include FT_FREETYPE_H
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "glocale.h"

#include <stdio.h>
/* #define DEBUG_LOG(S) {FILE *fp = fopen("debug.TXT","a");fputs(S,fp);fclose(fp);} */

int isTrueTypeFont(char*);
int release();

int
main( int argc , char **argv )
{
    struct GModule *module;
    struct Option *opt1;
    struct Option *opt2;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		_("Selects the font in which text will be displayed "
		"on the user's graphics monitor.");

    opt1 = G_define_option() ;
    opt1->key        = "font";
    opt1->type       = TYPE_STRING;
    opt1->required   = NO;
    opt1->description= _("Path to TrueType font (including file name)");
    opt1->gisprompt  = "old_file,,TrueType font";

    opt2 = G_define_option() ;
    opt2->key        = "charset" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = NO;
    opt2->answer     = "EUC-JP";
    opt2->description= _("Character encoding");

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Check command line */
    if (G_parser(argc, argv))
            exit(-1);

    /* load the font */
    if (R_open_driver() != 0)
	G_fatal_error (_("No graphics device selected"));

	if(!opt1->answer) {
		release();
		exit(-1);
	}else{
		int len = strlen(opt1->answer);
		if(len==0) {
			release();
			exit(-1);
		}
	}

	if(isTrueTypeFont(opt1->answer)==-1) {
		G_fatal_error("Invalid font: %s", opt1->answer);
		exit(-1);
	}

	R_font_freetype(opt1->answer) ;
	R_charset(opt2->answer) ;

    /* add this command to the list */
	D_add_to_list(G_recreate_command()) ;

    R_close_driver();

    exit(0);
}

int isTrueTypeFont(char* filename) {

	FT_Library		library;
	FT_Face			face;
	FT_Error		ans;

	/* set freetype */
	ans = FT_Init_FreeType(&library);
	if(ans) {
		return -1;
	}
	ans = FT_New_Face(library,filename,0,&face);
	if(ans==FT_Err_Unknown_File_Format) {
		FT_Done_FreeType(library);
		return -1;
	}else if(ans) {
		FT_Done_FreeType(library);
		return -1;
	}

	/* FT_done */
	FT_Done_Face(face);
	FT_Done_FreeType(library);
	
	return 0;
}

int release() {
	R_font_freetype_release();
	R_font("romans");
	fprintf(stdout,_("\nSetting release of FreeType\n"));
	return 0;
}

