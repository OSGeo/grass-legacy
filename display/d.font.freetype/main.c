#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

#define	USE_FREETYPECAP
#ifdef	USE_FREETYPECAP
typedef struct	{
	char	*font, *path, *charset;
} capinfo;

static int read_capfile(char *capfile, capinfo **fonts, int *fonts_count,
		int *cur_font, char **font_names);
static int find_font(capinfo *fonts, int fonts_count, char *name);
#endif

int
main( int argc , char **argv )
{
    struct GModule *module;
    struct Option *opt1;
    struct Option *opt2;
#ifdef	USE_FREETYPECAP
    struct Flag *flag1;

    capinfo *fonts;
    int fonts_count, i;
#endif
    char *font, *charset;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		_("Selects the font in which text will be displayed "
		"on the user's graphics monitor.");

    opt1 = G_define_option() ;
    opt1->key        = "font";
    opt1->type       = TYPE_STRING;
    opt1->required   = NO;
#ifdef	USE_FREETYPECAP
    opt1->description= _("Font name (not supported interactively, see also -l flag) or path to TrueType font (including file name)");
#else
    opt1->description= _("Path to TrueType font (including file name)");
#endif
    /* opt1->gisprompt  = "old_file,,TrueType font"; */

    opt2 = G_define_option() ;
    opt2->key        = "charset" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = NO;
    opt2->answer     = "UTF-8";
    opt2->description= _("Character encoding");

#ifdef	USE_FREETYPECAP
    flag1 = G_define_flag();
    flag1->key         = 'l';
    flag1->description = "list fonts defined in freetypecap";
#endif

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Check command line */
    if (G_parser(argc, argv))
            exit(-1);

#ifdef	USE_FREETYPECAP
    read_capfile(getenv("GRASS_FREETYPECAP"), &fonts, &fonts_count, NULL, NULL);
    if(flag1->answer){
	    if(fonts_count){
		    for(i=0; i<fonts_count; i++)
			    fprintf(stdout, "%s\n", fonts[i].font);
	    }
	    exit(0);
    }
#endif

    font = opt1->answer;
    charset = opt2->answer;

    /* load the font */
    if (R_open_driver() != 0)
	G_fatal_error (_("No graphics device selected"));

	if(!font) {
		release();
		exit(-1);
	}else{
		int len = strlen(font);
		if(len==0) {
			release();
			exit(-1);
		}
#ifdef	USE_FREETYPECAP
		if(fonts_count){
			i = find_font(fonts, fonts_count, font);
			if(i >= 0){
				font = fonts[i].path;
				charset = fonts[i].charset;
			}
		}
#endif
	}

	if(isTrueTypeFont(font)==-1) {
		G_fatal_error("Invalid font: %s", font);
		exit(-1);
	}

	R_font_freetype(font) ;
	R_charset(charset) ;

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

#ifdef	USE_FREETYPECAP
static int
read_capfile(char *capfile, capinfo **fonts, int *fonts_count, int *cur_font,
		char **font_names)
{
	char	file[4096], *ptr;
	int	i, font_names_size = 0;
	char	buf[4096], ifont[128], ipath[4096], icharset[32];
	FILE	*fp;

	*fonts = NULL;
	*fonts_count = 0;
	if(cur_font)
		*cur_font = -1;
	if(font_names)
		*font_names = NULL;

	ptr = file;
	sprintf(file, "%s/etc/freetypecap", G_gisbase());
	if(capfile)
	{
		if(access(capfile, R_OK))
			G_warning("%s: Unable to read FreeType definition file; use the default", capfile);
		else
			ptr = capfile;
	}
	if(ptr == file && access(ptr, R_OK))
	{
		G_warning("%s: No FreeType definition file", ptr);
		return -1;
	}
	if(!(fp = fopen(ptr, "r")))
	{
		G_warning("%s: Unable to read FreeType definition file", ptr);
		return -1;
	}

	while(fgets(buf, sizeof(buf), fp) && !feof(fp))
	{
		capinfo *font;
		int offset;
		char *p;

		p = strchr(buf, '#');
		if(p)
			*p = 0;

		if(sscanf(buf, "%[^:]:%[^:]:%[^:]", ifont,ipath,icharset) != 3)
			continue;

		if(access(ipath, R_OK))
			continue;

		*fonts = (capinfo *)
			G_realloc(*fonts, (*fonts_count + 1) * sizeof(capinfo));

		font = &((*fonts)[*fonts_count]);

		offset = (ifont[0] == '*') ? 1 : 0;

		if(cur_font && offset > 0 && *cur_font < 0)
			*cur_font = *fonts_count;

		font->font    = G_store(ifont + offset);
		font->path    = G_store(ipath);
		font->charset = G_store(icharset);

		(*fonts_count)++;
	}

	fclose(fp);

	if(!font_names)
		return 0;

	font_names_size = 0;
	for(i = 0; i < *fonts_count; i++)
		font_names_size += strlen((*fonts)[i].font) + 1;
	
	G_debug(3,"font_names_size: %d", font_names_size);
	*font_names = (char *) G_malloc(font_names_size);
	(*font_names)[0] = '\0';
	for(i = 0; i < *fonts_count; i++)
	{
		if(i > 0)
			strcat(*font_names, ",");
		strcat(*font_names, (*fonts)[i].font);
	}

	return 0;
}

static int
find_font(capinfo *fonts, int fonts_count, char *name)
{
	int i;

	for(i = 0; i < fonts_count; i++)
		if(strcasecmp(fonts[i].font, name) == 0)
			return i;

	return -1;
}
#endif
