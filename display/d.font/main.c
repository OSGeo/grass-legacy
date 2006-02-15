#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/D.h>
#include <grass/raster.h>

int main( int argc , char **argv )
{
        char fonts[2048];
        char buf[1024];
	DIR *dirp;
	struct dirent *dp;
	struct GModule *module;
        struct Option *opt1;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
			"Selects the font in which text will be displayed "
			"on the user's graphics monitor.";

        /* find out what fonts we have */
        *fonts = 0;
        sprintf (buf, "%s/fonts", G_gisbase());
	if ((dirp = opendir(buf)) != NULL)
        {
                while ((dp = readdir(dirp)) != NULL)
                {
			if(dp->d_name[0] == '.')
				continue;

                        if (*fonts) strcat(fonts, ",");
                        strcat(fonts, dp->d_name);
                }
                closedir(dirp);
        }
        if (*fonts == 0)
                G_fatal_error("ERROR: no fonts available");

        opt1 = G_define_option() ;
        opt1->key        = "font" ;
        opt1->type       = TYPE_STRING ;
        opt1->required   = YES ;
        opt1->options    = fonts;
        opt1->answer     = "romans";
        opt1->description= "Choose new current font" ;

        /* Initialize the GIS calls */
        G_gisinit(argv[0]) ;

        /* Check command line */
        if (G_parser(argc, argv))
                exit(-1);

        /* load the font */
        if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	R_font_freetype_release();
        R_font(opt1->answer) ;

        /* add this command to the list */
	D_add_to_list(G_recreate_command()) ;

        R_close_driver();

        exit(0);
}


