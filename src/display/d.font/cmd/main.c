#include "gis.h"
#include "display.h"
#include "D.h"
#include <string.h>
#include "raster.h"

int main( int argc , char **argv )
{
        char fonts[2048];
        char buf[1024];
        FILE *fd;
		struct GModule *module;
        struct Option *opt1;
        int i;

		module = G_define_module();
		module->description =
			"Selects the font in which text will be displayed "
			"on the user's graphics monitor.";

        /* find out what fonts we have */
        *fonts = 0;
        sprintf (buf, "ls %s/fonts", G_gisbase());
        fd = popen(buf,"r");
        if (fd != NULL)
        {
                while (fscanf(fd, "%s", buf)==1)
                {
                        if (*fonts) strcat(fonts, ",");
                        strcat(fonts,buf);
                }
                pclose(fd);
        }
        if (*fonts == 0)
        {
                fprintf (stderr, "ERROR: no fonts available\n");
                exit(1);
        }

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
        R_open_driver();
        R_font(opt1->answer) ;

        /* add this command to the list */
		D_add_to_list(G_recreate_command()) ;

        R_close_driver();

        exit(0);
}


