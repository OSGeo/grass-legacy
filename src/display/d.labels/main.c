#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "gis.h"
#define MAIN
#include "config.h"
#include "local_proto.h"

char *element = "paint/labels" ;
/* temp panel file for save/restore ops */
char *panel_save = NULL;

static void exit_func (void) {
	if (panel_save)
		remove (panel_save);
}

int 
main (int argc, char **argv)
{
    char name[30];
    char *mapset;
    char *tempfile;
    FILE *in;
    int stat, option, new_file;

    atexit (exit_func);

	/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    option = 0;

    mapset = G_ask_any ("enter new or existing paint labels file", 
			    name, element, "labels", 0);
    if (mapset == NULL)
	  exit(0);

    if(G_find_file (element, name, mapset) == NULL)
    {
	new_file = 1;
	file_status = 0;
    }
    else
    {
	new_file = 0;
	file_status = 1;
    }
    in = G_fopen_old (element, name, mapset);

    G_clear_screen();

    tempfile = G_tempfile();

    G_zero (&config, sizeof config);
    strcpy (config.ref,"center");
    strcpy (config.color,"black");
    strcpy (config.width,"1");
    strcpy (config.hcolor,"none");
    strcpy (config.hwidth,"0");
    strcpy (config.background,"white");
    strcpy (config.border,"black");
    strcpy (config.size,"500");
    strcpy (config.opaque,"yes");
    strcpy (config.font,"standard");

    chk_status = 1;
    if (!new_file)  display_all(in);
    stat = 1;
    while (stat != 0)
      {
      G_clear_screen(); 
      fprintf (stdout,"PAINT LABELS\n");

      if (!in)
           in = G_fopen_old (element, name, mapset);

      if (!new_file) option = get_location(1,config.east,config.north);
      config.count = 0;

           /* edit existing labels */
      if (option == 1)   
           {
           G_clear_screen();
           stat = process_old (in, name, tempfile);  
           rewind (in);
           }

            /* add labels */
      else if (option == 0 || option == 3)
              {
              G_clear_screen();
              stat = process_new (in, name, tempfile);
	      if (stat < 0) break;
	      if (stat == 1)
		 {
                 if(!new_file) fclose (in);
	         in = 0;
                 sav_file(tempfile, name);
		 new_file = 0;
		 }
              }

            /* option = 2, quit */
      else stat = 0; 

    } /* end of while stat != 0 */
 
    if (!file_status) G_remove(element,name);
    unlink (tempfile);
    fclose (in);

    exit(0);
}

int sav_file (char *input, char *output)
{
	int in, out;
	int n;
	char buf[1024];

        if (!file_status) file_status = 1;
	in = open (input, 0);
	if (in < 0)
	{
	  fprintf (stdout,"error in sav_file input\n");
	    perror (input);
	    exit(1);
	}
	out = G_open_new (element, output);
	if (out < 0)
	  {
	  fprintf (stdout,"error in sav_file output\n");
	  perror (output);
	  exit(1);
	  }
	while ((n = read (in, buf, sizeof buf)) > 0)
	    write (out, buf, n);

	close (in);
	close (out);

	return 0;
}
