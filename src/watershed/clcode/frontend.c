/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"STREAM CODE",
"",
"This program takes a thinned stream network and numbers each",
"branch of the stream as a separate category.  The outlet ",
"point and downstream direction are determined through the ",
"use of the drainage accumulation values; hence, the drainage",
"accumulation file is needed as input.  The cell output giving",
"numbered stream branches is optional. Automatic output is ",
"saved which lists the node coordinates and the length of each ",
"channel segment in meters.  This output is needed to complete ",
"the watershed generation series.",
0
};

main (argc,argv) char *argv[];
{
    char stream[30], *str_mapset;
    char link_name[30], *link_mapset;
    char river_name[30], *river_mapset;
    char ptr_name[30], *ptr_mapset;
    char command[1024];
    int i;
    char buf[100];


    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
        printf ("%s\n", intro[i]);
    
    river_mapset = G_ask_cell_old ("enter drainage accumulation file:",
      river_name);
    if (!river_mapset) exit(2);

    ptr_mapset = G_ask_cell_old("enter drainage direction file:", ptr_name);
    if (!ptr_mapset) exit(2);

    str_mapset = G_ask_cell_old ("enter thinned stream network file :",
      stream);
    if (!str_mapset) exit(2);

    G_set_ask_return_msg("if you don't want this product");
    link_mapset = G_ask_cell_new("enter coded network output file: ",
       link_name);

/* go into background now, running the backend function */

    sprintf (command,
      "G%s -v 'accum=%s in %s' 'stream=%s in %s' 'drain=%s in %s'",
       argv[0], river_name, river_mapset, stream, str_mapset,
       ptr_name, ptr_mapset);

    if (link_mapset != NULL) 
    {
        strcat(command, "out=");
        strcat(command, link_name);
    }

    system (command);

}
