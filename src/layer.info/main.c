/* %W% %G% */

#include "gis.h"
/*---------------------------------------------------------------------
 |      layer_info
 |      Map Layer Information Table
 |
 */

main (argc, argv)   char *argv[];
{
    char *mapset;
    char name[50];
    char *tempfile;
    char command[512];
    struct Cell_head cellhd;
    struct Categories cats;
    struct History hist;
    struct Reclass reclass;
    int ok;

    G_gisinit(argv[0]);

    mapset = G_ask_cell_old ("", name);
    if (!mapset)
        exit(0);

    tempfile = G_tempfile () ;
    if (tempfile == NULL)
	G_fatal_error ("Could not open tempfile\n");


    ok = 1;
    G_suppress_warnings(1);
    if(G_get_cellhd (name, mapset, &cellhd) < 0 ||
       G_read_cats (name, mapset, &cats) < 0    ||
       G_read_history (name, mapset, &hist) < 0 ||
       G_get_reclass (name, mapset, &reclass) < 0)
	ok = 0;
    G_suppress_warnings(0);


    sprintf (command, "Glayer.info \"%s in %s\" > %s", name, mapset, tempfile);
    system (command);

    if (!ok)
    {
        printf("please note above problems. hit RETURN to continue -->");
	G_gets(command);
    }

    more_print(tempfile);
    return (0);
}
