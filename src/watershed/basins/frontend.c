/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"BASINS",
"",
"This program attempts to delineate subbasin boundaries within",
"a given waterhsed.  Files telling of branching nodes from the",
"stream network and of drainage direction are needed as input.",
"It is assumed that this program will be used as part of the",
"series of programs developing watershed topography, so the",
"branching nodes are assumed to be associated with a thinned",
"stream network cell file.  Output will include a cell file ",
"depicting subbasins for each stream segment with the category",
"numbering for the subbasins being related to the numbering for",
"the stream segments.  The window information from the stream",
"network input file is used.  A file telling information about",
"the types of nodes will also be output.",
0
};

main (argc,argv) char *argv[];
{
    char pointer[30], *ptr_mapset;
    char stream[30], *str_mapset;
    char basin[30], *bas_mapset;
    char command[1024];
    int i;


    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
        printf ("%s\n", intro[i]);
    
    ptr_mapset = G_ask_cell_old ("enter drainage directin input file name :",
      pointer);
    if (!ptr_mapset) exit(2);

    str_mapset = G_ask_cell_old ("enter thinned network file :",
      stream);
    if (!str_mapset) exit(2);

    bas_mapset = G_ask_cell_new ("enter subbasin file name :",
      basin);
    if (!bas_mapset) exit(2);

/* go into background now, running the backend function */

    sprintf (command,
      "Gbasins -v 'drain=%s in %s' 'stream=%s in %s' basin=%s",
       pointer, ptr_mapset, stream, str_mapset, basin );

    system (command);

}
