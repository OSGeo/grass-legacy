/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"FILTERMAP",
"",
"This program performs center weighted averaging on a moving",
"3 by 3 matrix.  The purpose is to smooth the input data in",
"a particular manner.  The greater the number of filtering",
"iterations, the more the data will be smoothed.",
0
};

main (argc,argv) char *argv[];
{
    char input[30], *in_mapset;
    char out[30], *out_mapset;
    char command[1024];
    int i;
    int num_passes;


    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
        printf ("%s\n", intro[i]);
    
    in_mapset = G_ask_cell_old ("enter input file name :",
      input);
    if (!in_mapset) exit(2);

    out_mapset = G_ask_cell_new ("enter [filtered] output file name :",
      out);
    if (!out_mapset) exit(2);

    num_passes = ask_int ("enter number of filter iterations");

/* go into background now, running the backend function */

    sprintf (command,
      "Gfiltermap -v 'in=%s in %s' out=%s passes=%d",
       input, in_mapset, out, num_passes );

    system (command);

}
