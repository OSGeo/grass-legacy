/*  @(#)a_b_main.c	2.1  6/26/87  */
#include <stdio.h>
#include <Vect.h>
#include "gis.h"

#define MAIN
#include "dlghead.h"

/*
*	old version is for 1.0 release dlg files
*	new version is for 2.0 release and beyond
*/

/*
printf("\nUsage: %s old-dlg-ascii-file new-dlg-binary-file\n",
*/
#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
#define         A_DLG           "dlg"
#define         ATT             "dig_att"
#define         PLUS            "dig_plus"

doit (dig_name, a_dlg_name, force_areas)
    char * dig_name, a_dlg_name;
    int force_areas;
{
    char *mapset;
    char a_dlgfile[300];
    struct Map_info Map;

    int i;
    FILE *dlg;
    FILE *fopen();
    char filename[300];
    char  *rindex();

    extern	int	new_format;

    /* Print warning */
    if ((mapset = G_find_file2 (A_DLG, a_dlg_name, "")) == NULL)
    {
	fprintf (stderr, "Ascii DLG file <%s> not found.\n", a_dlg_name);
	exit(-1);
    }
    G__file_name (a_dlgfile, A_DLG, a_dlg_name, mapset);



    if ( (dlg = fopen(a_dlgfile, "r")) == NULL)
    {
	printf("Can't find %s\n", "dlg");
	exit(-1);
    }


    /* Read the header section */
    if (read_dlg_head(dlg) == -1)
	G_fatal_error ("Error reading DLG header");

    /* Check to see if we are going to make multiple files.  Binary
    * files contain only one category per file while "optional" DLG
    * files can contain up to 32 category overlays.
    */

    if (num_cats == 1)
    {
	if (0 > Vect_open_new (&Map, dig_name))
	    G_fatal_error ("Not able to create vector file <%s>");

	/* Write binary dlg head */
/*
**	if (write_bdlg_head (bin, 0) == -1)
**	{
**	    printf("Error in writing binary dlg header\n");
**	    exit (-1);
**	}
*/

	/* Read and write the main body */
	if (dlg_to_dig (dlg, &Map) == -1)
	{
	    printf("Error in translating header\n");
	    exit (-1);
	}

	fclose (dlg);
	Vect_close (&Map);
    }
    else
    {
	printf("This dlg file contains %d overlays.  Each overlay will be\n",
	    num_cats);
	printf("written to a different file\n");

	for (i=1; i<=num_cats; i++)
	{
	    /* Open file for writing */
	    sprintf(filename, "%s_%d", dig_name, i);
	    
	    if (0 > Vect_open_new (&Map, filename))
		G_fatal_error ("Not able to create vector file");

	    printf("\nCreating vector file [%s]", filename);

	    /* Write binary dlg head */
/*
**	    if (write_bdlg_head(bin, i-1) == -1)
**	    {
**		printf("Error in writing binary dlg header\n");
**		exit (-1);
**	    }
*/

	    /* Read and write the main body */
	    if (dlg_to_dig (dlg, &Map) == -1)
	    {
		printf("Error in translating header\n");
		exit (-1);
	    }

	    Vect_close (&Map);
	}
	fclose (dlg);
    }
    exit(0);
}
