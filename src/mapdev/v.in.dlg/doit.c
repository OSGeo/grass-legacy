/*  @(#)a_b_main.c	2.1  6/26/87  */
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

#define MAIN
#include "dlg.h"
#include "l_proto.h"

/*
*	old version is for 1.0 release dlg files
*	new version is for 2.0 release and beyond
*/

/*
fprintf (stdout,"\nUsage: %s old-dlg-ascii-file new-dlg-binary-file\n",
*/
#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
#define         A_DLG           "dlg"
#define         ATT             "dig_att"
#define         PLUS            "dig_plus"

struct dlg dlgstr;

int doit (char *dig_name, char *a_dlg_name,
	char *add_att_name, int off_att, int force_lines)
{
    char *mapset;
    char *mymapset;
    char att_name[300];
    char a_dlgfile[300];
    struct Map_info Map;

    int i;
    FILE *dlg, *att;
    struct dig_head head;
    char filename[300];

    /* Print warning */
    if ((mapset = G_find_file2 (A_DLG, a_dlg_name, "")) == NULL)
    {
	fprintf (stderr, "Ascii DLG file <%s> not found.\n", a_dlg_name);
	exit(-1);
    }
    G__file_name (a_dlgfile, A_DLG, a_dlg_name, mapset);
    mymapset = G_mapset();

    G__file_name (att_name, ATT, dig_name, mymapset);
    G__make_mapset_element(ATT);


    if ( (dlg = fopen(a_dlgfile, "r")) == NULL)
    {
	fprintf (stdout,"Can't find %s\n", "dlg");
	exit(-1);
    }

    /* Read the header section */
    if (read_dlg_head(dlg,&dlgstr) == -1)
	G_fatal_error ("Error reading DLG header");


    /* Check to see if we are going to make multiple files.  Binary
    * files contain only one category per file while "optional" DLG
    * files can contain up to 32 category overlays.
    */

    if (dlgstr.head.num_cats == 1)
    {
	if (0 > Vect_open_new (&Map, dig_name))
	    G_fatal_error ("Not able to create vector file <%s>");

	if (! (att = fopen(att_name, "w")) )
	    G_fatal_error ("Not able to create att file <%s>");

    hd_dlg_to_dig (&dlgstr, &head);

    Vect_copy_head_data (&head, &Map.head);

	/* Read and write the main body */
	if (dlg_to_dig (dlg, &Map, att, add_att_name, off_att, force_lines) == -1)
	{
	    fprintf (stdout,"Error in translating header\n");
	    exit (-1);
	}

	fclose (dlg);
	Vect_close (&Map);
    }
    else
    {
	fprintf (stdout,"This dlg file contains %d overlays.  Each overlay will be\n",
	    dlgstr.head.num_cats);
	fprintf (stdout,"written to a different file\n");

	for (i=1; i<=dlgstr.head.num_cats; i++)
	{
	    /* Open file for writing */
	    sprintf(filename, "%s_%d", dig_name, i);
	    
	    if (0 > Vect_open_new (&Map, filename))
		G_fatal_error ("Not able to create vector file");

	    sprintf(filename, "%s_%d", att_name, i);

            if (! (att = fopen(att_name, "w")) )
	        G_fatal_error ("Not able to create att file <%s>");

	    fprintf (stdout,"\nCreating vector file [%s]", filename);

	    /* Write binary dlg head */
    hd_dlg_to_dig (&dlgstr, &head);

    Vect_copy_head_data (&head, &Map.head);

	    /* Read and write the main body */
	    if (dlg_to_dig (dlg,&Map,att,add_att_name,off_att,force_lines) == -1)
	    {
		fprintf (stdout,"Error in translating header\n");
		exit (-1);
	    }

	    Vect_close (&Map);
	}
	fclose (dlg);
	fclose (att);
    }
    exit(0);
}




