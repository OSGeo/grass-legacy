/*  @(#)imp_main.c	2.2  3/07/91 GRASS4.0  
*   @(#)imp_main.c	2.1  6/26/87  
* Created by: CERL, original is in mapdev/dlg_to_bdlg
* modified:   code for adding categories to dlg files, user
*             asked for attribute name here.
*                               Paul Carlson, SCS, 10-10-87 
* modified:   moved from mapdevdlg_to_bdlg to importdlg routines
*                                     R.Glenn, SCS 12-1-87  
* modified:  input arguements to include an attribute file
*            requirement, needed for batch operations. Removed
*            attribute name request.
*                                     R.Glenn, SCS 12-10-87 
*
* modified:  removed as a stand alone program and merged into
*           the import.to.vect routines.
*                                     R.Glenn, SCS  7-26-88
*
* modified:  changed input arguement handling for 4.0
*                                     R.Glenn, SCS  3-07-91
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.in.dlg.scs [-uf]  dlg=ascii-dlg-name
*                               bdlg=binary-dlg-name
*                               [att=ascii-attr-file-name]
*
*    flags:
*         -u      : dlg contains universe polygon
*         -f      : use attribute file
*
*/

#include <stdio.h>
#define MAIN
#include "dlghead.h"
#include "format.h"
#include "gis.h"

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	int i ;
	char  *ascii ;
	char  *binary ;
	FILE *dlg ;
	FILE *bin ;
	FILE *fopen() ;
	char filename[128] ;
	char  *rindex() ;

	char answer[5], *fileptr, *namptr;
        char *gets(), dlg_in[128], bdlg_out[128];
	int namcnt, add_cats, Univ;
        struct Option *dlgopt, *bdlgopt, *attopt;
	struct Flag *u_flag, *f_flag;
	FILE *cats_fd;

	extern	int	new_format ;

	setbuf(stdout, 0) ;
        G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */
		  
	u_flag = G_define_flag();
	u_flag->key              = 'u';
	u_flag->description      = "Does dlg file contain universe area";
					   
	f_flag = G_define_flag();
	f_flag->key              = 'f';
	f_flag->description      = "Is an attribute file included";
					   
        dlgopt = G_define_option();
        dlgopt->key              = "dlg";
        dlgopt->type             =  TYPE_STRING;
        dlgopt->required         =  NO;
	dlgopt->description      = "ascii dlg file name";

        bdlgopt = G_define_option();
        bdlgopt->key              = "bdlg";
        bdlgopt->type             =  TYPE_STRING;
        bdlgopt->required         =  NO;
	bdlgopt->description      = "binary dlg file name";
					   
        attopt = G_define_option();
        attopt->key              = "att";
        attopt->type             =  TYPE_STRING;
        attopt->required         =  NO;
	attopt->description      = "dlg.att file name";
					   
       /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		exit (-1);
		    
	new_format = 1 ;

		   /* start checking options and flags */

	add_cats = 0;
	cats_fd = NULL;    /* get the attribute file name, if f flag */
	if (f_flag->answer)
	  {		   /* no GRASS category codes in DLG file */
		add_cats = 1;
		sprintf(filename,"%s",attopt->answer);
		cats_fd = fopen(filename, "r");
	  }

	if (u_flag->answer) Univ = 1;
	else Univ = 0;

/* Print warning */
	sprintf(dlg_in,"%s",dlgopt->answer);
	sprintf(bdlg_out,"%s",bdlgopt->answer);
	if (strcmp(dlg_in,"-") == 0)
		dlg = stdin;
	else if ( (dlg = fopen(dlg_in, "r")) == NULL)
	      {
	      fprintf (stdout,"Can't find %s\n", dlg_in) ;
	      exit(-1) ;
	      }
				/* rindex is for ATT System V */
	if ( ! (ascii= rindex(dlg_in, '/')))
		ascii = dlg_in ;
	else
		++ascii ;

	if ( ! (binary= rindex(bdlg_out, '/')))
		binary = bdlg_out ;
	else
		++binary ;


/*	G_clear_screen();

	fprintf (stdout,"\nConverting the ascii import file: %s\n", ascii);
	fprintf (stdout,"     to the %s binary file format: %s\n\n",
		new_format ? "NEW": "OLD" , binary); */

/* Read the header section */
	if (read_dlg_head(dlg) == -1)
	{
		fprintf (stdout,"Error in reading header\n") ;
		exit (-1) ;
	}
/* Check to see if we are going to make multiple files.  Binary
 * files contain only one category per file while "optional" DLG
 * files can contain up to 32 category overlays.
 */

	if (num_cats == 1)
	{
	/* Open file for writing */
		if ( (bin = fopen(bdlg_out, "w")) == NULL)
		{
			fprintf (stdout,"Can't open %s\n", bdlg_out) ;
			exit(-1) ;
		}

	/* Write binary dlg head */
		if (write_bdlg_head(bin, 0) == -1)
		{
			fprintf (stdout,"Error in writing binary dlg header\n") ;
			exit (-1) ;
		}
		
	/* Read and write the main body */
		if (imp_dlg(dlg, bin, cats_fd, Univ) == -1)
		{
			fprintf (stdout,"Error in translating header\n") ;
			exit (-1) ;
		}

		fclose (dlg) ;
		fclose (bin) ;
	}
	else
	{
		fprintf (stdout,"This dlg file contains %d overlays.  Each overlay will be\n",
			num_cats) ;
		fprintf (stdout,"written to a different file\n") ;

		for (i=1; i<=num_cats; i++)
		{
		/* Open file for writing */
			sprintf(filename, "%s_%d", bdlg_out, i) ;
			if ( (bin = fopen(filename, "w")) == NULL)
			{
				fprintf (stdout,"Can't open %s\n", "bin") ;
				exit(-1) ;
			}

			fprintf (stdout,"\nWriting file [%s]", filename) ;

		/* Write binary dlg head */
			if (write_bdlg_head(bin, i-1) == -1)
			{
				fprintf (stdout,"Error in writing binary dlg header\n") ;
				exit (-1) ;
			}
			
		/* Read and write the main body */
			if (imp_dlg(dlg, bin, cats_fd, Univ) == -1)
			{
				fprintf (stdout,"Error in translating header\n") ;
				exit (-1) ;
			}

			fclose (bin) ;
		}
		fclose (dlg) ;
		fclose(cats_fd);
	}
	exit(0);
}
