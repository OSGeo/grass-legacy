/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       g.proj 
 * AUTHOR(S):    Paul Kelly - paul-grass@stjohnspoint.co.uk
 * PURPOSE:      Provides a means of reporting the contents of GRASS
 *               projection information files. Will be extended to handle
 *               modification of and creation of new projection information 
 *               files.
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "config.h"
#include <stdio.h>
#include "gis.h"
#include "gprojects.h"

int main(int argc, char *argv[])
{
    struct Flag *printinfo,      /* Print contents of PROJ_INFO & PROJ_UNITS */
             *printproj4,        /* Print projection in PROJ.4 format   */
#ifdef HAVE_OGR     
             *printwkt,          /* Print projection in WKT format      */
             *esristyle,         /* Use ESRI-style WKT format           */
#endif   
             *dontprettify;      /* Print 'flat' output (no linebreaks) */
   
    struct Key_Value *projinfo, *projunits;
    struct Cell_head cellhd;
   
    G_gisinit(argv[0]);
   
    printinfo = G_define_flag();
    printinfo->key = 'p';
    printinfo->description = "Print projection information for current location";
   
    printproj4 = G_define_flag();
    printproj4->key = 'j';
    printproj4->description = "Print projection information for current "
                              "location in PROJ.4 format";
   
#ifdef HAVE_OGR   
    printwkt = G_define_flag();
    printwkt->key = 'w';
    printwkt->description = "Print projection information for current location "
                            "in WKT ('.prj') format";
   
    esristyle = G_define_flag();
    esristyle->key = 'e';
    esristyle->description = "Use ESRI-style format (applies to WKT output only)";
#endif
   
    dontprettify = G_define_flag();
    dontprettify->key = 'f';
    dontprettify->description = "Print 'flat' output with no linebreaks";
   
    if (G_parser(argc, argv))
        exit(-1);

    G_get_default_window(&cellhd);
   
    projinfo = G_get_projinfo();
    projunits = G_get_projunits();
   
    if( cellhd.proj != 0 && (projinfo == NULL || projunits == NULL) )
        G_warning("Projection files missing");
   
    if(printinfo->answer)
    {
        int i;
       
        fprintf(stdout, "-PROJ_INFO-------------------------------------------------\n");
        for (i=0; i < projinfo->nitems; i++)	
            fprintf (stdout, "%-11s: %s\n",
		     projinfo->key[i], projinfo->value[i]);

        fprintf(stdout, "-PROJ_UNITS------------------------------------------------\n");
        for (i=0; i < projunits->nitems; i++)	
            fprintf (stdout, "%-11s: %s\n",
		     projunits->key[i], projunits->value[i]);       
    }

#ifdef HAVE_OGR   
    if(printwkt->answer)
    {
        char *wktstring;
       
        wktstring = GPJ_grass_to_wkt(projinfo, projunits, esristyle->answer,
				     !(dontprettify->answer));
        fprintf(stdout, "%s\n", wktstring);       
        G_free(wktstring);       
    }	
#endif   
	   
    if(printproj4->answer)
    {
        struct pj_info pjinfo;
        char *projstring, *i;
       
        pj_get_kv(&pjinfo, projinfo, projunits);
        projstring = pj_get_def(pjinfo.pj, 0);
        pj_free(pjinfo.pj);

        for(i=projstring; *i; i++)
	{
	    /* Don't print the first space */
	    if(i == projstring && *i == ' ')
	        continue;
	   
            if(*i == ' ' && !(dontprettify->answer))
	        fputc('\n', stdout);
	    else
	        fputc(*i, stdout);
	}       
        fputc('\n', stdout);
        G_free(projstring);
    }

    if( projinfo != NULL )
        G_free_key_value(projinfo);
    if( projunits != NULL )
        G_free_key_value(projunits);
    return 0;
  
}
