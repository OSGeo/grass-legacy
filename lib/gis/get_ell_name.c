/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       GRASS 5 gis library, get_ell_name.c
 * AUTHOR(S):    unknown, updated by Andreas Lange, andreas.lange@rhein-main.de
 * PURPOSE:      Get ellipse name from user
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include  "gis.h"
#include  "glocale.h"

int G_ask_ellipse_name( char *spheriod)
{ 
	char buff[1024],answer[50];
        double aa,e2;
	char  *sph, *Tmp_file;
        FILE  *Tmp_fd = NULL;
	int  i;

        Tmp_file = G_tempfile ();
        if (NULL == (Tmp_fd = fopen (Tmp_file, "w"))) {
	    G_fatal_error(_("Cannot open temp file")) ;
        }
        fprintf(Tmp_fd,"sphere\n");
        for (i=0; (sph = G_ellipsoid_name(i)); i++) {
          fprintf(Tmp_fd,"%s\n",sph);
        }

        fclose(Tmp_fd);

        for(;;) {
	  do {
	      fprintf(stderr,_("\nPlease specify ellipsoid name\n"));
	      fprintf(stderr,_("Enter 'list' for the list of available ellipsoids\n"));
	      fprintf (stderr, _("Hit RETURN to cancel request\n"));
	      fprintf(stderr,">");
          } while(!G_gets(answer));
          G_strip(answer); 
          if(strlen(answer)==0) return -1;
          if (strcmp(answer,"list") == 0) {
            if (isatty(1)) {
	      sprintf(buff,"$GRASS_PAGER %s",Tmp_file);
            }
            else
	      sprintf(buff,"cat %s",Tmp_file);
            system(buff);
          }
          else {
            if (strcmp(answer,"sphere") == 0) break; 
            if (G_get_ellipsoid_by_name(answer,&aa,&e2) == 0) {
	      fprintf(stderr,_("\ninvalid ellipsoid\n"));
            }
            else break;
          }
        }
        sprintf(spheriod,"%s",answer);
#ifdef __MINGW32__
        remove ( Tmp_file );
#else        
        unlink(Tmp_file);
#endif        
        if (strcmp(spheriod,"sphere") == 0) {
          return 2;
        }
        return 1;
}
