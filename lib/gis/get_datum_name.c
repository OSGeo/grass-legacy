/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       GRASS 5.0 gis library, get_datum_name.c
 * AUTHOR(S):    unknown, updated by Andreas Lange, andreas.lange@rhein-main.de
 * PURPOSE:      Get datum name for new location database
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
#include "gis.h"

/***********************************************************************
 * G_ask_datum_name(char *datum)
 *
 * ask interactively for a valid datum name
 *
 * returns -1, 0 on error
 * returns 1 on success
 ***********************************************************************/
int 
G_ask_datum_name(char *datum)
{ 
	char buff[1024],answer[50];
	char  *dat, *Tmp_file;
        FILE  *Tmp_fd = NULL;
	int  i;

        Tmp_file = G_tempfile ();
        if (NULL == (Tmp_fd = fopen (Tmp_file, "w"))) {
	    G_fatal_error("Cannot open temp file") ;
        }
        fprintf(Tmp_fd,"datum\n");
        for (i=0; (dat = G_datum_name(i)); i++) {
          fprintf(Tmp_fd,"%s\n",dat);
        }

        fclose(Tmp_fd);

        for(;;) {
	  do {
	      fprintf(stderr,"\nPlease specify datum name\n");
	      fprintf(stderr,"Enter 'list' for the list of available datums\n");
	      fprintf (stderr, "Hit RETURN to cancel request\n");
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
            if (strcmp(answer,"datum") == 0) break; 
            if (G_get_datum_by_name(answer) < 0) {
	      fprintf(stderr,"\ninvalid datum\n");
            }
            else break;
          }
        }
        sprintf(datum,"%s",answer);
        unlink(Tmp_file);
       
        return 1;
}

