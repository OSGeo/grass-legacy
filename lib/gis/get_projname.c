/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       GRASS 5 gis library, get_projname.c
 * AUTHOR(S):    unknown
 * PURPOSE:      Get projection name from user
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
#include <grass/gis.h>
#include <grass/glocale.h>

int G_ask_proj_name (char *proj_id, char *proj_name)

{
	char path[1024], buff[1024], answer[50], *a;
        struct Key_Value *in_proj_keys;
	char  *Tmp_file;
        FILE  *Tmp_fd = NULL;
	int   in_stat, i, npr;

        sprintf(path,"%s/etc/projections",G_gisbase());
        while (access(path,0) !=0)
        { 
          sprintf(buff,_("%s not found"),path);
          G_fatal_error(buff);
        }
        in_proj_keys = G_read_key_value_file(path,&in_stat);
        if (in_stat != 0)
        {
          sprintf(buff,_("ERROR in reading %s"),path);
          G_fatal_error(buff);
        }
        npr = in_proj_keys->nitems;
        Tmp_file = G_tempfile ();
        if (NULL == (Tmp_fd = fopen (Tmp_file, "w"))) {
	  G_fatal_error(_("Cannot open temp file")) ;
        }
        for (i=0; i<npr; i++) {
          fprintf(Tmp_fd,"%s -- %s\n",in_proj_keys->key[i],in_proj_keys->value[i]);
        }
        fclose(Tmp_fd);

        for(;;) {

	  do {
	      fprintf(stderr,_("\n\nPlease specify projection name\n"));
	      fprintf(stderr,_("Enter 'list' for the list of available projections\n"));
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
            a = G_find_key_value(answer,in_proj_keys);
            if (a==NULL) 
	    {
	      fprintf(stderr,_("\ninvalid projection\n"));
	    }
            else break;
          }
        }

        sprintf(proj_id,"%s",answer);
        sprintf(proj_name,"%s",a);
        remove ( Tmp_file );
        return 1;
      }

