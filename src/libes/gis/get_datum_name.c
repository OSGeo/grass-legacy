/***********************************************************************
 * GRASS 5.0 gis library
 * get_datum_name.c, get datum name for new location database
 *
 * Andreas Lange, andreas.lange@rhein-main.de
 * version 0.9
 * modified Jul 13 2000 
 *
 ***********************************************************************/

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "CC.h"

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
        for (i=0; dat = CC_datum_name(i); i++) {
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
	      sprintf(buff,"more %s",Tmp_file);
            }
            else
	      sprintf(buff,"cat %s",Tmp_file);
            system(buff);
          }
          else {
            if (strcmp(answer,"datum") == 0) break; 
            if (CC_get_datum_by_name(answer) < 0) {
	      fprintf(stderr,"\ninvalid datum\n");
            }
            else break;
          }
        }
        sprintf(datum,"%s",answer);
        unlink(Tmp_file);
       
        return 1;
}

