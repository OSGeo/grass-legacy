#include "gis.h"
#include <stdio.h>
#include "config.h"

display_all (in)
    FILE *in;
{
    char *tmp_fname;
    FILE *tmp;
    int ok = 0;
    int first=1;
    int stat = 0;

    if (!in)
	return 0;

    rewind(in);
    tmp_fname = G_tempfile();

    chk_status = 0;
    fprintf(stderr," \nExisting labels:    ");
    while (gather(in))
    {
           tmp = fopen (tmp_fname, "w");
           update(tmp);
           fclose(tmp);
           config.count++;
           fprintf(stderr,"\b\b\b%3d",config.count);
           /* get ready to plot label */
           if ((strcmp(config.skip,"YES")==0  ||
                strcmp(config.skip,"yes")==0) &&
                !first) 
                continue; 
           else 
	      show_a_label(tmp_fname);   /* show it  */
    }
    unlink (tmp_fname); 
    fprintf(stderr,"\n");
    if (config.count > 0)
      {
      chk_status = 1;
      return 1;
      }
    else return 0;
}
