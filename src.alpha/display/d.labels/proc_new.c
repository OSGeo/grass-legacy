#include "gis.h"
#include "config.h"
#include <stdio.h>

process_new (in, file, upd_file)
    FILE *in;
    char *file, *upd_file;
{
    char *tmp_fname;
    FILE *tmp, *out;
    char response[5];
    char border[20], color[20], background[20];
    int i, ok=0, mods=0;

    out = fopen (upd_file, "w");
    if (!out)
       {
       perror (upd_file);
       return(-1);
       }

    if (in)
       {
       rewind (in);
       while (gather(in))
          {
          update(out);
          config.count++;
          }
       }

    tmp_fname = G_tempfile();
    while (1)
    {
	/* initialize variables */
        for (i = 0; i < TEXTLINES; i++)
	    config.text[i][0] = 0;
	config.xoffset[0] = config.yoffset[0] = 0;
	strcpy (config.skip,"no");

        /* ask user to indicate location of next label */
	if (get_location(3,config.east,config.north))
           {      /* QUIT */
           if (mods > 0) 
             {
             fclose (out);
             return(1);
             }
           return(0);
           }
        /* check for within current window */
        if (!get_location(4,config.east,config.north)) continue;

        /* loop until user says a label looks ok or quits */
        ok = 0;
        while (!ok)
           {
           /* setup VASK screen with label parameters for this label */
	   strcpy (reset_loc,"no");
           setup (file,1);

           /* present the VASK screen */
	   if(!modify())
               {
               fclose (out);
               if (mods > 0) return(1);
	       return(0);
               }
           ok = check_responses();
           if (ok)
              {
              if (!strcmp("N",reset_loc)  || !strcmp("no",reset_loc) ||
                  !strcmp("NO",reset_loc) || !strcmp("n",reset_loc) )
                 {
                 tmp = fopen(tmp_fname,"w");   /* open temp file */ 
                 update(tmp);                  /* write label info to it */ 
                 fclose(tmp);
                 if(strcmp(config.skip,"YES")==0||strcmp(config.skip,"yes")==0)
                    ok=1;
                 else
                    {
                    /* plot label for users approval */
                    ok = show_a_label(tmp_fname);
                    if (!ok)  /* clear label from the screen */
                      {
                      strcpy (color,config.color);
                      strcpy (background,config.background);
                      strcpy (border,config.border);
                      strcpy (config.color,"black");
                      strcpy (config.background,"black");
                      strcpy (config.border,"black");
                      tmp = fopen(tmp_fname,"w");  /* open temp file */ 
                      update(tmp);                /* write label info to it */ 
                      fclose(tmp);
                      chk_status = 0;
                      show_a_label(tmp_fname);
                      chk_status = 1;
                      strcpy (config.color,color);
                      strcpy (config.background,background);
                      strcpy (config.border,border);
		      ok = 1;
                      }
                    else
                      {   /* label is acceptable */
                      config.count++;
                      /* update the label file */
                      update(out);
                      mods++;
                      }
                    }   /* end user approval */
                 unlink (tmp_fname); 
                 }
              }   /* end of if ok  */
           }  /* end while !ok */
    }   /* end while 1 */
}
