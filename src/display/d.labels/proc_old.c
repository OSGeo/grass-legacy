#include "gis.h"
#include <stdio.h>
#include <math.h>
#include "config.h"

process_old (in, file, upd_file)
    FILE *in;
    char *file, *upd_file;
{
    char *tmp_fname;
    FILE *tmp, *out;
    int ok = 0, make_changes = 0, got_label, label_num, option;
    int mods=0;
    char border[20], color[20], background[20];
    char ch_east[20], ch_north[20];
    double East, North, e_coord, n_coord;
    double dist, dist2;
    double eastsq, northsq;

    if (!in)
	return -1;

    tmp_fname = G_tempfile();
    
    out = fopen (upd_file, "w");
    if (!out)
       {
       perror (upd_file);
       return -1;
       }

    make_changes = 0;
    while (!make_changes)
      {
           /* have user to point to desired label  */
      option = get_location(2,ch_east,ch_north);
      if (option == 1) return(1);
      else if (option != 0) continue;
     
      rewind(in);
      config.count = 0;
      dist = 9999999.0;
      sscanf(ch_east,"%lf",&East);
      sscanf(ch_north,"%lf",&North);
         /* look through existing labels, find the closest */
      while (gather(in))
        {
        sscanf(config.east,"%lf",&e_coord);
        sscanf(config.north,"%lf",&n_coord);
        eastsq = (East - e_coord) * (East - e_coord);
        northsq = (North - n_coord) * (North - n_coord); 
        dist2 = sqrt(eastsq + northsq);

                     /* compare to coordinates, find closest */ 
        if (dist > dist2) 
           {
           dist = dist2;
           label_num = config.count + 1;
           strcpy(border,config.border);
           if (!strcmp(config.border,"black"))  strcpy(config.border,"red");
           else if (!strcmp(config.border,"red"))
                                                strcpy(config.border,"yellow");
           else if (!strcmp(config.border,"yellow"))
                                                strcpy(config.border,"green");
           else if (!strcmp(config.border,"blue")) 
                                                strcpy(config.border,"violet");
           else if (!strcmp(config.border,"green")) 
                                                strcpy(config.border,"cyan");
           else if (!strcmp(config.border,"orange"))
                                                strcpy(config.border,"brown");
           else if (!strcmp(config.border,"cyan")) 
                                                strcpy(config.border,"magenta");
           else strcpy(config.border,"white");

           tmp = fopen(tmp_fname,"w");   /* re-open temp file */ 
           update(tmp);                  /* write label info to it */ 
           fclose(tmp);
           }
        config.count++;
        }  /* end of while gather(in) */

                    /* plot label for users approval */
      got_label = show_a_label(tmp_fname);
      rewind (in);
      config.count = 0;
      if (!got_label)
        {
        while (gather(in))
            {
            if (config.count+1 == label_num)
               {
               strcpy(config.border, border);
               tmp = fopen(tmp_fname,"w");   /* re-open temp file */ 
               update(tmp);                  /* write label info to it */ 
               fclose(tmp);
               chk_status = 0;
               show_a_label(tmp_fname);
               chk_status = 1;
               }    /* end of count = label_num */
           config.count++;
           }    /* end while gather */
        }
      else
        {
        while (gather(in))
            {
            if (config.count+1 == label_num)
               {
               ok = 0;
               while (!ok)
                 {
                 tmp = fopen(tmp_fname,"w");       /* open temp file */
                 update(tmp);                      /* write label info to it */
                 fclose(tmp);
                 strcpy (reset_loc,"no");           /* not ok */
                 setup (file,0);
	         if(!modify())             
	           {
                   mods = 0;
                   ok = 1;
                   continue;
                   }
                 ok = check_responses();
                 if (ok)
                   {
                   if (!strcmp("Y",reset_loc)   || !strcmp("yes",reset_loc) ||
                     !strcmp("YES",reset_loc) || !strcmp("y",reset_loc) )
                      {
                      /* allow user to choose new location for label */
                            /* clear the old label first */
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
                             /* get the new location */
                      while(1)
                        {
                        get_location(3,config.east,config.north);
                             /* check for within current window */
                        if (get_location(4,config.east,config.north)) 
                                                                 break;
                        }
                             /* restore original colors */
                      strcpy (config.color,color);
                      strcpy (config.background,background);
                      strcpy (config.border,border);
                      ok = 0;
                      }
                   }
                 tmp = fopen(tmp_fname,"w");   /* re-open temp file */ 
                 update(tmp);                  /* write label info to it */ 
                 fclose(tmp);
                    /* plot label for users approval */
                 ok = show_a_label(tmp_fname);
                 if (ok) mods = 1;
                 }   /* end while !ok */
               }    /* end of count = label_num */
           config.count++;
           update(out);
           }    /* end while gather */
        if (mods) 
           {
           fclose (out);
           sav_file(upd_file, file);
           mods = 0;
           out = fopen (upd_file, "w");
           }
       }   /* end of got_label */
    }    /* end of make_changes */
    fclose(out);
    return 1;
}
