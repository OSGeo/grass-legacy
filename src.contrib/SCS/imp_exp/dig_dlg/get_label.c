/* @(#)get_label.c	1.0   06/27/90 */
/* Created by: RL Glenn, SCS, CGIS Division
*/
#include "digit.h"

get_label (labl, num, fc)
    char *labl;
    int num;
    FILE *fc;
{
    int icode, recd, cat_cnt, cnt;
    char buffer[82], cat_name[71],dum_name[60];
    char *ptr, *code, *label;

/* find input code in category file, assign category value to the
		    area_number based on category file code number*/

    rewind (fc);
				/*  get current category count */
    fgets (buffer, 72, fc);
    sscanf(buffer,"%*s%d%*s",&cat_cnt);
                             /*  read past next four records */
    for (recd=0;recd<=3;++recd)
       fgets (buffer, 72, fc);

    for (recd=0;;++recd)
       {		 /* category file search */
       if (!fgets (buffer, 82, fc)) break;

       cnt = 0;
       ptr = code = buffer;
       while (*ptr && *ptr != 0) 
	  {
	  *ptr++;
	  if (*ptr != ':') continue;
	  else
	     {    /* get first label field only */
	     *ptr = 0;
	     *ptr++;
	     sprintf(cat_name,"%s",ptr);
  	     label = cat_name; 
		/* remove the line-feed  or
		   end at ':' (SCS multi-field delimiter) */
	     for (ptr = label; *ptr; ptr++)
	        if (*ptr == '\n' || *ptr == '\072')
	            *ptr = 0;
 	     break;
	     }
	  }
       sscanf (code,"%d",&icode);

	/* compare for match */
       if (num == icode)
          {       /* match */
  	  strcpy(labl,label);
	  return(1);
	  }
       }         /* end of category search */

    return(0) ;
}

