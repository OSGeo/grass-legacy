#include <stdio.h>
#include "stc123.h"
#include "defines.h"
#include "externs.h"

#define MAX_FILENAME 11
#define BUF_MAX 500
char buf[BUF_MAX+2], rec_buf[2*BUF_MAX];

int
write_quality_mod (sdts_prefix,  sdts_path, mod_id, current_layer)
  char *sdts_prefix;
  char *sdts_path;
  int mod_id;
  char *current_layer;
{
    FILE *fp_dq_src;
    FILE *fp_dq;
    char dq_filename[200];
    char out_fname [100];
    char err_mess[200];
    char *mod_name = Mod[mod_id].name;
    char cur_dq_file [30];
    int cnt, rec_num;
    char *p, *q;
  
/*intialize record count for the module*/
    Mod[mod_id].rec_cnt = 0;

  /* check if the data quality text file is available.  If not, print out
     warning message and exit.*/
  sprintf (dq_filename, "%s/%s/dig_misc/%s/sdts_meta/", G_location_path(),
	   G_mapset(), current_layer);
  switch (mod_name[2])
    {
    case 'H' :	/* DQHL, lineage module */
      strcat (dq_filename, "HL_");
	  strcpy (cur_dq_file, "Lineage (HL)");
      break;

    case 'P' :	/* DQPA, position accuracy module */
      strcat (dq_filename, "PA_");
	  strcpy (cur_dq_file, "Positional Accuracy (PA)");
      break;

    case 'A' :	/* DQAA, attribute accuracy module */
      strcat (dq_filename, "AA_");
	  strcpy (cur_dq_file, "Attribute Accuracy (AA)");
      break;

    case 'L' : 	/* DQLC, logical consistency module */
      strcat (dq_filename, "LC_");
	  strcpy (cur_dq_file, "Logical Consistency (LC)");
      break;

    case 'C' :	/* DQCG, completeness module */
      strcat (dq_filename, "CG_");
	  strcpy (cur_dq_file, "Completeness (CG)");
      break;

    default :	/* It should never be reached */
	  put_err_mess ("Internal error: Illegal data quality module name.", 0);
      return (0);
	  break;
    }
  
  if ((int)strlen (current_layer) > MAX_FILENAME)
    current_layer [MAX_FILENAME] = 0;

  strcat (dq_filename, current_layer);
  if ((fp_dq_src = fopen (dq_filename, "r")) == NULL)
    {
      sprintf (err_mess, "Failure : %s data quality source file is missing.\n", cur_dq_file);
	  put_err_mess (err_mess, 0);
      return (0);
    }

  sprintf (out_fname, "%4s%4s.DDF", sdts_prefix, mod_name);
  if (!Open_sdtsfile (sdts_path, out_fname, &fp_dq, 'W'))
      return (0);

  if (!begin_sdts_ddr (out_fname, fp_dq))
  {
	   end123file (&fp_dq);
	   return (0);
  }
  sprintf (buf, "1600;&%s", Mod[mod_id].type);

  if (!write_dd_fld (out_fname, fp_dq, mod_name, buf,"MODN!RCID!COMT", "(A, I, A)", 3))
  {
	   end123file (&fp_dq);
	   return (0);
  }
  if (!end_dd_rec (out_fname, fp_dq))
  {
	   end123file (&fp_dq);
	   return (0);
  }
 
  /* Read DQ data from fp_dq_src, and put each paragraph into separate
     DQ records */

     rec_num = 1;

     sprintf (rec_buf, "%4s%c%6d%c", mod_name, FT_EXPORT, rec_num, FT_EXPORT);

     while ((cnt = fread (buf, 1, BUF_MAX, fp_dq_src)) != NULL)
     {
        short cr_flag = 0;
	    short new_paragraph = 0;

        buf[cnt] = 0;
        for (p = q = buf; p<&buf[cnt] ; p++)
	    {
	       if ((cr_flag && *(p) == '\t') ||
	         (cr_flag && *(p) == '\n') || (cr_flag >1 && *(p) == ' '))	

	       /* The end of a paragraph */
           {
	           char tmp_buf [10];

	           cr_flag = 0;
		       new_paragraph = 1;
	       /* Get a complete DQ record, write it out. */

	          if (!begin_data_rec (out_fname, fp_dq))
		      {
			   	  end123file (&fp_dq);
				  return (0);
		      }

	          sprintf (tmp_buf, "%6d", rec_num);

	          write_data_fld (out_fname, fp_dq, DDF_ID, LEAD_ID, tmp_buf, strlen (tmp_buf), 2);
/*
	          *(p-1) = FT_EXPORT;
*/
	          *p = 0;
	          strcat (rec_buf, q);
	          if (cnt < BUF_MAX && p >= &buf[cnt-1])
		         /* this record is the last one in the module */
		        write_data_fld (out_fname, fp_dq, mod_name, LEAD_ID, rec_buf, strlen (rec_buf),4);
	         else
		        write_data_fld (out_fname, fp_dq, mod_name, LEAD_ID, rec_buf, strlen (rec_buf), 3);

             if (get_err_stat() != 1)
             {
                 end123file (&fp_dq);
                 Mod[mod_id].rec_cnt = 0;
                 return (0);
             }

             if (!end_data_rec (out_fname, fp_dq))
             {
                 end123file (&fp_dq);
                 Mod[mod_id].rec_cnt = 0;
                 return (0);
             }

	         q = ++p;
	         rec_num ++;
	         sprintf (rec_buf, "%4s%c%6d%c", mod_name, FT_EXPORT, rec_num, FT_EXPORT);
           }

	       /*strip extra CRs, spaces, tabs at beginnings of new paragraphs*/
	       if (new_paragraph)
	       {
		      if ( *p == ' '  || *p == '\t' || *p == '\n' )
			     q = ++p;
              else
		         new_paragraph = 0;
           }
           else
	       {
	          if (cr_flag)
	          {
	            if ( *p == ' '  || *p == '\t' || *p == '\n' )
		           cr_flag ++;
	            else
		           cr_flag = 0;
	          }
	          else
	            if (*p == '\n')
	            {
		          cr_flag = 1;
	            }
	       }

	    }
        /* Put the remaining chars in buf into rec_buf */
        if (q<p)
	      strcat (rec_buf, q);
	
     }

    /* Write out the possible last record */
     if (q<p)
     {
         char tmp_buf [10];
         int  len;
     
         len = strlen (rec_buf);
/*
      rec_buf[len] = FT_EXPORT; rec_buf[len + 1] = 0;
      len ++; 
*/
 	     if (!begin_data_rec (out_fname, fp_dq))
	     {
		    end123file (&fp_dq);
            Mod[mod_id].rec_cnt = 0;
	   	    return (0);
	     }
         sprintf (tmp_buf, "%6d", rec_num);

         write_data_fld (out_fname, fp_dq, DDF_ID, LEAD_ID, tmp_buf, strlen (tmp_buf), 2);
         write_data_fld (out_fname, fp_dq, mod_name, LEAD_ID, rec_buf, len , 4);

         if ( (get_err_stat() != 1) || (!end_data_rec (out_fname, fp_dq)) )
	     {
             Mod[mod_id].rec_cnt = 0;
	         end123file (&fp_dq);
	         return 0;
	     }
      }

      Mod[mod_id].rec_cnt = rec_num;

      if (end_sdtsfile (out_fname, &fp_dq))
      {
          Mod[mod_id].rec_cnt = rec_num;
          return (1);
      } 
      else
      {
          Mod[mod_id].rec_cnt = 0;
          return (0);
      } 
}


