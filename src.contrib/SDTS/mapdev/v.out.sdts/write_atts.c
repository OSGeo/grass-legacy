#include "gis.h"
#include "Vect.h"
#include "defines.h"
#include "externs.h"
#include "stc123.h"
#include "globals2.h"



/***

AP01: General Attribute Module

    field  subfield  len  type       description
--------------------------------------------------------------
    ATPR   Module Name4    A     module name: AP01
    ATPR   Record ID  6    I     record id (1 to map->n_atts)

    ATTP   ATTRIB     6    I     dig_att value (map->Att[att].cat)    
    ATTP   LABEL      --   I     dig_cats value (returned by 
                                    G_get_cat (map->Att[att].cat,...)   

***/

write_SDTS_atts (map, full_fname, fname, ll_input, 
				 zone, vector_name, mapset)
    struct Map_info *map;
    char *full_fname, *fname;
    int ll_input, zone;
	char *vector_name, *mapset;
{
    FILE *fpout;
    P_ATT *Att;
	register int att;
    char tmp_str[5000];
	struct Categories cats;
	int cat_stat;
	char *cat_label;


   fprintf (stderr, "Writing AP01 Module\n");
   /*read cat labels, if they exist*/
	cat_stat = get_cats_struct (vector_name, mapset, &cats); 

    if (!open_sdtsfile (full_fname, &fpout, 'W'))
         return (0);

	if (!begin_sdts_ddr (fname, fpout ))
	{
		end123file (&fpout);
		return (0);
	}

    write_dd_fld (fname, fpout, "ATPR", "1600;&ATTRIBUTE PRIMARY","MODN!RCID", "(A,I)", 6)
;
    write_dd_fld (fname, fpout, "ATTP", "1600;&PRIMARY ATTRIBUTES","ATTR_NUM!ATTR_LABEL", "(I,A)", 3);

	if ((get_err_stat()) != 1)
	{
		end_dd_rec (fname, fpout);
		end123file (&fpout);
		return (0);
	}

    if (!end_dd_rec(fname, fpout))
    {
		end123file (&fpout);
		return (0);
    }


	for (att = 1; att <= map->n_atts; att++)
	{
		Att = &(map->Att[att]);

       if (Aline_only && Att->type == LINE && map->Line[Att->index].type == LINE)
		  {
		     /*if this is the last record, must backup and re-write 
			 prev rec with EOF option*/
              if (att == map->n_atts && Mod[AP01].rec_cnt > 0)
			  {
				 int bkstat;

                 if (!bak123fld (fpout, &bkstat)) 
				 {
				     put_err_mess ("Failure backing up and writing last record.", 0);
		             Mod[AP01].rec_cnt = 0;
					 end123file (&fpout);
                     return (0);
                 }

		          if (!write_data_fld (fname,  fpout, "ATTP", LEAD_ID, tmp_str, (long) strlen (tmp_str), 4))
				  {
		             Mod[AP01].rec_cnt = 0;
					 end123file (&fpout);
                     return (0);
				  }
                  if (!end_data_rec (fname, fpout))
				  {
		             Mod[AP01].rec_cnt = 0;
					 end123file (&fpout);
                     return (0);
				  }
                  break;
			  }
			  else
		         continue;
		  }


		if (!begin_data_rec (fname, fpout))
	    {
		   Mod[AP01].rec_cnt = 0;
		   end123file (&fpout);
		   return (0);
	    }

		sprintf (tmp_str, "%6d", att);
		write_data_fld (fname, fpout, DDF_ID, LEAD_ID, tmp_str, RCID_LEN, 2);

        sprintf (tmp_str, "%4s%s%6d", "AP01",  UT_STR, att);
	    write_data_fld (fname, fpout, "ATPR", LEAD_ID, tmp_str, (long) strlen (tmp_str), 6);

	   /*if there is a category label file*/
	   if (cat_stat >= 0)
	   {
		   /*get cat_label*/
		   cat_label = G_get_cat (Att->cat, &cats);
		   sprintf (tmp_str, "%6d%s%s", Att->cat, UT_STR, cat_label);
	   }
	   else
	   {
		   /*put blanks in place of label in output str*/
		   sprintf (tmp_str, "%6d", Att->cat);
		   /*
		   sprintf (tmp_str, "%6d%s", Att->cat, " ");
		   */
	   }

	   /*
	   sprintf (tmp_str, "%6d%s%s%s", Att->cat, UT_STR, cat_label, FT_STR);
	   */

       if (att == map->n_atts)
		   write_data_fld (fname, fpout, "ATTP", LEAD_ID, tmp_str, (long) strlen (tmp_str), 4);
	   else
	       write_data_fld (fname, fpout, "ATTP", LEAD_ID, tmp_str, (long) strlen (tmp_str), 3);


/*end DD records*/
		Mod[AP01].rec_cnt++;

        if ( (get_err_stat() != 1) || (!end_data_rec (fname, fpout)))
		{
		    Mod[AP01].rec_cnt = 0;
            end123file (&fpout);
			return (0);
        }

#ifdef TEST_VER
    if (Mod[AP01].rec_cnt == 50)
	{
		 end123file (&fpout);
		 return(1);
	}
#endif
	}
	if (!end_sdtsfile (fname, &fpout))
	{
	   Mod[AP01].rec_cnt = 0;
	   return (0);
	}
	else
	   return (1);
}


/*calls G_read_vector_cats to fill cats structure, which will later be
  read by G_get_cat
*/
get_cats_struct (vector_name, vector_mapset, cats)
   char *vector_name, *vector_mapset; 
   struct Categories *cats;
{
	int stat;

    G_suppress_warnings (1);
    stat = G_read_vector_cats (vector_name, vector_mapset, cats);
    G_suppress_warnings (0);
	return (stat);
}
