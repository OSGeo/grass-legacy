
#include     <stdio.h>
#include     <signal.h>
#include     "sdts_in.h"
#include     "gis.h"
#include     "Vect.h"
#include     "stc123.h"
#include  "sdts_globals.h"


int do_NE_module(cur_mfold, S_globals, bounds, Map, Points, Obj_num)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
    struct Bounds *bounds;
	struct Map_info *Map;
	struct line_pnts *Points;
	int *Obj_num;
{
   FILE *fpin;
   char ice[2], ccs[4];
   char filename[100];
   char leadid;
   char tag[10];
   char descr[5000];
   char frmts[500];
   char string[5000];
   int status;
   long bytlen;
   long int_level;

   struct Sdts_NE NE;
   long rec_num = 0;
   int coord_x, coord_y;
   int att_flag;
   char modn[10];
   char obj_code[20];
   char atid_prefix[10];
   char atid_label[20];
   int first_rec;
   int atid_count;



   if (cur_mfold->mod_name[NExx] == 0 || cur_mfold->mod_nrec[NExx] == 0)
   {
	 fprintf (stderr, "Warning: No Entity Points (NE) Module for this map layer.\n");
     return (1);
   }
   strcpy (modn, cur_mfold->mod_name[NExx]);
   strcpy (filename, cur_mfold->file_name[NExx]);  

   fprintf (stderr, "Processing NE Module: %s ", filename);

   
    if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
	   return (0);
   
   /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
   if (!read_dd_rec (filename, fpin, string, &status)) 
   {
	   end123file (&fpin);
	   return (0);
   }

   
   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;
   first_rec = 1;
   att_flag = 0;
   atid_count = 0;

/*===================DATA SECTION========================*/

   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

	   /* CALL RD123SFLD() TO READ SUBFIELD */
	   if(!read_data_sfld (filename, fpin,tag,&leadid,string,&bytlen,&status)) 
	   {
		   end123file (&fpin);
		   return (0);
       }

       if (!check_data_sfld (filename, fpin, tag, descr, frmts))
	   {
		   end123file (&fpin);
		   return (0);
       }
		  /*DEBUG*/
		  /*
          fprintf (stderr, "%s%ld\n",    "Record Number     = ",  rec_num);
          fprintf(stderr,"%s%s%s%s\n",   "* TAG & DESC      = ",tag, " | ", descr );
		  */

       if (strcmp (tag, "0001") == 0) {
            rec_num++;    
			(*Obj_num)++;
			NE.att_num = *Obj_num;

#ifdef TEST_VER
				/*DEBUG*/ 
				if (rec_num % 50 == 0)
				  goto end;
#endif /*TEST_VER*/

			if (first_rec)
				first_rec = 0;
            else
			{
               if (rec_num % 100 == 0)
                  fprintf(stderr,". ",  rec_num);
				       /*write out previous record here*/
				  dig_write_point (Map, Points,  &(NE.x), &(NE.y), DOT); 

				  cur_mfold->n_entity_pnts++;

				  if (att_flag) /*write out the attribute*/
				  {
			            build_attr_structs_from_tmp_list 
						   (obj_code, DOT, atid_count, NE.x, NE.y);
						att_flag = 0;
						atid_count = 0;
						cur_mfold->n_attr_objs++;
				  }
				  else
					if (find_objs_in_ff_elem_list (obj_code))
					{
					/*write out attribute*/
					    build__attr_structs (obj_code, (char *) NULL,  DOT, NE.x, NE.y);
						att_flag = 0;
						atid_count = 0;
						cur_mfold->n_attr_objs++;
					}
             }
       }
	   else if (strcmp (tag, "PNTS") == 0) 
	   {
		  if (strcmp (descr, "RCID") == 0)
		  {
			 NE.rcid = atoi (string);
			 sprintf (obj_code, "%s#%s", modn, G_squeeze (string));
		  }
	   }
	   else if (strcmp (tag, "LINE") == 0)
	   {
	   }
	   else if (strcmp (tag, "ATID") == 0)
	   {
		  att_flag = 1;
		  if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
			 strcpy (atid_prefix, G_squeeze (string));
		  else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
		  {
			 atid_count++;
			 sprintf (atid_label, "%s#%s", atid_prefix, G_squeeze (string));
			 add_atid_to_tmp_list (atid_label, atid_count);
		  }
	   }
	   else if (strcmp (tag, "ARID") == 0)
	   {
	   }
	   else if (strcmp (tag, "SADR") == 0)
	   {

		  if ((strcmp (descr, "X") == 0) || (strcmp (descr, "!X") == 0))
		  {
		     coord_x = conv_bits (string);
			 get_min_max ( 1, coord_x, bounds);
			 NE.x = S_globals->Iref_sfax * (double) coord_x;
		  }
		  else if ((strcmp (descr, "Y") == 0) || (strcmp (descr, "!Y") == 0))
		  {
		     coord_y = conv_bits (string);
			 get_min_max ( 2, coord_y, bounds);
			 NE.y = S_globals->Iref_sfay * (double) coord_y;
		  }
	   }
	   else
	   {
           fprintf (stderr, "\n%s %s\n", "Anomoly = ", tag);
	   }

   }
   
	/*write last record*/
	  /*write out previous record here*/
   dig_write_point (Map, Points,  &(NE.x), &(NE.y), DOT); 

   cur_mfold->n_entity_pnts++;

   if (att_flag) /*write out the attribute*/
   {
		build_attr_structs_from_tmp_list
		   (obj_code, DOT, atid_count, NE.x, NE.y);
		att_flag = 0;
		cur_mfold->n_attr_objs++;
    }
    else
      if (find_objs_in_ff_elem_list (obj_code))
	  {
		/*write out attribute*/
			build__attr_structs (obj_code, (char *) NULL,  DOT, NE.x, NE.y);
			att_flag = 0;
			atid_count = 0;
			cur_mfold->n_attr_objs++;
	  }

	/*DEBUG*/ 
	/*fprintf (stderr, "min x %ld \n", bounds->min_x);
	  fprintf (stderr, "max x %ld \n", bounds->max_x);
	  fprintf (stderr, "min y %ld \n", bounds->min_y);
	  fprintf (stderr, "max y %ld \n", bounds->max_y);
	*/

end:

   /* CALL END123FILE() FOR INPUT FILE */
   if (!end_sdts_rfile (filename, &fpin))
      return (0);
   
   
   fprintf (stderr, "\n");

   return (1);
}

