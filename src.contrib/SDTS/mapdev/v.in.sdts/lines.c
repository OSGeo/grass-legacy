
#include     <stdio.h>
#include     <signal.h>
#include     "sdts_in.h"
#include     "gis.h"
#include     "Vect.h"
#include     "stc123.h"
#include  "sdts_globals.h"

#define  COOR_MAX       5000


int do_LE_module(cur_mfold, S_globals, bounds, Map, Points)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
    struct Bounds *bounds;
    struct Map_info *Map;
    struct line_pnts *Points;
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

   struct Sdts_LE LE;
   long rec_num = 0;
   int coord_x, coord_y ;
   int att_flag;
   char modn[10];
   char obj_code[20];
   char atid_prefix[10];
   char atid_code[20];
   int first_rec;
   int coord_cnt, atid_count;
   double att_x, att_y;
   
   if (cur_mfold->mod_nrec[LExx] == 0)
   {
	  put_err_mess ("Required Module (LE) not found.", 0);
      return 0;
   }

   if (noNA_flag)
     init_areapoint ();
   strcpy (modn, cur_mfold->mod_name[LExx]);
   /*
   strcpy (filename, G_tolcase (cur_mfold->file_name[LExx]));  
   */
   strcpy (filename, cur_mfold->file_name[LExx]);  

   fprintf (stderr, "Processing LE module: %s ", filename);

    if (!(LE.X = (double *) malloc(sizeof(double)*COOR_MAX))) {
       fprintf(stderr,"Cannot allocate LE.X array\n");
 	   exit (0);
    }
    if (!(LE.Y = (double *) malloc(sizeof(double)*COOR_MAX))) {
       fprintf(stderr,"Cannot allocate LE.Y array\n");
 	   exit (0);
    }

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
   atid_count = 0;
   coord_cnt = 0;
   att_flag = 0;

/*===================DATA SECTION========================*/

   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

	   /* CALL RD123SFLD() TO READ SUBFIELD */
	   if(!read_data_sfld (filename,fpin,tag,&leadid,string,&bytlen,&status)) 
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
#if TEST_VER
	   /*DEBUG*/ if (rec_num == 100)
					goto end;
#endif					
				
		   if (first_rec)
			   first_rec = 0;
		   else
		   {
				/*WRITE OUT PREVIOUS RECORD*/
			    if (rec_num % 100 == 0)
			    {
			        fprintf(stderr,". ",  rec_num);
					/*
			        fprintf(stderr,"Line # %d\n",  rec_num);
					*/
			  /*
			  fprintf (stderr, "%d %d %d %d\n", bounds->min_x, bounds->max_x, bounds->min_y, bounds->max_y);
			  */
			    }

		   /*write out previous record*/
		   /*write dig*/
			    Vect_copy_xy_to_pnts (Points, LE.X, LE.Y, coord_cnt);

			    if (LE.pidl == LE.pidr)
				  Vect_write_line (Map, LINE, Points);
			    else
				  Vect_write_line (Map, AREA, Points);

                cur_mfold->n_lines++;

                if (noNA_flag)
		            compute_areapoint ( &LE, coord_cnt );

                if (att_flag)
                {
		            /*get dig_att x and y*/
		             get_line_center (&att_x, &att_y, Points);
                     build_attr_structs_from_tmp_list
                      (obj_code, LINE, atid_count, att_x,  att_y);
					 cur_mfold->n_attr_objs++;
                     att_flag = 0;
                     atid_count = 0;
                }
                else
                if (find_objs_in_ff_elem_list (obj_code))
                {

		            /*get dig_att x and y*/
		             get_line_center (&att_x, &att_y, Points);
                     build__attr_structs (obj_code, (char *) NULL, LINE, att_x, att_y);
					 cur_mfold->n_attr_objs++;
                     att_flag = 0;
                     atid_count = 0;
                }

		        coord_cnt = 0;               
		   }
		   rec_num++;    
       }
	   else if (strcmp (tag, "LINE") == 0) 
	   {
		  if (strcmp (descr, "RCID") == 0)
		  {
			 sprintf (obj_code, "%s#%s", modn, G_squeeze (string));
		  }
	   }
	   else if (strcmp (tag, "PIDL") == 0)
	   {
	     if (!strcmp (descr, "MODN"))
	       strcpy (LE.pidl_modn, G_squeeze (string));
	     if (!strcmp (descr, "RCID"))
	       LE.pidl = atoi (G_squeeze (string));
	   }
	   else if (strcmp (tag, "PIDR") == 0)
	   {
	     if (!strcmp (descr, "MODN"))
	       strcpy (LE.pidr_modn, G_squeeze (string));
	     if (!strcmp (descr,  "RCID"))
	       LE.pidr = atoi (G_squeeze (string));
	   }
	   else if (strcmp (tag, "SNID" ) == 0)
	   {
	   }
	   else if (strcmp (tag, "ENID") == 0)
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
			 sprintf (atid_code, "%s#%s", atid_prefix, G_squeeze (string));
			 add_atid_to_tmp_list (atid_code, atid_count);
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
			 if (++coord_cnt > COOR_MAX)
				G_fatal_error ("Maximum coordinates exceeded");
		     LE.X[coord_cnt-1] = S_globals->Iref_sfax *(double) coord_x;	
		  }
		  else if ((strcmp (descr, "Y")==0) || (strcmp (descr, "!Y") == 0))
		  {
		     coord_y = conv_bits (string);
			 get_min_max ( 2, coord_y, bounds);
		     LE.Y[coord_cnt-1] = S_globals->Iref_sfay *(double) coord_y;	
		  }
		  else
			fprintf (stderr, "# %d funny SADR descr: '%s'\n", rec_num, descr);
	   }
	   else
	   {
           fprintf (stderr, "%s %s\n", "Illegal Tag = ", tag);
	   }

    }

/*DEBUG*/

end:

/*write last record*/
/*to dig, dig_att, dig_cats, links*/
	/*
fprintf (stderr, "END: writing %d coords for line %d\n", coord_cnt, rec_num);
    */

   Vect_copy_xy_to_pnts (Points, LE.X, LE.Y, coord_cnt);

   if (LE.pidl == LE.pidr)
	    Vect_write_line (Map, LINE, Points);
   else
		Vect_write_line (Map, AREA, Points);

   cur_mfold->n_lines++;

   if (noNA_flag)
      compute_areapoint (&LE, coord_cnt);

	if (att_flag)
	{
		/*get dig_att x and y*/
		 get_line_center (&att_x, &att_y, Points);
		 build_attr_structs_from_tmp_list
		  (obj_code, LINE, atid_count, att_x,  att_y);
		 cur_mfold->n_attr_objs++;
		 att_flag = 0;
		 atid_count = 0;
	}
	else
	if (find_objs_in_ff_elem_list (obj_code))
	{

		/*get dig_att x and y*/
		 get_line_center (&att_x, &att_y, Points);
		 build__attr_structs (obj_code, (char *) NULL, LINE, att_x, att_y);
		 cur_mfold->n_attr_objs++;
		 att_flag = 0;
		 atid_count = 0;
	}

	/*DEBUG*/ 
	/*fprintf (stderr, "min x %ld \n", bounds->min_x);
	  fprintf (stderr, "max x %ld \n", bounds->max_x);
	  fprintf (stderr, "min y %ld \n", bounds->min_y);
	  fprintf (stderr, "max y %ld \n", bounds->max_y);
	*/

   /* CALL END123FILE() FOR INPUT FILE */
   if (!end_sdts_rfile( filename, &fpin))
      return (0);
   
   if (noNA_flag)
     setup_areapoint ();
   
   fprintf (stderr, "\n");

   /* STOP/EXIT */

   return(1);
}

