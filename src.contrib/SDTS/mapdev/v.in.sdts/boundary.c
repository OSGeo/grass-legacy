
#include     <stdio.h>
#include     <signal.h>
#include     "sdts_in.h"
#include     "gis.h"
#include     "Vect.h"
#include     "stc123.h"
#include  "sdts_globals.h"



int get_boundary (cur_mfold, S_globals, bounds)
    struct Sdts_manifold *cur_mfold;
    struct Sdts_globals *S_globals;
    struct Bounds *bounds;
{
   FILE *ne_file, *le_file;
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

   long rec_num = 0;
   int coord_x, coord_y ;
   char modn[10];
   int first_x, first_y;

 

   /*initialize first_time to yes*/
   first_x = first_y = 1;

   if (cur_mfold->mod_name[NExx] != 0 && cur_mfold->mod_nrec[NExx] != 0)
   {
        strcpy (modn, cur_mfold->mod_name[NExx]);
        strcpy (filename, cur_mfold->file_name[NExx]);  

   
   if (!open_sdts_rfile (filename,  &int_level, ice, ccs, &ne_file))
   {
       sprintf (Error_msg, "Can't open SDTS NE-entity point file: %s\n", filename);
       G_fatal_error (Error_msg);
   }
   
   /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
   if (!read_dd_rec(filename, ne_file,string,&status)) 
   {
       sprintf (Error_msg, "Unable to read DDR for NO-planar node. status = %d\n", status);
       G_fatal_error (Error_msg);
   }

   
   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;


   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

	   /* CALL RD123SFLD() TO READ SUBFIELD */
	   if(!read_data_sfld (filename, ne_file,tag,&leadid,string,&bytlen,&status)) {
          sprintf (Error_msg, "Unable to read DR subfield: status = %d\n", status);
          G_fatal_error (Error_msg);
       }

       if (!check_data_sfld (filename, ne_file,tag,descr,frmts)){
          sprintf (Error_msg, "Unable to retrieve input DR subfield description\n");
          G_fatal_error (Error_msg);
       }

       if (strcmp (tag, "0001") == 0) {
                rec_num++;    
		}
	   else if (strcmp (tag, "SADR") == 0)
	   {

		  if ((strcmp (descr, "X") == 0) || (strcmp (descr, "!X") == 0))
		  {
		     coord_x = conv_bits (string);
		     if (first_x)
		       {
			 first_x = 0;
			 bounds->min_x = bounds->max_x = coord_x;
		       }
		     else	
		       {
		         bounds->min_x = bounds->min_x > coord_x ? coord_x : bounds->min_x;
		         bounds->max_x = bounds->max_x < coord_x ? coord_x : bounds->max_x;
		       }
		  }
		  else if ((strcmp (descr, "Y") == 0) || (strcmp (descr, "!Y") == 0))
		  {
		     coord_y = conv_bits (string);
		     if (first_y)
		       {
			 first_y = 0;
			 bounds->min_y = bounds->max_y = coord_y;
		       }
                     else
		       {
		         bounds->min_y = bounds->min_y > coord_y ? coord_y : bounds->min_y;
		         bounds->max_y = bounds->max_y < coord_y ? coord_y : bounds->max_y;
                        }
		  }
	   }

      }
   
/*write last record*/

	   /* CALL END123FILE() FOR INPUT FILE */
	   if (!end_sdts_rfile (filename, &ne_file))
	   {
		  printf("%s\n","Unable to close input file");
		  exit(1);
	   }
   }

   if (cur_mfold->mod_name[LExx] && cur_mfold->mod_nrec[LExx])
   {

   strcpy (modn, cur_mfold->mod_name[LExx]);
   /*
   strcpy (filename, G_tolcase (cur_mfold->file_name[LExx]));  
   */
   strcpy (filename, cur_mfold->file_name[LExx]);  


    if (!open_sdts_rfile (filename, &int_level, ice, ccs, &le_file))
    {
       sprintf (Error_msg, "Can't open SDTS LE - line file: %s\n", filename);
       G_fatal_error (Error_msg);
    }
   /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
   if (!read_dd_rec (filename, le_file,string,&status)) {
       sprintf (Error_msg, "Unable to read DDR for LE-line module. status = %d\n", status);
       G_fatal_error (Error_msg);
   }

   
   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;

/*===================DATA SECTION========================*/

   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

	   /* CALL RD123SFLD() TO READ SUBFIELD */
	   if(!read_data_sfld (filename, le_file,tag,&leadid,string,&bytlen,&status)) 
	   {
          sprintf (Error_msg, "Unable to read DR subfield: status = %d\n", status);
          G_fatal_error (Error_msg);
       }

       if (!check_data_sfld (filename, le_file,tag,descr,frmts))
	   {
          sprintf (Error_msg, "Unable to retrieve input DR subfield description\n");
          G_fatal_error (Error_msg);
       }
		  /*DEBUG*/
		  /*
          fprintf (stderr, "%s%ld\n",    "Record Number     = ",  rec_num);
          fprintf(stderr,"%s%s%s%s\n",   "* TAG & DESC      = ",tag, " | ", descr );
		  */

	  if (strcmp (tag, "SADR") == 0)
	   {

		  if ((strcmp (descr, "X") == 0) || (strcmp (descr, "!X") == 0))
		  {
		     coord_x = conv_bits (string);
		     if (first_x)
		       {
			 first_x = 0;
			 bounds->min_x = bounds->max_x = coord_x;
		       }
                     else
		       {
		         bounds->min_x = bounds->min_x > coord_x ? coord_x : bounds->min_x;
		         bounds->max_x = bounds->max_x < coord_x ? coord_x : bounds->max_x;
                       }
		  }
		  else if ((strcmp (descr, "Y")==0) || (strcmp (descr, "!Y") == 0))
		  {
		     coord_y = conv_bits (string);
		     if (first_y)
		       {
			 first_y = 0;
			 bounds->min_y = bounds->max_y = coord_y;
		       }
                     else
		       {
		          bounds->min_y = bounds->min_y > coord_y ? coord_y : bounds->min_y;
		          bounds->max_y = bounds->max_y < coord_y ? coord_y : bounds->max_y;
                       }
		  }
		  else
			fprintf (stderr, "# %d funny SADR descr: '%s'\n", rec_num, descr);
	   }
      }


   /* CALL END123FILE() FOR INPUT FILE */
	   if (!end_sdts_rfile(filename, &le_file)){
		  printf("%s\n","Unable to close input file");
		  exit(1);
	   }
   }
   

   
   /* STOP/EXIT */
   return(1);
}
   
