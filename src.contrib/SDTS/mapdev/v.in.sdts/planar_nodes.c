#include     <stdio.h>
#include     <signal.h>
#include     "sdts_in.h"
#include     "gis.h"
#include     "Vect.h"
#include     "stc123.h"
#include  "sdts_globals.h"

char *le_tags [] = {
    "dummy",
    "0001",
    "PNTS",
    "SADR",
    "ARID",
    "ATID"
};


/*********************
 
 processing of NODES:

   for now, only process NODES that have attributes, and import them
   as lines of type DOT
   eventually, this code will change when GRASS structures change to 
   accomodate attributed nodes
   check each node
   if the NODE is attributed
	   call build_attr_structs_from_tmp_list () to add attribute to lists
	   write out node as a GRASS line of type DOT
   else
	  skip
*********************/

int do_NO_module(cur_mfold, S_globals, bounds, Map, Points, Obj_num)
	struct Sdts_manifold *cur_mfold;
	struct Sdts_globals *S_globals;
	struct Bounds *bounds;
	struct Map_info *Map;
	struct line_pnts *Points;
	int *Obj_num;
{
   FILE *fp_no;
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

   struct Sdts_NO Node;
   long rec_num = 0;
   int coord_x, coord_y;
   int got_sadr;
   int first_rec;
   char modn[10];
   char obj_code[20];
   char atid_prefix[10];
   char atid_label[20];
   int atid_count, att_flag;

   
   if (cur_mfold->mod_nrec[NOxx] == 0)
   {
	   put_err_mess ("Required module (NO) not found.", 0);
       return 0;
   }
   strcpy (modn, cur_mfold->mod_name[NOxx]);
   strcpy (filename, cur_mfold->file_name[NOxx]);  

	fprintf (stderr, "Processing NO Module: %s ", filename);
    if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fp_no))
	   return (0);
   
   /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
   if (!read_dd_rec (filename, fp_no, string, &status)) {
	   end123file (&fp_no);
	   return (0);
   }

   
   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;
   got_sadr = 0;
   att_flag = 0;
   atid_count = 0;
   first_rec = 1;

/*===================DATA SECTION========================*/

   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

	   /* CALL RD123SFLD() TO READ SUBFIELD */
	   if(!read_data_sfld (filename,fp_no,tag,&leadid,string,&bytlen,&status)) 
	   {
	      end123file (&fp_no);
	      return (0);
       }

       if (!check_data_sfld (filename, fp_no, tag, descr, frmts))
	   {
	      end123file (&fp_no);
	      return (0);
       }

		  /*DEBUG*/
		  /*
          fprintf (stderr, "%s%ld\n",    "Record Number     = ",  rec_num);
          fprintf(stderr,"%s%s%s%s\n",   "* TAG & DESC      = ",tag, " | ", descr );
		  */

       if (strcmp (tag, "0001") == 0) {
            rec_num++;    
			/*DEBUG*/
#ifdef TEST_VER
			if (rec_num % 100 == 0)
			  goto end;
#endif
			if (rec_num % 100 == 0)
				  fprintf (stderr, ". ");
				  /*
				  fprintf (stderr, "Planar Node # %d\n", rec_num);
				  */

            got_sadr = 0;

			if (first_rec)
			   first_rec = 0;
            else
			{
			   if (att_flag)
			   {
		          build_attr_structs_from_tmp_list
				  (obj_code, DOT, atid_count, Node.x, Node.y);
				  /*write out node as entity point*/
				  dig_write_point (Map, Points, &(Node.x), &(Node.y), DOT);
				  cur_mfold->n_attr_nodes++;
				  att_flag = 0;
				  atid_count = 0;
			   }
			   else 
			   {
				   if (find_objs_in_ff_elem_list (obj_code))
				   {
				  /*write out attribute*/
					 build__attr_structs (obj_code, (char *) NULL, DOT, Node.x, Node.y);
				  /*write out node as entity point*/
					 dig_write_point (Map, Points, &(Node.x), &(Node.y), DOT);
				     cur_mfold->n_attr_nodes++;
					 att_flag = 0;
					 atid_count = 0;
					}
			   }
			}
       }
	   else if (strcmp (tag, "PNTS") == 0) 
	   {
		  if (strcmp (descr, "RCID") == 0)
		  {
			 sprintf (obj_code, "%s#%s", modn, G_squeeze (string));
		  }
	   }
	   else if (strcmp (tag, "LINE") == 0)
	   {
	   }
	   else if (strcmp (tag, "ATID") == 0)
	   {
		  att_flag = 1;
		  /*if node is attributed, we import this to grass as a entity point*/
		  /*since GRASS doesn't handle attributed nodes*/
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
          got_sadr = 1;
		 if ((strcmp (descr, "X") == 0) || (strcmp (descr, "!X") == 0))
		  {
			coord_x = conv_bits (string);
			if (coord_x  > 0)
				Node.x = S_globals->Iref_sfax * (double) coord_x;
			else
			  got_sadr = 0;
		  }
		else if ((strcmp (descr, "Y") == 0) || (strcmp (descr, "!Y") == 0))
		  {
			if (got_sadr)
			{
			   coord_y = conv_bits (string);
			   if (coord_y > 0)
				Node.y = S_globals->Iref_sfay * (double) coord_y;
			   else
				got_sadr = 0;
			}
		  }
	   }
	   else
	   {
		   fprintf (stderr, "\n%s %s\n", "Anomoly = ", tag);
	   }

      }

end:

	  /*write out last record if there is an attribute*/
	if (att_flag)
	{
	   build_attr_structs_from_tmp_list
	     (obj_code, DOT, atid_count, Node.x, Node.y);
	  /*write out node as entity point*/
		 dig_write_point (Map, Points, &(Node.x), &(Node.y), DOT);
	     cur_mfold->n_attr_nodes++;
	 }
	 else 
	 {
	   if (find_objs_in_ff_elem_list (obj_code))
	   {
		  /*write out attribute*/
		  build__attr_structs (obj_code, (char *) NULL, DOT, Node.x, Node.y);
		  /*write out node as entity point*/
		  dig_write_point (Map, Points, &(Node.x), &(Node.y), DOT);
	      cur_mfold->n_attr_nodes++;
	   }
     }

     cur_mfold->n_nodes = rec_num;

	 fprintf (stderr, "\n");

   /* CALL END123FILE() FOR INPUT FILE */
   if (!end_sdts_rfile (filename, &fp_no))
      return (0);
   
   cur_mfold->n_attr_objs += cur_mfold->n_attr_nodes;

   
   return (1);
}

