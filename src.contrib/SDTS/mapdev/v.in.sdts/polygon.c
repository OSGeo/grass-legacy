/*#include <dig_atts.h>*/
#include     <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "sdts_in.h"
#include "sdts_globals.h"
#include "stc123.h"


struct Sdts_PC *polygon_modns;

/***********
process PC module AFTER NA module, if NA module is present.

   if (NA module)
    for each PC record, retrieve corresponding att_x and att_y from
	  area_pnt_list; then for each attribute pointer pass to
	  build_att_pnt_list();

*************/

int do_PC_module (cur_mfold, S_globals, bounds, Map, Points )
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
  char string [5000];
  char descr [5000];
  char frmts [500];
  int status;
  long bytlen;
  long int_level;
  
  long rec_num = 0;
  double att_x, att_y;
  char modn[10];
  char pc_code[20];
  char atid_prefix[10];
  char atid_code[20];
  int first_rec;
  int atid_count, att_flag;
   

  strcpy (modn, cur_mfold->mod_name[PCxx]);
  /*
    strcpy (filename, G_tolcase (cur_mfold->file_name[PCxx]));  
    */
  strcpy (filename, cur_mfold->file_name[PCxx]);  

  if (!(cur_mfold->mod_nrec[PCxx]))
  {
	put_err_mess ("Required module (PC) not found.", 0 );
    return (0);
  }
   fprintf (stderr, "Processing PC module: %s ", filename);

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

    if (strcmp (tag, "0001") == 0) {
      rec_num++;    
      if (first_rec)
	     first_rec = 0;
      else
	  {
		 if (rec_num % 100 == 0)
			fprintf (stderr, ". ");
			/*
			fprintf (stderr, "Polygon # %d\n", rec_num);
			*/

		 if (att_flag || find_objs_in_ff_elem_list (pc_code))
		 {
		   if (!get_xy_from_area_pnt_list (pc_code, &att_x, &att_y)) 
		   {
			   if (atoi(pc_code + 5) != 1)/*bypass Universe polygon*/
			   {
				   sprintf (Error_msg, "No area_point for attributed area '%s' found\n", pc_code);
				   G_warning (Error_msg);
			   }
		   }
		   else
		   {
		       cur_mfold->n_attr_objs++;
			   if (att_flag)
			   {
				  build_attr_structs_from_tmp_list (pc_code, AREA, atid_count, att_x, att_y);
				  att_flag = 0;
			   }
			   else
				  build__attr_structs (pc_code, (char *) NULL,  AREA, att_x, att_y);
           }
		   atid_count = 0;
		}
	  }
    }
    else if (strcmp (tag, "POLY") == 0) 
      {
	if (strcmp (descr, "RCID") == 0)
	  {
	    sprintf (pc_code, "%s#%s", modn, G_squeeze (string));
	  }
      }
    else if (strcmp (tag, "ATID") == 0)
      {
		  att_flag =  1;
	      if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
			 strcpy (atid_prefix, G_squeeze (string));
		  else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
			{
			  atid_count++;
			  sprintf (atid_code, "%s#%s", atid_prefix, G_squeeze (string));
			  add_atid_to_tmp_list (atid_code, atid_count);
			}


      }
    else if (strcmp (tag, "RFID") == 0)
	{
	}
    else if (strcmp (tag, "CHID") == 0)
	{
	}
    else if (strcmp (tag, "CPID") == 0)
	{
	}
    else if (strcmp (tag, "RPID") == 0)
	{
	}
    else
      {
	fprintf (stderr, "\n%s %s\n", "Anomoly = ", tag);
      }

  }
  end:
  /*write last record*/

   if (att_flag || find_objs_in_ff_elem_list (pc_code))
   {
	   if (!get_xy_from_area_pnt_list (pc_code, &att_x, &att_y)) 
	   {
		   if (atoi(pc_code + 5) != 1)/*bypass Universe polygon*/
		   {
		   sprintf (Error_msg, "No area_point for attributed area '%s' found\n", pc_code);
		   G_warning (Error_msg);
		   }
	   }
	   else
	   {
	       cur_mfold->n_attr_objs++;
		   if (att_flag)
	       {
			  build_attr_structs_from_tmp_list 
				(pc_code, AREA, atid_count, att_x, att_y);
				  att_flag = 0;
		   }
		   else
				  build__attr_structs 
					(pc_code, (char *) NULL,  AREA, att_x, att_y);
	   }
	   atid_count = 0;
   }
   fprintf (stderr, "\n");

  /* CALL END123FILE() FOR INPUT FILE */
  if (!end_sdts_rfile(filename, &fpin))
    return (0);
   
  cur_mfold->n_polygons = rec_num -1; /* -1 to skip universe polygon*/
  
  return (1);
}

