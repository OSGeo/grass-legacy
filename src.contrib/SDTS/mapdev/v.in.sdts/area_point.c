#include "sdts_in.h"
/*#include <dig_atts.h>*/
#include <stdio.h>
#include "sdts_globals.h"
#include "stc123.h"
#include "gis.h"
#include "Vect.h"

/**NA  (Area Point) processing:

the TVP requires:
	 NA records point to  their surrounding PC record
	 PC records point to zero or more Attribute records

	 therefore, things are handled indirectly.

       NA-module processing consists of creating a list of records,
		 with add_area_pnt_to_list():

		 area_pnt_list: NA_code   PC_code  att_x   att_y

   later, in PC module processing: (see polygon.c)

	  for each new PC record,
		 get matching att_x and att_y from  area_pnt_list
         for each Attribute pointer (in PC record)
		   build_attr_pnt_list (PC_code, Attr_code, att_x, att_y)

Therefore, PC module(s) MUST be processed AFTER NA modules!

**/

int do_NA_module (cur_mfold, S_globals, bounds, Map, Points, Obj_num)
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
   char string[5000];
   char descr [5000];
   char frmts [500];
   int status;
   long bytlen;
   long int_level;

   struct Sdts_NA NA;
   long rec_num = 0;
   int coord_x, coord_y;
   char modn[10];
   char na_code[20];
   char arid_prefix[10];
   char arid_code[20];
   char atid_prefix[10];
   char atid_label[20];
   int first_rec;
   int got_sadr;


   if (cur_mfold->mod_name[NAxx] == 0  || cur_mfold->mod_nrec[NAxx] == 0)
   {
 	 noNA_flag = 1;
     return (1);
   }

  strcpy (modn, cur_mfold->mod_name[NAxx]);
  strcpy (filename, cur_mfold->file_name[NAxx]);  

  fprintf (stderr, "Processing NA module: %s ", filename);
   
  if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
    {
	  fprintf (stderr, "\n");
      sprintf (Error_msg, "Can't open SDTS NA-area point file: %s.\n Area points will be generated.", filename);
      G_warning (Error_msg);
	  noNA_flag = 1;
	  return 0;
    }
   
  /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
  if (!read_dd_rec(filename, fpin,string,&status)) 
  {
	  noNA_flag = 1;
	  end123file (&fpin);
	  return (0);
  }

   
  /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
  status = -1;

  /*initialize first_rec to yes*/
  first_rec = 1;
  got_sadr = 0;

  /*===================DATA SECTION========================*/

  /* WHILE NOT END OF INPUT FILE DO */
  while (status != 4) {


	/*DEBUG*//* fprintf (stderr, "reading first NA rec\n");*/
    /* CALL RD123SFLD() TO READ SUBFIELD */
    if(!read_data_sfld(filename, fpin,tag,&leadid,string,&bytlen,&status)) {
	  noNA_flag = 1;
	  end123file (&fpin);
	  return (0);
    }

    if (!check_data_sfld (filename, fpin,tag,descr,frmts))
	{
	  noNA_flag = 1;
	  end123file (&fpin);
	  return (0);
    }

    if (strcmp (tag, "0001") == 0) {
	  if (first_rec)
	  {
		  first_rec = 0;
	  }
	  else if (got_sadr)
	  {
          add_area_pnt_to_list (na_code, arid_code, NA.x, NA.y);
	  }
      rec_num++;    
      (*Obj_num)++;
      NA.att_num = *Obj_num;
	  got_sadr = 0;
    }
    else if (strcmp (tag, "PNTS") == 0) 
    {
	     if (strcmp (descr, "RCID") == 0)
	     {
	         NA.rcid = atoi (string);
	         sprintf (na_code, "%s#%s", modn, G_squeeze (string));
	     }
    }
    else if (strcmp (tag, "ATID") == 0)
    {
/*this should never happen; attributed area points are ILLEGAL in the TVP*/
	if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
	  strcpy (atid_prefix, G_squeeze (string));
	else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
	  {
	    sprintf (atid_label, "%s#%s", atid_prefix, G_squeeze (string));
	    /*DEBUG*/ fprintf (stderr, "%s %s\n", na_code, atid_label);
		/*
		build_attr_structs (na_code, atid_label, 1, 1.0, 1.0);
		*/
	  }
    }
    else if (strcmp (tag, "ARID") == 0)
    {
	     if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
	         strcpy (arid_prefix, G_squeeze (string));
	     else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
		 {
	         sprintf (arid_code, "%s#%s", arid_prefix, G_squeeze (string));
         } 
	
    }
    else if (strcmp (tag, "SADR") == 0)
    {
		  got_sadr = 1;
	     if ((strcmp (descr, "X") == 0) || (strcmp (descr, "!X") == 0))
	     {
	        coord_x = conv_bits (string);
		    if (coord_x  > 0)
		    {
	           get_min_max ( 1, coord_x, bounds);
	           NA.x = S_globals->Iref_sfax * (double) coord_x;
		    }
		    else
		       got_sadr = 0;
	     }
	     else if ((strcmp (descr, "Y") == 0) || (strcmp (descr, "!Y") == 0))
	     {
		    if (got_sadr)
		    {
	            coord_y = conv_bits (string);
		        if (coord_y > 0)
		        {
	               get_min_max ( 2, coord_y, bounds);
			    NA.y = S_globals->Iref_sfay * (double) coord_y;
		        }
                else
			       got_sadr = 0;
		    }
	     }
      }
      else
	    fprintf (stderr, "\n%s %s\n", "Anomoly = ", tag);

    }
    /*write last record*/

  if (got_sadr)
	  add_area_pnt_to_list (na_code, arid_code, NA.x, NA.y);
   
	
  /* CALL END123FILE() FOR INPUT FILE */
  if (!end_sdts_rfile (filename, &fpin))
  {
 	 noNA_flag = 1;
     return (0);
  } 
  /* STOP/EXIT */
/*debug*/
/*
fprintf (stderr, "leaving NA module\n");
*/
  fprintf (stderr, "\n");

  return (1);
}

