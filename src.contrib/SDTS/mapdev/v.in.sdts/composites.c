#include     <stdio.h>
#include     "gis.h"
#include     "sdts_in.h"
#include     <stc123.h>
#include  "sdts_globals.h"

do_composites (Att_FF_info)
   struct att_ff_info *Att_FF_info;
{
   FILE *fpin;
   char filename[100];
   char ice[2], ccs[4];
   char leadid;
   long bytlen;
   long int_level;
   int i;
   char descr[500];
   char frmts[500];
   char string[1000];
   char tag[10];
   int status;
   int rec_num;
   int first_rec;
   char modn [10];
   char rec_label[20];
   char atid_prefix[10];
   char atid_label[30];
   char elem_prefix[10];
   char elem_label[30];

   if (Att_FF_info->n_ff_files == 0)
   {
	   FF_flag = 0;
	   return;
   }

   for (i = 0; i < Att_FF_info->n_ff_files; i++)
   {

	   strcpy (filename, Att_FF_info->FF[i].fname);
       strncpy (modn, filename + 4, 4);
       modn[4] = '\0';

	   fprintf (stderr, "Processing Composite module: %s ", filename);

	   if (!open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
			 return (0);
		  /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
	   if (!read_dd_rec(filename, fpin,string,&status)) 
	   {
			end123file (&fpin);
			return (0);
	   }


		 /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
	   status = -1;
	   first_rec = 1;
	   rec_num = 0;

/*==================DATA SECTION============*/
   
	   while (status !=4) {

       /* CALL RD123SFLD() TO READ SUBFIELD */
       if(!read_data_sfld(filename, fpin,tag,&leadid,string,&bytlen,&status)) 
	   {
		  /*this is a hack to get around TIGER ff files with no records*/
		  if (first_rec)
		  {
			fprintf (stderr, "\n");
            sprintf (Error_msg, "Unable to read first DR subfield in file: status = %d. Continuing with next file.\n", status);
			G_warning (Error_msg);
		    goto end;
		  }
		  else
		  {
            end123file (&fpin);
            return (0);
		  }
       }
	   if (status == 3 || status == 4)
		  rec_num++;

	   G_squeeze (string);

       if (!check_data_sfld(filename, fpin,tag,descr,frmts)){
            end123file (&fpin);
            return (0);
       }
          /*DEBUG*/
		  /*
		  if (rec_num % 100 == 0)
		  {
          fprintf (stderr, "%s%ld\n",    "Record Number     = ",  rec_num);
          fprintf(stderr,"%s%s%s%s\n",   "* TAG & DESC      = ",tag, " | ", descr);
		  }
          */
	   if (strcmp (tag, "0001") == 0) {

           if (first_rec)
               first_rec = 0;
           else /*WRITE OUT PREVIOUS RECORD*/
			  if (rec_num % 100 == 0)
				  fprintf (stderr, ". ");
			  /*
				  fprintf(stderr,"Composite record # %ld\n",  rec_num);
				  */

	   }
       else if (strcmp (tag, "COMP") == 0)
       {
          if (strcmp (descr, "RCID") == 0)
          {
             sprintf (rec_label, "%s#%s", modn, string);
          }
       }

	   else if (strcmp (tag, "ATID") == 0)
	   {
          if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
             strcpy (atid_prefix, string);
          else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
          {
             /*GETTING_ATTRIBUTE_PTRS*/
             sprintf (atid_label, "%s#%s", atid_prefix, string);
             /*DEBUG*//* fprintf (stderr, "%s %s\n", rec_label, atid_label);*/
             build_ff_attr_structs (rec_label, atid_label, 1 );
          }

	   }
	   else if (strcmp (tag, "FRID") == 0)
	   {
          if (strcmp (descr, "MODN") == 0 || strcmp (descr, "!MODN") == 0)
             strcpy (elem_prefix, string);
          else if (strcmp (descr, "RCID") == 0 || strcmp (descr, "!RCID") == 0)
          {
             /*GETTING_ELEMENT_PTRS*/
             sprintf (elem_label, "%s#%s", elem_prefix, string);
             /*DEBUG*//* fprintf (stderr, "%s %s\n", rec_label, elem_label);*/
             add_ff_elem_to_list (rec_label, elem_label);
          }

	   }

	}
end:
    if (!end_sdts_rfile (filename, &fpin))
	  return (0);

   fprintf (stderr, "\n");
   
   }

   if (rec_num)
	 FF_flag = 1;

   return (1);
}
