
#include <stdio.h>
#include <string.h>
#include "sdts_in.h"
#include "sdts_globals.h"
#include "stc123.h"

#define TRUE 1
#define FALSE 0

FILE *fpin;              /* input ISO 8211 file pointer */
FILE *fptemp;              /* input ISO 8211 file pointer */


long num_mod;
int i;                   /* loop counter */
int display_IDEN ();
int display_XREF ();
int display_SPDM ();
void display_mfolds ();
char *get_err_mess ();

void
display_info (info, globals, mfolds)
   struct Sdts_info *info;
   struct Sdts_globals *globals;
   struct Sdts_manifold *mfolds;
{

  printf ("\n");
  printf ("*****************************************************\n");
  printf ("         INFORMATION ABOUT THE SDTS DATA SET\n");
  printf ("*****************************************************\n\n");

  if (!display_IDEN (mfolds->file_name[IDEN]))
  {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from IDEN module aborted. Continuing.\n");
  }

  printf ("\n\n");
  if (mfolds->mod_name[XREF] && mfolds->mod_nrec[XREF])
    if (!display_XREF (mfolds->file_name[XREF]))
    {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from XREF module aborted. Continuing.\n");
    }

  printf ("\n\n");
  if (mfolds->mod_name[SPDm] && mfolds->mod_nrec[SPDm])
    if (!display_SPDM (mfolds->file_name[SPDm], globals))
    {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from SPDM module aborted. Continuing.\n");
    }

  printf ("\n\n");
  display_manifold (info, globals);

  /*
    num_mod = -1;

    read_stat_mod ();
    read_catd_mod ();
    */
}

void
display_summary_info (info, globals, mfolds)
   struct Sdts_info *info;
   struct Sdts_globals *globals;
   struct Sdts_manifold *mfolds;
{
  printf ("\n");
  printf ("         TRANSFER SUMMARY\n");
  printf ("         ----------------\n\n");

  if (!display_IDEN (mfolds->file_name[IDEN]))
  {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from IDEN module aborted. Continuing.\n");
  }

  printf ("\n\n");
  if (mfolds->mod_name[XREF] && mfolds->mod_nrec[XREF])
    if (!display_XREF (mfolds->file_name[XREF]))
    {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from XREF module aborted. Continuing.\n");
    }

  printf ("\n\n");
  if (mfolds->mod_name[SPDm] && mfolds->mod_nrec[SPDm])
    if (!display_SPDM (mfolds->file_name[SPDm], globals))
    {
	  fprintf (stderr, "Error: %s\n", get_err_mess());
	  fprintf (stderr, "Display of information from SPDM module aborted. Continuing.\n");
    }

  display_manifold_obj_summary (info, globals);
}

#if 0
/******************************************************************************


  dump_ap_mod

  Function to dump a particular record of an attribute primary module.

******************************************************************************/

dump_ap_mod ()
{
int status;

/*      Set up file name for Attribute Primary module        */

/*     Dump attribute primary module        */

printf ("\n\n");
printf ("\nUSER-DEFINED ATTRIBUTES ::::::::::::::::::::::::::::::::::::::::::::");
printf ("\n\nAttribute Primary module:  %s\n",file_name);

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fptemp))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fptemp,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");

         goto done;
         }

status = -1;

/*       Loop to process each subfield             */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fptemp,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* string length */
        &status))       /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD (AP MODULE)");

         goto done;
         }

/*      Retrieve description of current subfield        */


 if (! chk123sfld 
        (fptemp,          /* file pointer */
        tag,            /* tag output */
        descr,          /* subfield descriptions output */
        frmts))          /* subfield format control */
         {
         printf ("\nERROR CHECKING DATA RECORD SUBFIELD");

         goto done;
         }

  printf ("\n%-4s %-29s %-s",tag,descr,string);

 } while (status != 4);   /* Break out of loop at end of file */

/*       Close input ISO 8211 file        */

status = end123file (&fptemp);

done:

return;
}

/******************************************************************************


  read_stat_mod

  Function to read information from the Statistics module into the mod_stat 
  structure.

******************************************************************************/

read_stat_mod (filename)
char *filename;
{


printf ("\n\nMODULE SUMMARY ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
printf ("\n\nStatistics module:  %s",file_name);

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fptemp))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fptemp,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");

         goto done;
         }

status = -1;

/*       Loop to process each subfield             */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fptemp,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* string length */
        &status))       /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD (STAT MODULE)");

         goto done;
         }

/*      Retrieve description of current subfield        */


 if (! chk123sfld 
        (fptemp,          /* file pointer */
        tag,            /* tag output */
        descr,          /* subfield descriptions output */
        frmts))          /* subfield format control */
         {
         printf ("\nERROR CHECKING DATA RECORD SUBFIELD");

         goto done;
         }

  if (!strcmp (tag,"STAT") && !strcmp (descr,"MODN"))
       {
       num_mod++;
       }
  else if (!strcmp (tag,"STAT") && !strcmp (descr,"MNRF"))
       {
       strcpy (mod_stat[num_mod].name, string);
       }
  else if (!strcmp (tag,"STAT") && !strcmp (descr,"NREC"))
       {
       mod_stat[num_mod].nrec = atoi (string);
       }
  else if (!strcmp (tag,"STAT") && !strcmp (descr,"NSAD"))
       {
       mod_stat[num_mod].nsad = atoi (string);
       }

 } while (status != 4);   /* Break out of loop at end of file */

/*       Close input ISO 8211 file        */

status = end123file (&fptemp);

done:

return;
}

/*******************************************************************************

  read_catd_mod

        Function to read the Catalog/Directory module and display
        its contents.  Also displays statistics for each module
        previously read from the Statistics module.          
*******************************************************************************/

read_catd_mod ()

{
char  mod_name[5], mod_type[30], mod_volume[30], mod_file[30];
char  mod_extr[2], mod_vers[30], mod_comt[300];

strcpy (file_name, base_name);

strcat (file_name, "CATD.ddf");
printf ("\n\nCatalog/Directory module:  %s",file_name);
printf 
("\n\nName Type                       File         E Records    Spatial addrs");
if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fptemp))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fptemp,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");

         goto done;
         }

status = -1;

/*       Loop to process each subfield             */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fptemp,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* string length */
        &status))       /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD (CATD MODULE)");

         goto done;
         }

/*      Retrieve description of current subfield        */


 if (! chk123sfld 
        (fptemp,          /* file pointer */
        tag,            /* tag output */
        descr,          /* subfield descriptions output */
        frmts))          /* subfield format control */
         {
         printf ("\nERROR CHECKING DATA RECORD SUBFIELD");

         goto done;
         }

  if (!strcmp (tag,"CATD") && !strcmp (descr,"NAME"))
       {
       strcpy (mod_name, string);
       }
  else if (!strcmp (tag,"CATD") && !strcmp (descr,"TYPE"))
       {
       strcpy (mod_type, string);
       }
  else if (!strcmp (tag,"CATD") && !strcmp (descr,"FILE"))
       {
       strcpy (mod_file, string);
       }
  else if (!strcmp (tag,"CATD") && !strcmp (descr,"EXTR"))
       {
       strcpy (mod_extr, string);
       }
  else if (!strcmp (tag,"CATD") && !strcmp (descr,"MVER"))
       {
       strcpy (mod_vers, string);
       }
  else if (!strcmp (tag,"CATD") && !strcmp (descr,"COMT"))
       {
       strcpy (mod_comt, string);
       }

  if (status == 3 || status == 4)
      {

      for (i=0; i<num_mod; i++)
        {
        if (!strcmp (mod_name, mod_stat[i].name))
             {

             printf ("\n%-4s %-20s %-12s %-1s %-10d %-10d",
                   mod_name, mod_type, mod_file, mod_extr,
                   mod_stat[i].nrec, mod_stat[i].nsad);
             mod_name[0] = 0;
             mod_type[0] = 0;
             mod_volume[0] = 0;
             mod_file[0] = 0;
             break;
             }
        }
      }

 } while (status != 4);   /* Break out of loop at end of file */

/*       Close input ISO 8211 file        */

status = end123file (&fptemp);

done:

return;

}

#endif

int
display_IDEN (filename)
  char *filename;
{
  char ice[2], ccs[4];
  char leadid;
  char tag[10];
  char string[5000];
  char descr[5000];
  char frmts[500];
  int status;
  long bytlen;
  long int_level;

  if (! open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
      return(0);

  printf ("Identification module:  %s\n",filename);

  /*      Read Identification module data descriptive record (DDR)      */

  if (! read_dd_rec (filename, fpin, string, &status))    /* status returned */
    {
	  end123file (&fpin);
      return (0);
    }

  status = -1;

  /*       Loop to process each subfield in Identification module            */

  do {
    
    /*      Read data record subfield    */
    
    if (! read_data_sfld (filename, fpin, tag, &leadid, string, &bytlen, &status)) 
    {
	   end123file (&fpin);
       return (0);
    }

    /*      Retrieve description of current subfield        */


    if (! check_data_sfld (filename, fpin, tag, descr, frmts)) 
    {
	   end123file (&fpin);
       return (0);
    }

    /*    Display subfield name and contents for each subfield       */

    if (!strcmp (tag, "IDEN") && !strcmp (descr, "STID"))
      printf ("\nStandard identification:           %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "STVS"))
      printf ("\nStandard version:                  %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "DOCU"))
      printf ("\nStandard documentation reference:  %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "PRID"))
      printf ("\nProfile identification:            %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "PRVS"))
      printf ("\nProfile version:                   %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "PDOC"))
      printf ("\nProfile documentation reference:   %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "TITL"))
      printf ("\nTitle:                             %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "DAID"))
      printf ("\nData ID:                           %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "DAST"))
      printf ("\nData structure:                    %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "MPDT"))
      printf ("\nMap date:                          %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "DCDT"))
      printf ("\nData set creation date:            %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "SCAL"))
      printf ("\nScale:                             %s",string);
    else if (!strcmp (tag, "IDEN") && !strcmp (descr, "COMT"))
      printf ("\nComment:                           %s",string);
#if 0
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "FFYN"))
      {
        printf ("\n\n");
        printf ("\nCONFORMANCE  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
        printf ("\nComposites:                        %s",string);
      }
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "VGYN"))
      printf ("\nVector geometry:                   %s",string);
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "GTYN"))
      printf ("\nVector topology:                   %s",string);
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "RCYN"))
      printf ("\nRaster:                            %s",string);
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "EXSP"))
      printf ("\nExternal spatial reference:        %s",string);
    else if (!strcmp (tag, "CONF") && !strcmp (descr, "FTLV"))
      printf ("\nFeatures level:                    %s",string);

    /*      If field is a foreign ID for an attribute primary record,
	    find the record and dump it            */

    else if (!strcmp (tag, "ATID") && !strcmp (descr, "MODN"))
      {
	
	/*  Save the module name just read from the Identification module */

	strcpy (mod_name, string);

	/*      Read next data record subfield    */

	if (! rd123sfld (fpin, tag, &leadid, string, &bytlen, &status))
	  {
	    fprintf (stderr, "\nERROR READING DATA RECORD SUBFIELD (RCID)");

	    exit (0);
	  }

	/*      Retrieve description of current subfield        */

	if (! chk123sfld (fpin, tag, descr, frmts)) 
	  {
	    fprintf (stderr, "\nERROR CHECKING DATA RECORD SUBFIELD");
	    exit (0);
	  }

	/*   The subfield just read should contain the record ID for the
	     Attribute Primary module.  If it doesn't, print error message.
	     */

	if (strcmp(tag,"ATID") || strcmp (descr,"RCID"))
	  {
	    fprintf (stderr, "\nFile is inconsistent with profile requirements:");
	    fprintf (stderr, "\nSubfields of ATID field are not in correct order.");
	    fprintf (stderr, "\nForeign identifier %s %s will be skipped",
		    mod_name, string);
	    return (0);
	  }

	printf ("\nForeign ID:                        %s %s",mod_name,string);
	
	/*
	  dump_ap_mod();
	  */
      }
#endif
  } while (status != 4);   /* Break out of loop at end of file */


  /*       Close input Identification module           */

  if (!end_sdts_rfile (filename, &fpin))
     return (0);

  return (1);

}

int
display_XREF (filename)  
  char *filename;
{
  char ice[2], ccs[4];
  char leadid;
  char tag[10];
  char string[5000];
  char descr[5000];
  char frmts[500];
  int status;
  long bytlen;
  long int_level;

  printf ("External Spatial Reference module:  %s\n", filename);

  if (! open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
    {
      return (0);
    }

  if (! read_dd_rec (filename, fpin, string, &status))
    {
      return (0);
    }

  status = -1;

  do {
    if (! read_data_sfld (filename, fpin, tag, &leadid, string, &bytlen, &status))
      {
		   end123file (&fpin);
		   return (0);
      }

	G_squeeze (string);

    if (!check_data_sfld (filename, fpin,  tag, descr, frmts))
      {
		   end123file (&fpin);
		   return (0);
      }
  
    if (!strcmp (tag, "XREF") && !strcmp (descr, "RSNM"))
      {
	fprintf (stderr, "Reference System name:\t\t\t%s\n", string);
      }
    else if (!strcmp (tag, "XREF") && !strcmp (descr, "HDAT"))
      {
	fprintf (stderr, "Horizontal Datum:\t\t\t%s\n", string);
      }
    else if (!strcmp (tag, "XREF") && !strcmp (descr, "ZONE"))
      {
	fprintf (stderr, "Zone Number:\t\t\t\t%s\n", string);
      }
  } while (status != 4);

  if (!end_sdts_rfile (filename, &fpin))
       return (0);

  return (1);
}


int
display_SPDM (filename, globals)
  char *filename;
  struct Sdts_globals *globals;
{
  char ice[2], ccs[4];
  char leadid;
  char tag[10];
  char string[5000];
  char descr[5000];
  char frmts[500];
  int status;
  long bytlen;
  long int_level;

  printf ("Spatial Domain module:  %s\n", filename);

  if (! open_sdts_rfile (filename, &int_level, ice, ccs, &fpin))
    {
      return (0);
    }

  if (! read_dd_rec (filename, fpin, string, &status))
    {
	  end123file (&fpin);
      return (0);
    }

  status = -1;

  do {
    if (! read_data_sfld (filename, fpin, tag, &leadid, string, &bytlen, &status))
      {
	  end123file (&fpin);
      return (0);
      }

    if (!check_data_sfld (filename, fpin,  tag, descr, frmts))
      {
	  end123file (&fpin);
      return (0);
      }
  
    if (!strcmp (tag, "SPDM") && !strcmp (descr, "DTYP"))
      {
	fprintf (stderr, "Spatial Domain type:\t\t\t%s\n", G_squeeze (string));
      }
    else if (!strcmp (tag, "XREF") && !strcmp (descr, "DSTP"))
      {
	fprintf (stderr, "Domain Spatial Address Type:\t\t\t%s\n", G_squeeze (string));
      }
    else if (!strcmp (tag, "DMSA"))
      {

		if (strcmp (descr, "X") == 0)
          fprintf (stderr, "\tDomain Spatial Address %s:\t%f\n", descr, 
			 globals->Iref_sfax * ((double) conv_bits (string)) );
		else if (strcmp (descr, "Y") == 0)
          fprintf (stderr, "\tDomain Spatial Address %s:\t%f\n", descr, 
			 globals->Iref_sfay * ((double) conv_bits (string)) );
      }
  } while (status != 4);

  if (!end_sdts_rfile (filename, &fpin))
     return (0);

  return (1);

}

