/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)
                                 FIPS 173


                          SDTS Summary Utility

                 program:         sum173.c
                 author:          Bob Lazar
                 date:            April 2, 1993
                 language:        C


         This program displays general information about an SDTS transfer.
         Information is read from the Identification, Catalog/Directory, and Statistics 
         modules as well as any Attribute Primary module record referenced by the Identification module.

         This program assumes that the transfer is compliant with the 
         Topological Vector Profile (TVP) or other profile which has 
         the following requirements:
         - All files follow the naming requirements specified in the TVP
         - All files are in the same directory
         - All standadard parts of the file name (last 4 characters of name
           and extension) are in upper case letters

         This program is an example which uses the FIPS 123 Function Library.  
         This is a public domain software library which supports the SDTS, 
         an exchange standard for digital spatial data which uses ANSI/ISO 8211 
         (FIPS 123) as its implementation.

         The FIPS 123 Function Library has been developed on the MS/DOS
         and Data General AViiON Unix platforms.  It has not been fully
         tested on other platforms.

         Information on SDTS, and on how to obtain this library, is available
         from the following address:

              SDTS Program Coordinator
              U.S. Geological Survey
              National Mapping Division
              526 National Center
              Reston, VA   22092

              Fax:    (703) 648-5542
              E-mail: sdts@usgs.gov

         To compile and link under DOS using Microsoft C:

                 CL /AL SUM173.C 123_MSC.LIB

         To run:

         SUM173 BASE_NAME

         where BASE_NAME is the 4-character base name for the transfer
         required by TVP naming requirements.  It may also include a
         device and/or directory path.



*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#define TRUE 1
#define FALSE 0

typedef struct stats
{

       char    name[5];
       unsigned int   nrec;
       unsigned int nsad;
}  Stats;

FILE *fpin;              /* input ISO 8211 file pointer */
FILE *fptemp;              /* input ISO 8211 file pointer */


long num_mod;
Stats mod_stat[500];    /* used to store statistics module info */
long str_len;            /* byte length of subfield returned by rd123sfld */
long int_level;          /* interchange level returned by beg123file */              
int i;                   /* loop counter */
int status;              /* status returned by FIPS library routines */
char ice;                /* inline code extension from beg123file */
char leadid;             /* leader ID returned by rd123sfld */
char ccs[4];             /* character set string returned by beg123file */
char tag[10];            /* field tag returned by rd123sfld and chk123sfld */
char base_name[100];     /* input ISO 8211 file base name */
char file_name[100];     /* input ISO 8211 file name */
char string[5000];       /* subfield data returned by rd123sfld */
char descr[100];        /* subfield description (mnemonic) returned by chk123sfld */
char frmts[100];         /* subfield format returned by chk123sfld */
char mod_name[5];            /* temporary storage of module names */


main(argc,argv)
  int argc;
  char *argv[];
{

/*     Print error if argument was not included on command line        */
       
if (argc < 2)
  {
  printf ("\nError:  no transfer base on command line\n");
  printf ("\nsum173 <input_base>");
  printf ("\n   input base:  directory path (optional)");
  printf ("\n                4-character base file name");
  exit(0);
  }

/*    Get file base name from command line   */

strcpy (base_name, argv[1]);

/*      Open Identification module */

strcpy (file_name,base_name);
strcat (file_name,"IDEN.DDF");

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

printf ("\nSDTS TRANSFER SUMMARY :::::::::::::::::::::::::::::::::::::::::::::::::::");
printf ("\n\n");
printf ("\nIDENTIFICATION ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
printf ("\n\nIdentification module:  %s\n",file_name);

/*      Read Identification module data descriptive record (DDR)      */

if (! rd123ddrec 
        (fpin,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");

         goto done;
         }

status = -1;

/*       Loop to process each subfield in Identification module            */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fpin,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* string length */
        &status))       /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD (IDEN MODULE)");

         goto done;
         }

/*      Retrieve description of current subfield        */


 if (! chk123sfld 
        (fpin,          /* file pointer */
        tag,            /* tag output */
        descr,          /* subfield descriptions output */
        frmts))          /* subfield format control */
         {
         printf ("\nERROR CHECKING DATA RECORD SUBFIELD");

         goto done;
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

/*      Save the module name just read from the Identification module */

         strcpy (mod_name, string);

/*      Read next data record subfield    */

          if (! rd123sfld 
                 (fpin,          /* file pointer */
                 tag,            /* field tag returned */
                 &leadid,        /* leader identifier returned */
                 string,         /* subfield contents returned */
                 &str_len,       /* string length */
                 &status))       /* status returned */
                  {
                  printf ("\nERROR READING DATA RECORD SUBFIELD (RCID)");

                  goto done;
                  }

/*      Retrieve description of current subfield        */

          if (! chk123sfld 
                 (fpin,          /* file pointer */
                 tag,            /* tag output */
                 descr,          /* subfield descriptions output */
                 frmts))          /* subfield format control */
                  {
                  printf ("\nERROR CHECKING DATA RECORD SUBFIELD");
                  goto done;
                  }

/*      The subfield just read should contain the record ID for the
        Attribute Primary module.  If it doesn't, print error message.
        */

           if (strcmp(tag,"ATID") || strcmp (descr,"RCID"))
                 {
                 printf ("\nFile is inconsistent with profile requirements:");
                 printf ("\nSubfields of ATID field are not in correct order.");
                 printf ("\nForeign identifier %s %s will be skipped",
                         mod_name, string);
                 return;
                 }

           printf ("\nForeign ID:                        %s %s",mod_name,string);

                  dump_ap_mod();
         }


 } while (status != 4);   /* Break out of loop at end of file */

done:

/*       Close input Identification module           */

status = end123file (&fpin);

/*     Read statistics module        */

num_mod = -1;
read_stat_mod ();

/*     Read Catalog/Directory module        */

read_catd_mod ();

}


/******************************************************************************


  dump_ap_mod

  Function to dump a particular record of an attribute primary module.

******************************************************************************/

dump_ap_mod ()
{
int status;

/*      Set up file name for Attribute Primary module        */

strcpy (file_name, base_name);
strcat (file_name, mod_name);
strcat (file_name, ".DDF");

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

read_stat_mod ()

{

strcpy (file_name, base_name);

strcat (file_name, "STAT.DDF");
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

strcat (file_name, "CATD.DDF");
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
