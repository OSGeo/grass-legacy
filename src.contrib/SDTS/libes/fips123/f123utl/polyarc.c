/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)

                 Polygon Module to Info Compatible File Utility

                 program:         polyarc.c
                 author:          Bob Lazar
                 date:            March 4, 1993
                 language:        C

         This program dumps an SDTS polygon module to a data file which is
         compatible with the INFO READ FROM command with 
         polygon record # and attribute record #.

         This program is an example which uses the FIPS 123 Function Library.  
         This is a public domain software library which supports the SDTS, 
         an exchange standard for digital spatial data which uses ANSI/ISO 8211 
         (FIPS 123) as its implementation.

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

                 CL /AL POLYARC.C FIPS123.LIB

         Revisions
         =========


*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#define TRUE 1
#define FALSE 0
FILE *fpin;
FILE *fpinfo;
long int_level;
long record_id;
long a_record_id;
long str_len;

int stat2;
int status;
int i;
char ice;
char leadid;
char ccs[4];
char tag[10];
char fdlen[10];
char *fdname;
char file_name[100];
char info_file[100];
char string[5000];
char descr[5000];
char frmts[500];


main()
{

/*      Prompt for input SDTS file name      */

printf ("\n\n  Spatial Data Transfer Standard (SDTS)");
printf ("\n\n  SDTS Polygon Module to Arc/Info Utility");
printf ("\n\nEnter input SDTS file name: ");
scanf ("%s%*1c",file_name);

/*      Open input file         */

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Prompt for output attribute data file name      */

printf ("\nEnter output INFO data file name: ");
scanf ("%s%*1c",info_file);

/*       Open output INFO input file           */

fpinfo = fopen (info_file,"w");
if (fpinfo == NULL)
         {
         printf ("\nERROR OPENING FILE %s",info_file);
         goto done2;
         }


/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fpin,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");
         fprintf (fpinfo,"\n*** ERROR READING DDR ***");
         goto done;
         }

status = -1;

/*       Loop to process each subfield             */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fpin,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* subfield length */
        &status))        /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD");
         fprintf (fpinfo,"\nERROR READING DATA RECORD SUBFIELD");
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
         fprintf (fpinfo,"\nERROR CHECKING DATA RECORD SUBFIELD");
         goto done;
         }

/*       Process based on field and subfield tags           */

   if ( !strcmp(tag,"POLY") && !strcmp(descr,"RCID"))
         {
         record_id = atol (string);
         }

/*      Get Attribute Primary foreign ID       */

   else if ( !strcmp(tag,"ATID") && !strcmp(descr,"!RCID"))
         {
           a_record_id = atol (string);
         }

/*       If end of record, write out record and reinitialize        */

 if (status == 3 || status == 4)
   {
     fprintf (fpinfo,"%ld,%ld\n",record_id,a_record_id);

   }
 } while (status != 4);   /* Break out of loop at end of file */

done:

/*       Close files and end      */


fprintf (fpinfo,"END\n");
fclose (fpinfo);


done2:
stat2 = end123file (&fpin);
}
