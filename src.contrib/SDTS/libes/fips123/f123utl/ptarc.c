/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)

                 Point Module to Arc/Info Generate Compatible File Utility

                 program:         ptarc.c
                 author:          Bob Lazar
                 date:            June 29, 1992
                 language:        C

         This program dumps an SDTS point module to a data file which is
         compatible with the Arc/Info GENERATE command.  Also created
         is a file for input to the INFO READ FROM command with 
         point record # and attribute record #.

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

                 CL /AL PTARC.C FIPS123.LIB

         Revisions
         =========

09/14/92  Added capability to read binary spatial addresses
          (hard coded for DLG-3 dataset.)

03/03/93  Modified to make use of g123order and s123tol routines.
          Modified to output GT-polygon record ID for area points instead of
          attribute record ID (due to change in profile which
          places links to attributes on GT-polygons rather than
          area points.)

*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#define TRUE 1
#define FALSE 0
FILE *fpin;
FILE *fpout;
FILE *fpinfo;
long int_level;
double sfax, sfay, xorg, yorg; /* internal spatial reference parameters */
double xtemp, ytemp;
long record_id;
long a_record_id;
long str_len;
long li;
int stat2;
int status;
int i;
char ice;
char leadid;
char ccs[4];
char object_rep[10];
char tag[10];
char fdlen[10];
char *fdname;
char file_name[100];
char out_file[100];
char info_file[100];
char string[5000];
char descr[5000];
char frmts[500];
int order;              /* byte order returned by g123order */
short natt;

main()
{
/*      Determine byte order of current machine         */

g123order (&order);

/*      Hard code scaling and translation factors for DLG-3 dataset.
        To be able to read any data set, these should be read from the
        Internal Spatial Reference module.     */

sfax = 0.01;
sfay = 0.01;
xorg = 0.0;
yorg = 0.0;

natt = 0;        /* initialize number of attributes on object        */

/*      Prompt for input SDTS file name      */

printf ("\n\n  Spatial Data Transfer Standard (SDTS)");
printf ("\n\n  SDTS Line Module to Arc/Info Utility");
printf ("\n\nEnter input SDTS file name: ");
scanf ("%s%*1c",file_name);

/*      Open input file         */

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Prompt for output report file name      */

printf ("\nEnter output ARC generate file name: ");
scanf ("%s%*1c",out_file);

/*       Open output report file           */

fpout = fopen (out_file,"w");
if (fpout == NULL)
         {
         printf ("\nERROR OPENING FILE %s",out_file);
         goto done2;
         }

/*      Prompt for output attribute data file name      */

printf ("\nEnter output INFO data file name: ");
scanf ("%s%*1c",info_file);

/*       Open output INFO input file           */

fpinfo = fopen (info_file,"w");
if (fpout == NULL)
         {
         printf ("\nERROR OPENING FILE %s",out_file);
         goto done2;
         }


/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fpin,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");
         fprintf (fpout,"\n*** ERROR READING DDR ***");
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
         fprintf (fpout,"\nERROR READING DATA RECORD SUBFIELD");
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
         fprintf (fpout,"\nERROR CHECKING DATA RECORD SUBFIELD");
         goto done;
         }

/*       Process based on field and subfield tags           */

   if ( !strcmp(tag,"PNTS") && !strcmp(descr,"RCID"))
         {
         record_id = atol (string);
         }

/*      Save object representation code          */

   else if (!strcmp(tag,"PNTS") && !strcmp(descr,"OBRP"))
           {
           strcpy (object_rep, string);
           }

/*      If area point, get GT-polygon foreign ID         */

   else if ( !strcmp(tag,"ARID") && !strcmp(descr,"RCID")
                && !strcmp(object_rep,"NA"))
         {
              if (natt == 0)
                 {
                 a_record_id = atol (string);
                 natt++;
                 }
         }

/*      If not area point, get Attribute Primary foreign ID       */

   else if ( !strcmp(tag,"ATID") && !strcmp(descr,"!RCID")
                && strcmp(object_rep,"NA"))
         {
              if (natt == 0)
                 {
                 a_record_id = atol (string);
                 natt++;
                 }
         }

/*     Note:  hard coded for DLG-3 dataset resolution,
       convert to decimal degrees        */

    else if ( !strcmp(tag,"SADR"))
         {
 
/*      Binary data     */

         if (strstr (frmts,"B") != NULL)
            {
             if (!order)
                s123tol (string, &li, 1);
             else
                s123tol (string, &li, 0);
             }

/*      ASCII data      */

          else
             li = atol (string);

/*      Process if X spatial address    */

          if (!strcmp (descr, "X"))
                {
                xtemp  = ((double) li * sfax) + xorg;
                }

/*      Process if Y spatial address    */

          else if (!strcmp (descr, "Y"))
                {
                ytemp  = ((double) li * sfay) + yorg;
                }
          }


/*       If end of record, write out record and reinitialize        */

 if (status == 3 || status == 4)
   {
     fprintf (fpout,"%ld,",record_id);
     fprintf (fpout,"%f,%f\n",xtemp,ytemp);
     
     if (natt > 0) 
            fprintf (fpinfo,"%ld,%ld\n",record_id,a_record_id);
         natt = 0;
   }
 } while (status != 4);   /* Break out of loop at end of file */

done:

/*       Close files and end      */

fprintf (fpout,"END\n");
fprintf (fpinfo,"END\n");
fclose (fpinfo);
fclose (fpout);

done2:
stat2 = end123file (&fpin);
}
