/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)

                 Line Module Decoding / Dump Utility

                 program:         linedump.c
                 author:          Bob Lazar
                 date:            June 9, 1992
                 language:        C

         This program dumps an SDTS line module to an ASCII data file.

         This program is an example which uses the FIPS 123 Function Library.  
         This is a public domain software library which supports the SDTS, 
         FIPS 173, an exchange standard for digital spatial data which uses 
         ANSI/ISO 8211 (FIPS 123) for its physical file implementation.

         The FIPS 123 Function Library has been developed on the MS/DOS
         and Data General AViiON Unix platforms.  It has been developed
         using the ANSI standard C language.  It has not been fully
         tested on other platforms.

         To obtain this library or obtain additional information on the
         SDTS, contact the following address:

              SDTS Request
              U.S. Geological Survey
              526 National Center
              Reston, VA   22092

              Fax:    (703) 648-5542
              Internet: sdts@usgs.gov

         To compile and link under DOS using Microsoft C:

                 CL /AL LINEDUMP.C DOS_0293.LIB

    To compile and link using the GNU C compiler on Data General
    workstations:

          gcc linedump.c dg_0293.a -o linedump

         Note that library names will vary depending upon the version of
         the software.

         Revisions
         =========

*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#define TRUE 1
#define FALSE 0

FILE *fpin;
FILE *fpout;
long int_level;
long li;
long x[10000];
long y[10000];
long natt, nxy, record_id, att_record_id;
long str_len;
int stat2;
int status;
int i;
char module_name[5], att_module[5];
char ice;
char leadid;
char ccs[4];
char tag[10];
char fdlen[10];
char *fdname;
char file_name[100];
char out_file[100];
char string[5000];
char descr[5000];
char frmts[500];
int order;              /* byte order returned by g123order */

main()
{
/*      Determine byte order of current machine         */

g123order (&order);

/*      Prompt for input SDTS file name      */

printf ("\n\n  Spatial Data Transfer Standard (SDTS)");
printf ("\n\n  SDTS Line Module Decode/Dump Utility");
printf ("\n\nEnter input SDTS file name: ");
scanf ("%s%*1c",file_name);

/*      Open input file         */

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*      Prompt for output report file name      */

printf ("\nEnter output dump file name: ");
scanf ("%s%*1c",out_file);

/*       Open output report file           */

fpout = fopen (out_file,"w");
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
natt = 0;       /* number of attributes */
nxy = 0;         /* number of coordinate pairs */

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

  if ( !strcmp(tag,"LINE") && !strcmp(descr,"MODN"))
         {
         strcpy (module_name,string);
         }
   else if ( !strcmp(tag,"LINE") && !strcmp(descr,"RCID"))
         {
         record_id = atol (string);
         }
   else if ( !strcmp(tag,"ATID") && !strcmp(descr,"!MODN"))
         {
         if (natt == 0) strcpy (att_module,string);
         }
   else if ( !strcmp(tag,"ATID") && !strcmp(descr,"!RCID"))
         {
         if (natt == 0)
           {
           att_record_id = atol (string);
           natt++;
           }
         }
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

          if (!strcmp (descr, "!X"))
                {
                x[nxy] = li;
                }

/*      Process if Y spatial address    */

          else if (!strcmp (descr, "!Y"))
                {
                y[nxy] = li;
                nxy++;
                }
         }

/*       If end of record, write out record and reinitialize        */

 if (status == 3 || status == 4)
   {
   fprintf (fpout,"%5s%11ld%11ld%11ld\n",module_name,record_id,natt,nxy);
   if (natt > 0)
         fprintf (fpout,"%5s%11ld\n",att_module,att_record_id);
   for (i=0; i<nxy; i++)
         fprintf (fpout,"%11ld%11ld\n",x[i],y[i]);
   natt = 0;
   nxy = 0;
   }
 } while (status != 4);   /* Break out of loop at end of file */

done:

/*       Close files and end      */

fclose (fpout);

done2:
stat2 = end123file (&fpin);
}
