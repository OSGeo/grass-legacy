/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)

            Attribute Primary Module Decoding / Dump Utility

                 program:         attrdump.c
                 author:          Bob Lazar
                 date:            June 9, 1992
                 language:        C

       This program dumps an attribute primary module to an ASCII file.

*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#define TRUE 1
#define FALSE 0

FILE *fpin;
FILE *fpout;
long int_level;
long natt, record_id, att_record_id;
long attr_record;
long str_len;
int stat2;
int status;
int interactive;
int i,j;
int process;
char module_name[5], att_module[5];
char junk[100],junk1[2];
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
char photorevised[10];
char feature_type[10];

main()
{

/*      Prompt for input SDTS file name      */

printf ("\n\n  Spatial Data Transfer Standard (SDTS)");
printf ("\n\n      SDTS Attribute Dump Utility");
printf ("\n\nEnter input SDTS file name: ");
scanf ("%s%*1c",file_name);

/*      Open input file         */

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

printf ("\nEnter output dump file name: ");
scanf ("%s%*1c",out_file);

fpout = fopen (out_file,"w");

/*      Read data descriptive record (DDR)      */

if (! rd123ddrec 
        (fpin,          /* file pointer */
        string,         /* DDR record returned */
        &status))       /* status returned */
         {
         printf ("\n*** ERROR READING DDR ***");
         goto done;
         }

status = -1;
natt = 0;       /* number of attributes */
process = FALSE;

/*       Loop to process each subfield             */

do {

/*      Read data record subfield    */

 if (! rd123sfld 
        (fpin,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,         /* length of subfield */
        &status))        /* status returned */
         {
         printf ("\nERROR READING DATA RECORD SUBFIELD");
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

       if (!strcmp(tag,"ATPR"))
       {
       if ( !strcmp(descr,"MODN"))
         {
              strcpy (module_name,string);
         }
       else if (!strcmp(descr,"RCID"))
         {
              record_id = atol (string);
	fprintf (fpout,"%s",string);
         }
       }
       else if (!strcmp(tag,"ATTP"))
       {
       fprintf (fpout,
       ",%s",string);
       }

/*	If end of record, write newline		*/

if (status == 3) fprintf (fpout,"\n");
 } while (status != 4);   /* Break out of loop at end of file */

fprintf (fpout,"\nEND");
done:

/*       Close files and end      */

fclose(fpout);
done2:
stat2 = end123file (&fpin);
}
