/*
*
* m.sdts.read - utility to read SDTS data in ISO 8211 format
*
* This program reads files in ISO 8211 (FIPS 123) format and dumps contents
* to screen and/or file.  Data is displayed record by record, showing
* names, sizes, and data types of individual record fields.
*
* It was originally written by Robert Lazar of the USGS, and as such is
* included with USGS's distribution of the FIPS 123 Function Library.  It
* was slightly modified by David Stigberg of USACERL to conform to GRASS
* program standards, essentially through the additon of the GRASS command-line
* parser.  The original title of the program, sdtsdump, has been changed to
* m.sdts.read.
*
* Usage:
*
*   m.sdts.read [-s] input=name  [output=name]

*   -s = suppress screen display; output to file only
*   input = name of (SDTS) ISO 8211-formatted file to be read
*   output = name of file to receive ascii-translated output
*
*
*/

/*
	Here is the original header of the program:



                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)
                                 FIPS 173


                          SDTS Browse Utility

                 program:         sdtsdump.c
                 author:          Bob Lazar
                 date:            June 14, 1994
                 language:        C

         This program dumps SDTS records interactively one at a time.  
         Output goes to both the computer screen/window
         and an optional report file on disk.  

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

                 CL /AL SDTSDUMP.C FIPS123.LIB

       To compile and link using the GNU C compiler on Data General
       workstations:

             gcc sdtsdump.c fips123.a -o sdtsdump

         Note that library names will vary depending upon the version of
         the software.

         Revisions
         =========

       02/15/93    Changes to make use of new functions g123order
                   and s123tol.
       06/14/94    SDTSDUMP created from SHOW173 -- file names are input
                   on command line instead of at prompt.

*/
#include <stdio.h>
#include <string.h>
#include "stc123.h"
#include "gis.h"
#define TRUE 1
#define FALSE 0


unsigned char temp[100]; /* temporary buffer used for hex dumps of binary */
long str_len;            /* byte length of subfield returned by rd123sfld */
long int_level;          /* interchange level returned by beg123file */              
long li;      /* long integer used for binary dumps */
int i;                   /* loop counter */
int status;              /* status returned by FIPS library routines */
int interactive;         /* TRUE if "interactive" mode:  output to screen */
char junk[101],junk1[2]; /* user input after subfield displaced */
char ice;                /* inline code extension from beg123file */
char leadid;             /* leader ID returned by rd123sfld */
char ccs[4];             /* character set string returned by beg123file */
char tag[10];            /* field tag returned by rd123sfld and chk123sfld */
char file_name[200];     /* input ISO 8211 file name */
char out_file[200];      /* output report file name */
char string[5000];       /* subfield data returned by rd123sfld */
char descr[100];        /* subfield description (mnemonic) returned by chk123sfld */
char frmts[100];         /* subfield format returned by chk123sfld */
FILE *fpin;              /* input ISO 8211 file pointer */
FILE *fpout;             /* output report file pointer */
int order;         /* byte order returned by g123order */
int have_outfile;  /*TRUE if user specifies output file*/

main(argc,argv)
  int argc;
  char *argv[];
{

	 struct Option *in, *out;
	 struct Flag *s_flag;

     
     G_gisinit (argv[0]);

	 in = G_define_option();
	 in->key		= "input";
	 in->type		= TYPE_STRING;
	 in->required	= YES;
	 in->multiple	= NO;
	 in->description = "SDTS/ISO 8211 filename";

	 out = G_define_option();
	 out->key		= "output";
	 out->type		= TYPE_STRING;
	 out->required	= NO;
	 out->multiple	= NO;
	 out->description = "output filename";

	 s_flag = G_define_flag ();
	 s_flag->key		= 's';
	 s_flag->description = "suppress screen display, dump to file only";

	 if (G_parser (argc, argv))
		 exit (-1);


/*     Print error if arguments were not included on command line        */

/* not needed
if (argc < 2)
  {
  printf ("\nsdtsdump <input_file> {output_file}");
  printf ("\n   input file:  ISO 8211 file");
  printf ("\n  output file:  optional report file");
  exit(0);
  }
*/

/*    Determine byte order of current machine       */

g123order (&order);

/*       Start in interactive mode, unless explicity not wanted         */

    if (s_flag->answer)
		interactive = FALSE;
    else
        interactive = TRUE;

/*      Open input file         */

/*
strcpy (file_name,argv[1]);
*/

strcpy (file_name,in->answer);

if (! beg123file (file_name,'R',&int_level,&ice,ccs,&fpin))
         {
         printf ("\nERROR OPENING FILE %s",file_name);
         exit(0);
         }

/*     Display parameters returned by beg123file       */

printf ("\nSDTS file %s opened for read ...",file_name);

/*       Open output report file           */

if (out->answer)
/*
if (argc >= 3)
*/
{
  have_outfile = TRUE;
  strcpy (out_file,out->answer);
  /*
  strcpy (out_file,argv[2]);
  */
  fpout = fopen (out_file,"w");
  if (fpout == NULL)
         {
         printf ("\nERROR OPENING FILE %s",out_file);
         goto done2;
         }

	fprintf (fpout,"\n\nSDTS Report\n\n");
	fprintf (fpout,"\nInput SDTS file:  %s\n",file_name);

	fprintf (fpout,"\nField Subfield Format Length Data");
	fprintf (fpout,"\n----- -------- ------ ------ ----");
}
else
  have_outfile = FALSE;

if (interactive)
{
	printf ("\n\nField Subfield Format Length Data");
	printf ("\n----- -------- ------ ------ ----");
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

    if (!interactive && !have_outfile)
	   goto done;

/*      Read data record subfield    */

 if (! rd123sfld 
        (fpin,          /* file pointer */
        tag,            /* field tag returned */
        &leadid,        /* leader identifier returned */
        string,         /* subfield contents returned */
        &str_len,       /* string length */
        &status))       /* status returned */
         {
		 if (interactive)
         printf ("\nERROR READING DATA RECORD SUBFIELD");
         if (have_outfile)
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
		 if (interactive)
         printf ("\nERROR CHECKING DATA RECORD SUBFIELD");
         if (have_outfile)
            fprintf (fpout,"\nERROR CHECKING DATA RECORD SUBFIELD");
         goto done;
         }

/*     Determine if format is binary         */


  if (strstr (frmts, "B") != NULL)
       {
       memcpy (temp, string, str_len);

/*      If binary print out value in hexadecimal        */

        if (interactive) printf 
              ("\n%-5s %-8s %-6s %6ld Binary: ",
              tag,descr,frmts,str_len);
        if (have_outfile)
		fprintf (fpout,"\n%-5s %-8s %-6s %6ld Binary: ",
              tag,descr,frmts,str_len);
        for (i=0; i<str_len; i++)
                {
                if (interactive) printf ("%2.2x",temp[i]);
                if (have_outfile) fprintf (fpout,"%2.2x",temp[i]);
                }
       if (interactive) printf (" (hex)");
       if (have_outfile) fprintf (fpout," (hex)");

/*     The Topological Vector Profile (TVP), the first profile developed
       to support SDTS, requires that spatial addresses (coordinates) be
       encoded as 32-bit signed integers in "two's complement" format.
       If a 32-bit integer is encountered, also output the value in
       decimal.  (The ISO 8211 format does not provide interpretation
       of the binary format:  the binary format is restricted by the
       profile to this one option, the binary format can also be
       determined by the Internal Spatial Reference module.)        */
       
       if (str_len == 4)
               {

/*     If little endian, switch byte order within long word        
       (SDTS requires "big endian" representation           */

if (!order)
      {
      s123tol (string, &li, 1);
      }
else
      {
      s123tol (string, &li, 0);
      }

               if (interactive) printf (" %12ld (decimal)",li);
               if (have_outfile) fprintf (fpout," %12ld (decimal)",li);
              }
       }
  else
       {
         
/*      Write out subfield data and description to screen
         if in interactive mode   */

       if (interactive) printf ("\n%-5s %-8s %-6s %6ld %s",
              tag,descr,frmts,str_len,string);
  
/*      Write out subfield data and description to report file      */

       if (have_outfile)
         fprintf (fpout,"\n%-5s %-8s %-6s %6ld %s",tag,descr,frmts,str_len,string);
       }

/*       Output record/end of file delimeters                */

 if (status == 3)   /* subfield is at end of record */
        {
        if (interactive) printf 
                 ("\n##### End of Record ##################################");
        if (have_outfile) fprintf (fpout,
                "\n##### End of Record ##################################");
        }

 else if (status == 4)   /* subfield is at end of file */
        {
        if (interactive)
         {
         printf ("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
         printf ("\n$$$$$ END OF FILE $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
         printf ("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
         }
	if (have_outfile)
  	 {
         fprintf (fpout,"\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
         fprintf (fpout,"\n$$$$$ END OF FILE $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
         fprintf (fpout,"\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
	 }
        }

/*       Prompt for next record if interactive and not end of file   */

          if (interactive && status == 3)
            {
              printf ("\nCommands:      b       - switch to batch mode");
              printf ("\n               q       - quit program");
              printf ("\n               <enter> - display next record");
              printf ("\nEnter command: ");

            fgets (junk,100,stdin);
            strncpy (junk1,junk,1);
            if (!strcmp (junk1,"q")) goto done;

/*          If user types in 'b', switch to non-interactive mode and
            send remainder of file to disk file only.       */

            if (!strcmp (junk1,"b")) interactive = FALSE;
            }

 } while (status != 4);   /* Break out of loop at end of file */

done:

/*       Close files and end      */

if (have_outfile) fclose (fpout);

done2:

/*       Close input ISO 8211 file        */

status = end123file (&fpin);
}
