/*
                           U.S. Geological Survey
                          National Mapping Division

                    Spatial Data Transfer Standard (SDTS)
                                  FIPS 173

                           ISO 8211 Dump Utility

                 program:         show8211.c
                 author:          Bob Lazar
                 date:            December 1, 1992
                 language:        C


This program reads an input ISO 8211 file and produces an output file which
is reformatted for easier interpretation.  This program is designed for use
by those familiar with the ISO 8211 file structure.

All field and unit terminators are converted to printable characters ; and & 
respectively.

A new line character is inserted after all field terminators.

New line characters are inserted when lines are greater than 70 characters.

In "ASCII" mode, all non-printable ASCII characters (less than 0x20 or greater 
than 0x7e) are output as "@" characters.  In "mixed" mode, non-printable
characters will be displayed with their hexadecimal value.  In "hex"
mode all characters are displayed with their hex values.

The program does not interpret the data using the ISO 8211 format control.  
Binary data will sometimes correspond to printable ASCII characters and be 
output as ASCII characters, in cases where it does not correspond to printable 
characters it will be output as "@" characters or hexadecimal values.



*/

#include <stdio.h>
#include <string.h>

FILE *fpin;              /* input file pointer */
short  count;            /* number of characters per line */
int c;         /* character read from ISO 8211 file */

main(argc,argv)
  int argc;
  char *argv[];
{

/*     Print error if arguments were not included on command line        */
       

if (argc < 3)
  {
  printf ("\nshow8211 <display_format> <input_file>");
  printf ("\n   display format:  a - ASCII");
  printf ("\n                    h - hex");
  printf ("\n                    m - mixed");
  printf ("\n   input file:  ISO 8211 file");
  exit();
  }
else
  {


  if (!strcmp (argv[1],"a") && !strcmp (argv[1],"h")
       && !strcmp (argv[1],"m"))
       {
         printf ("\nshow8211 <output_format> <input_file>");
         printf ("\nISO 8211 file display command");
         printf ("\n   display format:  a - ASCII");
         printf ("\n                    h - hex");
         printf ("\n                    m - mixed");
         printf ("\n   input file:  ISO 8211 file");
         exit();
       }
  }

/*      Open input file         */

fpin = fopen (argv[2],"rb");
if (fpin == NULL)
         {
         printf ("\nERROR OPENING FILE %s",argv[2]);
         exit(0);
         }

/*     Read each character from input file           */

c = fgetc (fpin);

while (! feof(fpin))
  {

/*      If hex mode, output hex value for all characters        */

  if (!strcmp (argv[1],"h"))
  {
         check_newline();
         printf ("%2.2x ", c);
         count = count + 3;
         if (c == 30)
         {
           putchar ('\n');
           count = 0;
         }
   }
   else

/*     ASCII or mixed mode        */

   {
/*      If character is field terminator, change to printable character
        and add new line character     */

  if (c == 30)
        {
        putchar (';');
        putchar ('\n');
        count = 0;
        }

/*      If character is unit terminator, change to printable character*/

   else if (c == 31)
        {
        check_newline();
        putchar ('&');
        count++;
        if (!strcmp (argv[1],"m"))
         {
         putchar (' ');
         putchar (' ');
         count = count+2;
         }
        }

/*     If character is other non-printing character, output hex value         */

   else if (c < 0x20 || c > 0x7f)
       {
        if ( !strcmp (argv[1],"m"))
         {
              check_newline();
              printf ("%2.2x ", c);
              count = count + 3;
         }
        else

/*     For brief format, output '@' for all non-printing characters         */

         {
         check_newline();
         putchar ('@');
         count++;
         }
       }

/*     All other situations, output the same character as the ISO 8211
       file   */

   else
        {

        check_newline();
        putchar (c);
        count++;
        if (! strcmp (argv[1],"m"))
         {
         putchar (' ');
         putchar (' ');
         count = count+2;
         }
        }
   }
   c = fgetc (fpin);

  }
printf ("\nEnd of File");

/*       Close file and end      */


done2:
fclose (fpin);

}

/*     Function to test if the line length is over 70 characters,
       if so add a newline        */

check_newline()
{
  if (count > 70)
       {
       putchar ('\n');
       count = 0;
       }
}
