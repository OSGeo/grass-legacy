#define INPUT_FILE

#include <stdio.h>
#include "globals.h"
#include "gis.h"

FILE *push_input_stack();

/* This routine pushes the given input file name onto the input stack (or
   if no filename is given it pushes the stdin) and  redirects the input
   file pointer Infile to read from the new file.  An error will be issued
   by the push_input_stack() routine if there are already 8 files in the
   input stack. */

input_file(inp_buf)
{
   int pos, need_echo;
   FILE *file;
   char buffer[INP_SIZ], new_name[INP_SIZ];

   /* if Infile is a tty and Outfile is not we need to echo output */
   if (FP_ISATTY(Infile)!=0 && FP_ISATTY(Outfile)==0) need_echo = TRUE;

   /* initialize the directory string */
   strcpy(new_name, "");

   /* get a copy of the input line and squeeze unnecessary white space out*/
   strcpy(buffer, inp_buf);
   G_squeeze(buffer);

   /* find the second word on the input line */
   pos = strcspn(buffer, " ");

   /* if at the end of the line (no file name provided), change to stdin*/
   if (pos==strlen(buffer)) {
      fprintf(Outfile, "\nInput file is changing to standard input.\n");
      if (need_echo==TRUE)
         printf("\nInput file is changing to standard input.\n");
      Infile = push_input_stack(stdin);
   }
   /* otherwise change to the provided filename */
   else {
      strcat(new_name, buffer+pos+1);
      if ((file = fopen(new_name, "r")) == NULL) {
         fprintf(Outfile, "\nWarning: Unable to open new input file '%s'.",
                  new_name);
         fprintf(Outfile, "\nWarning: Input file is unchanged.\n");
         if (need_echo==TRUE) {
            printf("\nWarning: Unable to open new input file '%s'.",
                     new_name);
            printf("\nWarning: Input file is unchanged.\n");
         }
      }
      else {
         fprintf(Outfile, "\nInput file is changing to file '%s'.\n",
                  new_name);
         if (need_echo==TRUE)
            printf("\nInput file is changing to file '%s'.\n", new_name);

         Infile = push_input_stack(file);
      }
   }
}




