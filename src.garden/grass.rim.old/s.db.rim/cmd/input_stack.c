#define INPUT_STACK

#include <stdio.h>
#include "globals.h"

FILE *push_input_stack(inpfile)
   FILE *inpfile;
{
   char temp[INP_SIZ];
   Input_nest_depth++;
   if (Input_nest_depth>=MAX_NEST_DEPTH) {
      sprintf(temp, "Input nested deeper than %d levels", MAX_NEST_DEPTH);
      G_fatal_error(temp);
   }

   Input_files[Input_nest_depth] = inpfile;
   return(inpfile);
}


/* this routine pops a file pointer off the input file stack and if successful
   the new file will be returned.  If the bottom of the input file stack has
   been reached it does nothing but return a NULL pointer */
FILE *pop_input_stack()
{
   if (Input_nest_depth<=0) return((FILE *) NULL);

   /* if inpfile is a non-tty file then close it */
   if (FP_ISATTY(Input_files[Input_nest_depth])==0)
      fclose(Input_files[Input_nest_depth]);

   Input_files[Input_nest_depth] = NULL;

   Input_nest_depth--;
   return (Input_files[Input_nest_depth]);
}



