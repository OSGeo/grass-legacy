#define OUTPUT_FILE

#include <stdio.h>
#include "globals.h"
#include "gis.h"


output_file(inp_buf)
char *inp_buf;
{
  static int is_pipe=0;
  int pos, need_echo;
  FILE *file;
  char buffer[INP_SIZ], *p;

  /* if Infile is a tty and Outfile is not we need to echo output */
  if (Infile==NULL) need_echo = FALSE;
  else if (FP_ISATTY(Infile)!=0 && FP_ISATTY(Outfile)==0) need_echo = TRUE;

  /* get a copy of the input line and squeeze unnecessary white space out*/
  strcpy(buffer, inp_buf);
  G_squeeze(buffer);

  /* find the second word on the input line */
  pos = strcspn(buffer, " ");
  p = buffer+pos+1;

  /* if at the end of the line (no file name provided), change to stdout*/
  if (pos==strlen(buffer)) {
    if (Outfile != stdout)
      fprintf(Outfile, "\nOutput file is changing to standard output.\n");
    if (need_echo==TRUE)
      printf("\nOutput file is changing to standard output.\n");
    /* if the Outfile is not a tty then close it */
    if (FP_ISATTY(Outfile)==0 && Outfile != stdout)
      if (is_pipe) pclose(Outfile);
      else fclose(Outfile);
    is_pipe =0;
    Outfile = stdout;
  }
  /* otherwise change to the provided filename */
  else {
    /* handle pipe possibility first */
    if (*p == '|') {
      if ((file=popen(p+1,"w")) == NULL) {
        fprintf(Outfile, "\nWarning: Unable to open process '%s'.", p);
        fprintf(Outfile, "\nWarning: Output destination is unchanged.\n");
        if (need_echo==TRUE) {
          printf("\nWarning: Unable to open new process '%s'.", p);
          printf("\nWarning: Output destination is unchanged.\n");
        }
      }
      else {
        fprintf(Outfile, "\nOutput is changing to process '%s'.\n", p);
        if (need_echo==TRUE)
          printf("\nOutput is changing to process '%s'.\n", p);

        /* if the Outfile is not a tty then close it */
        if (FP_ISATTY(Outfile)==0 && Outfile != stdout)
          if (is_pipe) pclose(Outfile);
          else fclose(Outfile);
        is_pipe = 1;
        Outfile = file;
        return;
      }
    }

    if ((file = fopen(p, "w")) == NULL) {
      fprintf(Outfile, "\nWarning: Unable to open new output file '%s'.", p);
      fprintf(Outfile, "\nWarning: Output file is unchanged.\n");
      if (need_echo==TRUE) {
        printf("\nWarning: Unable to open new output file '%s'.", p);
        printf("\nWarning: Output file is unchanged.\n");
      }
    }
    else {
                p = buffer+pos+1;
      fprintf(Outfile, "\nOutput file is changing to file '%s'.\n",
              p);
      if (need_echo==TRUE)
        printf("\nOutput file is changing to file '%s'.\n", p);

      /* if the Outfile is not a tty then close it */
      if (FP_ISATTY(Outfile)==0 && Outfile != stdout)
        if (is_pipe) pclose(Outfile);
        else fclose(Outfile);
      is_pipe = 0;
      Outfile = file;
    }
  }
}
