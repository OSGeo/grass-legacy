/* Cell-file line extraction */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* March 1988 */

#include <stdio.h>
#include "gis.h"
#include "extr_lines.h"

FILE *debug, *mem;

main(argc,argv)
int argc;
char *argv[];
{
  char *input, *output;
  FILE *fopen();

  if (syntax(argc,argv,&input,&output))
  {
    fprintf(stderr,"Usage:  %s [-ab] cell_file digit_file\n",argv[0]);
    exit(-1);
  }
  debug = fopen("/dev/null","w");
  setbuf(debug,0);
  mem = fopen("/dev/null","w");
  setbuf(mem,0);
  G_gisinit("LINE_EXTRACTION");
  fprintf(stdout,"Opening files\n");
  open_file(input,output);
  fprintf(stdout,"Performing extraction\n");
  extract_lines();
  fprintf(stdout,"Closing files\n");
  close_file();
  fclose(debug);
  exit(0);
}

