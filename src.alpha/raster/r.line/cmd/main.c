/* Raster-file line extraction */

/* Mike Baba */
/* DBA Systems*/
/* Farfax, VA */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* March 1988 */

#include <stdio.h>
#include "gis.h"
#include "extr_lines.h"

main(argc,argv)
int argc;
char *argv[];
{
  char input[40], output[40];
  FILE *fopen();

  G_gisinit(argv[0]);

  syntax(argc,argv,input,output) ;

  fprintf(stdout,"Opening files\n");
  open_file(input,output);
  fprintf(stdout,"Performing extraction\n");
  extract_lines();
  fprintf(stdout,"Closing files\n");
  close_file();
  exit(0);
}

