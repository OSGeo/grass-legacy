/* This program will insert the new line characters in a
   Tiger file of any type, given as stdin, and write a modified
   version to stdout.  If the file already has a line feed at
   the end of the first record, no action is taken as it is
   assumed that the file has already been converted, and
   the program exits with a status of 1. This program will
   only work on Tiger files (Types 1-8 and A, I, P and R).
   The type of file is automatically detected by the program
   and the proper record length selected.
*/

#include <stdio.h>
#define MIN 300

main(argc,argv)
     int argc; char *argv[];
{
  char firstr[MIN+2];
  int c;
  int i,j,n,r;

  /* get first char to check record length */

  firstr[0] = c = getchar();
  if (c == EOF) unexpected_eof();
  r = rec_length((char) c);

  /* get the next r-1 characters */

  for (i=1; i<r; i++) {
    if ((c = getchar()) == EOF) unexpected_eof();
    firstr[i] = c;
  }

  /* get the next one */
  c = getchar();
  /* check it */
  if (c == '\n' || c == '\r') {
    fprintf(stderr,"File already has line feeds. No action.\n");
    exit(1);
  }
  else if (c == EOF) unexpected_eof();

  /* now process the first r, then the rest of the file */

  for (i=0; i<r; i++) putchar(firstr[i]);
  putchar('\n');

  /* do rest of file */
  do {
    putchar((char) c);
    for (i=1; i<r; i++) putchar(getchar());
    putchar('\n');
  }
  while ((c=getchar()) != EOF);

  exit(0);

}

rec_length(c) /* this returns the record length for each
                 Tiger File type */
char c;
{
int len;
switch (c) {
        case '1': len=228; break;
        case '2': len=208; break;
        case '3': len=111; break;
        case '4': len= 58; break;
        case '5': len= 52; break;
        case '6': len= 76; break;
        case '7': len= 74; break;
        case '8': len= 36; break;
        case 'A': len= 98; break;
        case 'I': len= 52; break;
        case 'P': len= 44; break;
        case 'R': len= 46; break;
        default : {fprintf(stderr,"Bad record type <%c>. Exiting.\n", c);
                   exit(2);
                  }
        }
return len;
}


unexpected_eof()
  {
    fprintf(stderr,"Unexpected end of file encountered.  Process aborted.\n");
    exit(3);
  }

