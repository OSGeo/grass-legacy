/************************************************************************/
/* NAME: 	read_header						*/
/*									*/
/* FUNCTION:	read record into memory and test eof			*/
/*									*/
/* USAGE:	read_header(ignore_eof)					*/
/* 									*/
/* INPUT:	"ignore_eof" a integer denotes ~eof			*/
/*									*/
/* OUTPUT:	"1" as success;	"-1" as error;				*/
/*		"0" as warning;						*/
/************************************************************************/
#include "tape.h"

/* read from 'tape.fd' into 'tape.headbuf' and returns eof? */

read_header(ignore_eof)
{
    if (tape.eof > 1)
	fprintf (stderr, "** WARNING: unexpected end-of-file on tape\n");
    tape.record_type = -1;
    if (tape.eof > 1) return 0;
    for(;;)
    {
      tape.n = read (tape.fd, tape.headbuf+1, HEADER_LENGTH);

      if (tape.n > 0 )
      {
	tape.eof = 0;
	if (tape.n == HEADER_LENGTH)
	  return 1;
	fprintf(stderr, "\nERROR reading tape: ");
	fprintf(stderr, "read %d chars, should be %d\n", tape.n, HEADER_LENGTH);
	return -1;
      }
      if (tape.n < 0)
      {
        fprintf (stderr, "\nERROR: end of tape encountered\n");
        return -1;
      }
      tape.eof++;
      if (!ignore_eof) break;
    }
    tape.record_type = -1;
    if (tape.eof > 1)
	fprintf (stderr, "\nWARNING: unexpected end-of-file on tape\n");
    return 0;
}
