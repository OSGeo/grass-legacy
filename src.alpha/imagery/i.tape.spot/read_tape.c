/********************************************************/
/* NAME:	read_tape				*/
/*							*/
/* FUNCTION:	read record into memory and test eof	*/
/*							*/
/* USAGE:	read_tape(ignore_eof)			*/
/*							*/
/* INPUT:	"ignore_eof" a integer denote ~eof	*/
/*							*/
/* OUTPUT:	"1" as success; "-1" as error;		*/
/*		"0" as warning;				*/
/********************************************************/
#include "tape.h"

/* read from 'tape.fd' into 'tape.buf' and returns eof?

   note: the tape documentation refers to bytes within the record
	 in fortran style (ie, starting with 1). Since C starts
	 with 0, the tape record is read into 'tape.buf' starting with
	 the second byte of 'tape.buf' (tape.buf[1]) so that all
	 code can be written to match the documentation.
*/

read_tape(ignore_eof)
{
    int n;

    if (tape.eof > 1)
	printf ("** WARNING: unexpected end-of-file on tape\n");
    tape.record_type = -1;
    if (tape.eof > 1) return 0;
    for(;;)
    {
	if((tape.n = read (tape.fd, tape.buf+1, sizeof(tape.buf)-1)) > 0)
	{
	    tape.eof = 0;
	    tape.record_type = record_type();
	    if (tape.n == (n = number (9,12)))
		return 1;
	    printf("** ERROR reading tape: ");
	    printf("read %d chars, should be %d\n", tape.n, n);
	    exit(-1);
	}
	if (tape.n < 0)
	{
	    printf ("** ERROR end of tape encountered\n");
	    exit(-1);
	}
	tape.eof++;
	if (!ignore_eof) break;
    }
    tape.record_type = -1;
    if (tape.eof > 1)
	printf ("** WARNING: unexpected end-of-file on tape\n");
    return 0;
}
