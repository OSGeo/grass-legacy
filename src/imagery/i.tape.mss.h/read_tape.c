/*********************************************************************

NAME:		read_tape

FUNCTION:       reads the information at the start of LANDSAT tapes.
		Some interesting and useful information in the records
		preceeding data records is printed along the way.

USAGE:		read_tape(tape, buf)
		int tape
		unsigned char buf[]

RETURNS:	(via externals)
		values of the data record length, the type of LANDSAT
		tape, whether or not corrections have been done, the
		tape volume number, and the number of bands available.

**********************************************************************/
#include "tape.h"

read_tape(tape, buf)
    int tape ;
    unsigned char buf[] ;
{
    int n_read ;

    while(1)
    {
	n_read = read(tape, buf, REC_SIZE) ;
	if (n_read == 0)
	    continue ;

	record_type (buf[5]);

	switch(buf[5])
	{
	case TAPE_DIR:
		tape_dir(buf) ;
		break ;
	case HEADER:
		header(buf) ;
		break ;
	case ANCILLARY:
		break ;
	case ANNOTATION:
		annotation(buf) ;
		break ;
	case IMAGE:
		return(HAVE_IMAGE) ;
	case TRAILER:
		break ;
	default:
		return(NO_HEADER) ;
	}
    }
}
