/*********************************************************************

NAME:		read_tape

FUNCTION:       reads the information at the start of LANDSAT tapes.
		Some interesting and useful information in the records
		preceeding data records is printed along the way.

USAGE:		read_tape(tape, buf, full)
		int tape
		unsigned char buf[]
		int full

RETURNS:	(via externals)
		values of the data record length, the type of LANDSAT
		tape, whether or not corrections have been done, the
		tape volume number, and the number of bands available.

**********************************************************************/
#include "tape.h"

read_tape(tape, buf, full)
    int tape ;
    unsigned char buf[] ;
    int full;
{
    int n_read ;

    while(1)
    {
	n_read = read(tape, buf, REC_SIZE) ;
	if (n_read == 0)
	    continue ;

	switch(buf[5])
	{
	case TAPE_DIR:
		vol = tape_vol (buf[18]);
		if (full) tape_dir(buf) ;
		break ;
	case HEADER:
		if (full) header(buf) ;
		break ;
	case ANCILLARY:
		break ;
	case ANNOTATION:
		if (full) annotation(buf) ;
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
