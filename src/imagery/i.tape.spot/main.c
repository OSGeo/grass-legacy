/****************************************************************/
/* NAME:		i.tape.spot				*/
/*								*/
/* FUNCTION:		allows user to extract a window off of  */
/*			a set of SPOT tape			*/
/*								*/
/* USAGE:		i.tape.spot				*/
/*								*/
/* INPUT FILE:		tape device				*/
/*								*/
/* OUTPUT FILES:	raw band cell and supporting files	*/
/****************************************************************/
#define GLOBAL
#include "tape.h"

main (argc,argv) char *argv[];
{
    G_gisinit(argv[0]);

    I_must_be_imagery_projection();
    G_want_histogram(1);

    I_clear_tape_info (&tape.info);

    get_tapename(tape.name);
    tape.fd = -1;

/* read volume descriptor from first tape */

    I_ask("mount SPOT tape and hit RETURN-->", 0, 1);
    mount_tape ();
    while (!(read_tape(0) && tape.record_type == VOLUME_DESCRIPTOR))
    {
	unmount_tape();
	I_ask("mount SPOT tape and hit RETURN-->", 0, 1);
	mount_tape ();
    }

/* read volume descriptor file */

    header(0); 

/* read header file for calibration parameters */

    	para_read();
	I_tape_advance(tape.fd,-999);   

/* band sequential or band interleave? */

    switch (tape.interleaving) {

    case BSQ:	bsq();
		break;

    case BIL:	bil();
		break;

    default:	printf(" unknown interleaving type\n");
		exit(0);
    }

/* extraction done */

    unmount_tape();
    exit(0);
}
