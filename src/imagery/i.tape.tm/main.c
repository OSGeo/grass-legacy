#define GLOBAL
#include "tape.h"

main (argc,argv) char *argv[];
{
    char msg[100];
    G_gisinit(argv[0]);

    I_must_be_imagery_projection();
    G_want_histogram(1);

    I_clear_tape_info (&tape.info);

    get_tapename(tape.name);
    tape.fd = -1;

/* read volume descriptor from first tape */
    I_ask("mount thematic mapper tape and hit RETURN-->", 0, 1);
    mount_tape ();
    while (!(read_tape(0) && tape.record_type == VOLUME_DESCRIPTOR))
    {
	unmount_tape();
	I_ask("mount thematic mapper tape and hit RETURN-->", 0, 1);
	mount_tape ();
    }

/* read volume descriptor file */

    header(0);

/* band sequential or band interleave? */

    switch (tape.interleaving) {

    case BSQ:	bsq();
		break;

    case BIL:	bil();
		break;

    default:	printf("unknown interleaving type\n");
		exit(0);
    }

/* extraction done */

    unmount_tape();
    exit(0);
}
