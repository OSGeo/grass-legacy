/*********************************************************************

NAME:		i.tape.other

FUNCTION:	generic band file tape extraction

INPUT FILE:	tape drive with rewind upon close

OUTPUT FILES:   raw band cell files

********************************************************************/

#define GLOBAL
#include "tape.h"

main(argc,argv) char *argv[];
{
    char tapename[20];

    G_gisinit (argv[0]);
    I_must_be_imagery_projection();

/* mount the tape */

    get_tapename(tapename) ;
    I_ask("Please mount and load tape, then hit RETURN-->", 0, 1) ;
    tapefd = mount_tape (tapename);

/* ask for description of the tape */
    I_clear_tape_info (&tape_info);
    I_edit_tape_info (&tape_info);

/* ask for tape layout */
    ask_format();

    wantband = I_ask_bands (nbands);

/* get rows and cols to extract */

    ask_window();

    tapebuf = (unsigned char *) G_malloc (lastcol+1);
    cellbuf = G_allocate_cell_buf();

/* skip the initial files */

    printf("\n\n");
    if (skipfiles > 0)
    {
	fprintf(stderr,"skipping %d files..",skipfiles);
	while (skipfiles--)
	    I_tape_advance (tapefd, -1);
	fprintf(stderr,"\n\n");
    }

/* call the appropriate routine to handle the various formats */
/* for each bandfile, copy from tape to disk */

    switch (format)
    {
    case BIL:	bil();	break;

    case BSQ1:	bsq1();	break;

    case BSQ2:	bsq2();	break;
    }

    close (tapefd);

    exit(0);
}
