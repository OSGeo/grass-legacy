/*********************************************************************

NAME:		i.tape.mss

FUNCTION:	allows a user to extract a window off of a 
		set of LANDSAT BIL formatted tapes.

INPUT FILE:	generally /dev/rmt0  (user selected)

OUTPUT FILES:   raw band cell files

NOTE:	program can only handle MSS corrected tapes.
	MSS uncorrected image format is different than the corrected
	format.
********************************************************************/

#define GLOBAL
#include "tape.h"

main(argc,argv) char *argv[];
{
    unsigned char buffer[4 * REC_SIZE] ;
    int tape_1_rows     ;
    int tape_2_rows	    ;
    int tape_total	    ;
    char tapename[20]   ;
    int tapefd;
    int do_tape_1       ;
    int do_tape_2       ;
    int i               ;
    int image_size	    ;
    int image_offset    ;


    G_gisinit (argv[0]);
    I_must_be_imagery_projection();
    G_want_histogram(1);

    I_clear_tape_info (&tape_info);
/*
* NOTE: image_size as documented is the same for geometrically
*	 corrected and uncorrected landsat tapes
*	 image offset is 12 bytes into image record
*/
    image_size = 3548;
    image_offset = 12;

/* mount the tape */

    get_tapename(tapename) ;
    for (vol = 0; vol != 1; )
    {
	I_ask("Please mount LANDSAT tape 1, then hit RETURN-->", 0, 1) ;

	tapefd = mount_tape (tapename);

    /* read tape to get volume number */

	i = read_tape(tapefd, buffer, 1);
	close(tapefd) ;
	tapefd = -1;
    }

    printf("###############################################\n") ;
    if (i != HAVE_IMAGE)
    {
	printf ("** unknown tape format **\n");
	exit(1);
    }


    switch(tape_type)
    {
    char *format;

    case BIL: 
	    break ;
    default:
	    format = tape_type == BSQ ? "bsq" : "**unknown**";
	    printf("TAPE FORMAT: %s\n\t** this program only works with BIL format tapes\n", format) ;
	    exit(-1) ;
	    break ;
    }

    switch (correction)
    {
    case CORRECTED:
    case UNCORRECTED:      break;

    default:       
	    printf("CORRECTION: ** unknown **\n");
	    if (G_yes("Is this a corrected tape? ", -1))
		correction = CORRECTED;
	    else
		correction = UNCORRECTED;
    }

    switch(correction)
    {
    case CORRECTED:
	printf("corrected landsat tape\n");
	tape_1_rows = 1491 ;
	tape_2_rows = 1493 ;
	break ;

    case UNCORRECTED:
	printf("un-corrected landsat tape\n");
	tape_1_rows = 2400 ;
	tape_2_rows = 0 ;

	printf("%s can't read un-corrected MSS tapes\n", argv[0]);
	exit(1);

	break ;
    }
    tape_total = tape_1_rows + tape_2_rows;

    ask_info ();
    ask_window (image_size,tape_total);
    cellbuf = G_allocate_cell_buf();
    want_band= I_ask_bands(nbands);

    do_tape_1 = firstrow <= tape_1_rows;
    do_tape_2 = lastrow  >  tape_1_rows;


/* create and open the band files */

    for (i = 0; i < nbands; i++)
    {
	if (want_band[i])
	    bandfd[i] = I_open_band_new(i);
	else
	    bandfd[i] = -1;
    }


    if (do_tape_1)
    {
	vol = 0 ;
	while (vol != 1)
	{
	    close(tapefd) ;
	    tapefd = -1;
	    I_ask("Please mount, load, and put on-line TAPE 1. Then hit RETURN-->", 0, 1) ;

	    tapefd = mount_tape (tapename);
	    read_tape(tapefd, buffer, 0) ;
	}

	extract (tapefd, buffer, 1, tape_1_rows, firstrow, lastrow, firstcol+image_offset, lastcol+image_offset);
	close(tapefd) ;
	tapefd = -1;
    }

    if (do_tape_2)
    {
	vol = 0 ;
	while (vol != 2)
	{
	    close(tapefd) ;
	    tapefd = -1;
	    I_ask("Please mount, load, and put on-line TAPE 2. Then hit RETURN-->", 0, 1) ;

	    tapefd = mount_tape (tapename);
	    read_tape(tapefd, buffer, 0) ;
	}

	extract (tapefd, buffer, tape_1_rows+1, tape_total, firstrow, lastrow, firstcol+image_offset, lastcol+image_offset);
	close(tapefd) ;
	tapefd = -1;
    }

/* close the band files */

    for (i = 0; i < nbands; i++)
    {
	if (bandfd[i] >= 0)
	    I_close_band (bandfd[i], &tape_info, i);
    }
}
