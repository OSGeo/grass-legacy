/*********************************************************************

NAME:		i.tape.mss.h

FUNCTION:	prints header info from a
		set of LANDSAT BIL formatted tapes.

USAGE:		i.tape.mss.h <tapedev>
********************************************************************/

#define GLOBAL
#include "tape.h"

main(argc,argv) char *argv[];
{
    unsigned char buffer[4 * REC_SIZE] ;
    int tapefd ;
    int i;
    int image_size  ;
    int image_offset;
    int tape_1_rows ;
    int tape_2_rows ;
    int tape_total;
    struct Option *tapename;

/*
* NOTE: image_size as documented is the same for geometrically
*	 corrected and uncorrected landsat tapes
*	 image offset is 12 bytes into image record
*/
    G_gisinit(argv[0]);

    tapename = G_define_option();
    tapename->key = "input";
    tapename->type = TYPE_STRING;
    tapename->description = "Name of the tape device";
    tapename->required = YES;

    if (G_parser(argc,argv))
	exit(1);

/* mount the tape */
    tapefd = open (tapename->answer, 0);
    if (tapefd < 0)
    {
	perror (tapename->answer);
	exit(1);
    }

    image_size = 3548;
    image_offset = 12;

    i = read_tape(tapefd, buffer);
    if (i != HAVE_IMAGE)
    {
	printf ("\nPROBABLY NOT AN MSS TAPE!\n");
	exit(1);
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
	break ;
    default:
	exit(0);
    }
    tape_total = tape_1_rows + tape_2_rows;


    printf ("RECORD SIZE: %d\n", record_size) ;
    printf ("IMAGE SIZE:  %d\n", image_size) ;
    printf ("ROWS:\n");
    printf ("\ttape 1: %d\n",tape_1_rows);
    printf ("\ttape 2: %d\n",tape_2_rows);
    printf ("\ttotal:  %d\n",tape_total);
}
