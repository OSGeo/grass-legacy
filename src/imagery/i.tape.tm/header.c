#include "tape.h"

#define debug stderr

/* read the volume directory file */

header(vol_number_only)
{
    char *tape_item();
    char *item;
    int b;


    if (strcmp(item = tape_item(17,28),"CCB-CCT-0002")!=0)
    {
	printf("unexpected format control document number: %s\n", item);
	exit(-1);
    }

    tape.vol   = atoi (tape_item(99,100));
#ifdef DEBUG
printf("volume %d\n", tape.vol);
#endif

    if (vol_number_only)
	return;
    tape.nvols = atoi (tape_item(93,94));
#ifdef DEBUG
printf("%d volumes\n", tape.nvols);
#endif

    tape.mission_number = atoi(tape_item (46,46));
    tape.corrected = strcmp (tape_item(48,49),"CP") == 0;
#ifdef DEBUG
printf("%scorrected image\n", tape.corrected?"":"un-");
#endif

    b = 0;
    while (read_tape(0))
    {
	if (tape.record_type == TEXT)
	    text_record();
	if (tape.record_type != FILE_POINTER)
	    continue;

	if (strcmp(tape_item(65,68),"LEAD") == 0)
	{
#ifdef DEBUG
if(b==0)printf ("interleaving %s\n", tape_item(33,35));
#endif
	    if (strcmp(tape_item(33,35),"BSQ") == 0)
		tape.interleaving = BSQ;
	    else
		tape.interleaving = BIL;
	}
	else if (strcmp(tape_item(65,68),"IMGY") == 0)
	{
	    tape.band[b].nrows = atoi (tape_item(101,108));
	    /* there is a problem with nrows on tape 
	     * bug may be in the code, but i think there
	     * are less rows of data than stated, so I
	     * am artificially reducing the number of rows
	    tape.band[b].nrows -= 2;
	     */

	    if (tape.corrected)
		tape.band[b].ncols = CORRECTED_NCOLS;
	    else
		tape.band[b].ncols = UNCORRECTED_NCOLS;
	    tape.band[b].vol = atoi (tape_item(141,142));
#ifdef DEBUG
printf("band %d: %d rows, %d cols (vol %d)\n",
  b+1, tape.band[b].nrows, tape.band[b].ncols, tape.band[b].vol);
#endif
	    b++;
	}
    }

    tape.nrows = tape.band[0].nrows ;
    tape.ncols = tape.band[0].ncols ;

    if (tape.interleaving == BIL)
	tape.nrows /= THEMATIC_MAPPER_NBANDS;
    else
	for (b = 1; b < THEMATIC_MAPPER_NBANDS; b++)
	{
	    if (tape.band[b].nrows > 0 && tape.band[b].nrows < tape.nrows)
		tape.nrows = tape.band[b].nrows;
	}
#ifdef DEBUG
I_ask("hit RETURN-->", 0, 1) ;
#endif
}
