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
/* number of physical volume in the set */
    tape.nvols = atoi (tape_item(93,94));
#ifdef DEBUG
printf("%d volumes\n", tape.nvols);
#endif

/* tape id for physical volume containing this volume description */
    tape.mission_number = atoi(tape_item (45,52));
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
	    else if (strcmp(tape_item(33,35),"BIL") == 0)
		tape.interleaving = BIL;
	    else
		tape.interleaving = -1;

/* extract mode info from lead mesg */
	    strcat(tape.info.id[1],"Spectral Mode: ");
	    strcat(tape.info.id[1],tape_item(25,25));

	    if (strcmp(tape_item(25,25),"P") == 0)
	    {
		tape.nbands = 1;
	    }
	    else 
		tape.nbands = SPOT_NBANDS;
	}
	else if (strcmp(tape_item(65,68),"IMGY") == 0)
	{
	    tape.tapebufsize=atoi(tape_item(117,124));
	    tape.band[b].vol = atoi (tape_item(143,144));
#ifdef DEBUG
printf("band %d: %d rows, %d cols (vol %d)\n",
  b+1, tape.band[b].nrows, tape.band[b].ncols, tape.band[b].vol);
#endif
	    b++;
	}
    }

#ifdef DEBUG
I_ask("hit RETURN-->", 0, 1) ;
#endif
}
