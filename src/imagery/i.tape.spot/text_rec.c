/************************************************/
/* NAME:	text_rec			*/
/*						*/
/* FUNCTION:	read messages into history file */
/*						*/
/* USAGE:	text_rec()			*/
/*						*/
/* INPUT:	none				*/
/*						*/
/* OUTPUT:	none				*/
/************************************************/
/* need to figure out which items should be read in or set up another 
routine to read some parameters from other records;
cross over the EOF to extract parameters; */ 
#include "tape.h"
text_record()
{
    static int first = 1;
    char *tape_item();

    if (!first) return;
    first = 0;

/* scene id */
    strcat (tape.info.id[0],"SPOT IMAGE. ");
    strcat (tape.info.id[0], tape_item(18,31));
    strcat (tape.info.id[0],"; ");
    strcat (tape.info.id[0], tape_item(32,39));
    strcat (tape.info.id[0], tape_item(41,41));
    strcat (tape.info.id[0],"; ");
    strcat (tape.info.id[0], tape_item(44,46));
    strcat (tape.info.id[0], tape_item(48,48));
    strcat (tape.info.id[0],"; ");
    strcat (tape.info.id[0], tape_item(51,53));
    strcat (tape.info.id[1],"; ");
    strcat (tape.info.id[1], tape_item(56,60));
    strcat (tape.info.id[1],": ");
    strcat (tape.info.id[1], tape_item(63,64));
    strcat (tape.info.id[1], "; Tape Id: ");
    strcat (tape.info.id[1], tape_item(180,187));
/* product id */
    strcat (tape.info.desc[0], "Mission: ");
    strcat (tape.info.desc[0], tape_item(36,41));
    strcat (tape.info.desc[0], "; WRS REF: Path ");
    strcat (tape.info.desc[0], tape_item (143,145));
    strcat (tape.info.desc[0], ",  Row ");
    strcat (tape.info.desc[0], tape_item (147,149));
    strcat (tape.info.desc[0], ";  Scene Shift: ");
    strcat (tape.info.desc[0], tape_item (151,151));
}
