#include "tape.h"
text_record()
{
    static int first = 1;
    char *tape_item();

    if (!first) return;
    first = 0;

/* scene id */
    sprintf (tape.info.id[0],
	"Thematic Mapper. Scene Id %s",tape_item(106,117));
/* product id */
    strcat (tape.info.desc[1], "Mission ");
    strcat (tape.info.desc[1], tape_item(32,34));
    strcat (tape.info.desc[2], "Path ");
    strcat (tape.info.desc[2], tape_item (36,38));
    strcat (tape.info.desc[2], ",  Row ");
    strcat (tape.info.desc[2], tape_item (39,41));
    strcat (tape.info.desc[2], ",  Quadrant ");
    strcat (tape.info.desc[2], tape_item (35,35));
/* date/time */
    sprintf (tape.info.desc[3], "Date %s/", tape_item(78,79));
    strcat (tape.info.desc[3], tape_item(80,82));

/* CC tape type */
    if (strcmp (tape_item(47,48),"04") == 0)
	strcpy (tape.info.desc[4],"Geometrically uncorrected data");
    else if (strcmp (tape_item(47,48),"05") == 0)
	strcpy (tape.info.desc[4],"Geometrically corrected data (without geodetic corrections)");
    else if (strcmp (tape_item(47,48),"06") == 0)
	strcpy (tape.info.desc[4],"Geometrically corrected data (with geodetic corrections)");
}
