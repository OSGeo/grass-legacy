/************************************************************************/
/* NAME:	text_read						*/
/*									*/
/* FUNCTION:	read in parameters of the satellite of TM data		*/
/*									*/
/* USAGE:	text_rec_91()						*/
/*									*/
/* INPUT:	none							*/
/*									*/
/* OUTPUT:	none							*/
/************************************************************************/
#include "tape.h"
text_rec_91()
{
    static int first = 1;
    char *header_item();

    if (!first) return;
    first = 0;
/* for the TM data after Oct. 1, 1991; EOSAT, Fast Format Document, Version B */
/* scene id */
    sprintf (tape.info.id[0],
	"TM. Scene Id: %s",header_item(10,20));
    strcat (tape.info.id[0], "; LANDSAT ");
    strcat (tape.info.id[0], header_item(76,76));
    strcat (tape.info.id[0], "; PROD TYPE: ");
    strcat (tape.info.id[0], header_item(109,122));
    strcat (tape.info.id[1], "PROD SIZE: ");
    strcat (tape.info.id[1], header_item(138, 147));
/* date/time */
    strcat (tape.info.id[1], "; ACQ DATE: "); /* yyyy-mm-dd */
    strcat (tape.info.id[1], header_item(55,58));
    strcat (tape.info.id[1], "-");
    strcat (tape.info.id[1], header_item(59,60));
    strcat (tape.info.id[1], "-");
    strcat (tape.info.id[1], header_item(61,62));
    strcat (tape.info.id[1], "; RESAMPLING: ");
    strcat (tape.info.id[1], header_item(279,280));
    strcat (tape.info.id[1], "; PROJ: ");
    strcat (tape.info.id[1], header_item(514,517));
/* product id */
    strcat (tape.info.desc[0], "Earth Ellipsoid: ");
    strcat (tape.info.desc[0], header_item(973,992));
    strcat (tape.info.desc[0], "; WRS: ");
    strcat (tape.info.desc[0], "Path ");
    strcat (tape.info.desc[0], header_item(27,29));
    strcat (tape.info.desc[0], "; Row ");
    strcat (tape.info.desc[0], header_item (31,33));
    strcat (tape.info.desc[1], "Sun Elev: ");
    strcat (tape.info.desc[1], header_item (1427,1428));
    strcat (tape.info.desc[1], "; Sun Azimuth: ");
    strcat (tape.info.desc[1], header_item (1443,1445));
    strcat (tape.info.desc[1], "; Pixels/Line: ");
    strcat (tape.info.desc[1], header_item (1086,1090));
    strcat (tape.info.desc[1], "; Image Lines: ");
    strcat (tape.info.desc[1], header_item (1108,1112));
    strcat (tape.info.desc[2], "Bands Present: ");
    strcat (tape.info.desc[2], header_item (1361,1367));
    strcat (tape.info.desc[2], "; Record Length: ");
    strcat (tape.info.desc[2], header_item(1406,1410));
    strcat (tape.info.desc[2], "; Blocking Factor: ");
    strcat (tape.info.desc[2], header_item(1386,1389));
}
