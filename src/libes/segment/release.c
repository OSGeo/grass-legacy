#include <stdlib.h>
#include "segment.h"

/*!
 * \brief free allocated memory
 *
 * Releases the
 * allocated memory associated with the segment file <b>seg.</b> Does not close
 * the file. Does not flush the data which may be pending from previous
 * <i>segment_put</i> calls.
 *
 *  \param seg
 *  \return int
 */

int segment_release ( SEGMENT *SEG)
{
    int i;

    if (SEG->open != 1)
	return -1;

    for (i = 0; i < SEG->nseg; i++)
	free(SEG->scb[i].buf);
    free(SEG->scb);

    SEG->open = 0;

    return 1;
}
