/* read each band from current file */
#include "tape.h"

bsq2()
{
    int band;
    int tail;
    int lastband;

    tail = bandsize - (lastrow-1)/blocking_factor - 1;

    I_tape_advance (tapefd, skiprecords);
    for (band=0; band < nbands; band++)
	if (wantband[band])
		lastband = band;
    for (band=0; band <= lastband; band++)
    {
	if (wantband[band])
	{
	    first[band] = 1;
	    if(!readbsq (band))
		break;
	}
	else if (band < lastband) {
	    /* I_tape_advance(tapefd, nrows); wrong when band not wanted */
	    I_tape_advance(tapefd, (lastrow-1)/blocking_factor+1);
	}
	if (band < lastband)
	    I_tape_advance (tapefd, tail);
    }
}
