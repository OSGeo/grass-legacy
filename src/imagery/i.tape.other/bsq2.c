/* read each band from current file */
#include "tape.h"

bsq2()
{
    int band;
    int tail;
    int lastband;

    tail = bandsize - lastrow ;

    I_tape_advance (tapefd, skiprecords);
    for (band=0; band < nbands; band++)
	if (wantband[band])
		lastband = band;
    for (band=0; band <= lastband; band++)
    {
	if (wantband[band])
	{
	    if(!readbsq (band))
		break;
	}
	else if (band < lastband)
	    I_tape_advance(tapefd, nrows);
	if (band < lastband)
	    I_tape_advance (tapefd, tail);
    }
}
