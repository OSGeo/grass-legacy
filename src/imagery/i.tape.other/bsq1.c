/* read each file as a complete band */
#include "tape.h"

bsq1()
{
    int band;
    int lastband;

    for (band=0; band < nbands; band++)
	if (wantband[band])
		lastband = band;
    for (band=0; band <= lastband; band++)
    {
	if (wantband[band])
	{
	    I_tape_advance (tapefd, skiprecords);
	    if(!readbsq (band))
		break;
	}
	if (band < lastband)
	    I_tape_advance (tapefd, -1);
    }
}
