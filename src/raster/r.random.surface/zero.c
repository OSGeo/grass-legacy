/* zero.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

ZeroMapCells()
{
	int Row, Col;
	FUNCTION(ZeroMapCells);

	for( Row = 0; Row < Rs; Row++) {
		for( Col = 0; Col < Cs; Col++)
			Surface[ Row][ Col] = 0.0;
	}
}
