/* makepp.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

double
MakePP( Row, Col, OutRows, OutCols, Randoms, BigF)
	int	Row, Col, OutRows, OutCols;
	double	**Randoms;
	BIGF	BigF;
{
	int	DRow, DCol, LR, HR, LC, HC;
	int	RRow, RCol;
	double	Effect, Value;

	FUNCTION(MakePP);

	Value = 0.0;
	RRow = Row + BigF.RowPlus;
	RCol = Col + BigF.ColPlus;
	for( DRow = RRow - BigF.RowPlus;
	     DRow <= RRow + BigF.RowPlus; DRow++) {
	    /* if( BigF.LowBF  this to speed up function */
	    for( DCol = RCol - BigF.ColPlus;
		 DCol <= RCol + BigF.ColPlus; DCol++) {
		DistDecay( &Effect, RRow-DRow, RCol-DCol);
		INT(RRow-DRow);
		INT(RCol-DCol);
		DOUBLE(Effect);
		RETURN;
		Value += Effect * Randoms[ DRow][ DCol];
	    }
	}
	return( Value);
	FUNCTION(end MakePP);
}
