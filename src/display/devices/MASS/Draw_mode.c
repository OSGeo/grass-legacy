#include "graphics.h"

Draw_mode (mode)
{
	switch (mode) {

	case DRAW_DASH:

#define _dashed 006314631463
#define _dotted 012525252525
		mgidash (1, _dotted);
#undef _dashed
#undef _dotted
		break;
	
	case DRAW_NODASH:

		mgidash (2, 0);
		break;

	case DRAW_NORMAL: /* normal ten planes */

		mgipln (1023);
		mgimodfunc (3, 0, 0) ;

		break;

	case DRAW_BLACK: /* draw in plane 10 */

		mgihue (512);
		mgipln (512);
		mgimodfunc (1, 3, 3);

		break;

	case DRAW_ERASE: /* erase in plane 10 */

		mgihue (512);
		mgipln (512);
		mgimodfunc (0, 1, 3);

		break;
	
	case DRAW_FLIP: /* flip bit in plane 10 */

		mgihue(512);
		mgipln(512);
		mgimodfunc (1, 4, 3);
	}
}
