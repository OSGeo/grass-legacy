#include "vask.h"


/*!
 * \brief set number of decimal places
 *
 * 
 * V_float_accuracy() defines the number of decimal places in which floats and
 * doubles are displayed or accepted. <b>Num</b> is an integer value defining
 * the number of decimal places to be used. This routine affects subsequent calls
 * to V_const() and V_ques(). Various inputs or displayed constants can be
 * represented with different numbers of decimal places within the same screen
 * display by making different calls to V_float_accuracy() before calls to
 * V_ques() or V_const(). V_clear() resets the number of decimal places to
 * the default (which is unlimited).
 *
 *  \param num
 *  \return int
 */

int V_float_accuracy (int n)
{
    V__.decimal_places = n;

	return 0;
}
