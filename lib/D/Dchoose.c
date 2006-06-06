#include <time.h>
#include <grass/display.h>
#include <grass/D.h>


/*!
 * \brief
 *
 * Choose display window frame <b>name</b>
 *
 *  \param name
 *  \return int
 */

int Dchoose(char *name)
{
	int stat ;

	if ((stat = D_set_cur_wind(name)))
		return(stat) ;
	else
		D_timestamp() ;

	return 0;
}
