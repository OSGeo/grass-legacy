#include <grass/D.h>


/*!
 * \brief
 *
 * Clear current display window
 *
 *  \return int
 */

int Dscreen()
{
    Dclearscreen();
    Dnew ("full_screen", 0.0, 100.0, 0.0, 100.0);
    Dchoose ("full_screen");

    return 0;
}
