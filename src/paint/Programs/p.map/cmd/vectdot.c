/* this routine exists becase there are so many things defined in
   dig_defines.h, some of which may collide with paint code. All
   we want here is DOT.
*/
#include "vect/dig_defines.h"

int vect_type_is_dot(int type)
{
    return type==DOT;
}
