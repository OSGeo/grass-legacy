/* this routine exists becase there are so many things defined in
   dig_defines.h, some of which may collide with paint code. All
   we want here is DOT.
*/
#include "dig_defines.h"

vect_type_is_dot(type)
{
    return type==DOT;
}
