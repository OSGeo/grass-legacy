
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"


Del_isle (map, isle)
    struct Map_info *map;
    int isle;
{
    isle = ABS (isle);

    if (! ISLE_ALIVE (&(map->Isle[isle])))
	return (-1);


/*DEBUG*/ debugf ("Calling del_isle\n");
    dig_del_isle (map, isle);
}
