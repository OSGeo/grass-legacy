
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "Vect.h"
#include "debug.h"
#include "wind.h"
#include "Map_proto.h"
#include "local_proto.h"

int Del_isle (struct Map_info *map, int isle)
{
    isle = abs (isle);

    if (! ISLE_ALIVE (&(map->Isle[isle])))
	return (-1);


/*DEBUG*/ debugf ("Calling del_isle\n");
    dig_del_isle (map, isle);

    return 0;
}
