#include <string.h>
#include "gis.h"
#include "local_proto.h"

char *spheroid_list (void)
{
    int len,n;
    char *list;
    char *name;

    len=0;
    for (n=0; name = G_ellipsoid_name(n); n++)
	len += strlen(name)+1;
    list = G_malloc(len);
    for (n=0; name = G_ellipsoid_name(n); n++)
    {
	if (n) strcat (list, ",");
	else *list = 0;
	strcat (list, name);
    }
    return list;
}
