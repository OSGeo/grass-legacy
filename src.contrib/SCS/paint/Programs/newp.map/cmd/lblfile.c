#include "gis.h"
#include "labels.h"

labelfile (name, mapset)
    char *name;
    char *mapset;

{
    char fullname[100];

    sprintf (fullname, "%s in %s", name, mapset);

    if (labels.count >= MAXLABELS)
    {
	error (fullname,"","no more label files allowed");
	return 0;
    }

    labels.name[labels.count] = G_store (name);
    labels.mapset[labels.count] = G_store (mapset);

    labels.count++;
    return 1;
}
