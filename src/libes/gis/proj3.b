#include "gis.h"

char *
G_database_unit_name(plural)
{
    int n;
    static char name[256];
    char *G__unit_name();

    switch(n=G_projection())
    {
    case PROJECTION_XY:
    case PROJECTION_UTM:
    case PROJECTION_LL:
    case PROJECTION_SP:
	return G__unit_name(G__projection_units(n), plural);
    }

    if(!lookup (UNIT_FILE, plural?"units":"unit", name, sizeof(name)))
	strcpy (name, plural ? "units" : "unit");
    return name;
}

char *
G_database_projection_name()
{
    int n;
    static char name[256];
    char *G__projection_name();

    switch(n=G_projection())
    {
    case PROJECTION_XY:
    case PROJECTION_UTM:
    case PROJECTION_LL:
    case PROJECTION_SP:
	return G__projection_name(n);
    }
    if(!lookup (PROJECTION_FILE, "name", name, sizeof(name)))
	strcpy (name, "Unknown projection");
    return name;
}

double
G_database_units_to_meters_factor()
{
    char *unit;
    double factor;
    char buf[256];
    int n;

    static struct
    {
	char *unit;
	double factor;
    } table[] =
    {
	"meter", 1.0,
	"foot", .3048,
	"inch", .0254,
	NULL, 0.0
    };

    factor = 0.0;
    if (lookup(UNIT_FILE, "meters", buf, sizeof(buf)))
	sscanf (buf, "%lf", &factor);
    if (factor <= 0.0)
    {
	unit = G_database_unit_name(0);
	for (n=0; table[n].unit; n++)
	    if (equal(unit, table[n].unit))
	    {
		factor = table[n].factor;
		break;
	    }
    }
    return factor;
}

static
lookup (file, key, value, len)
    char *file, *key, *value;
{
    char path[1024];

    G__file_name (path, "", file, G_mapset());
    if (access(path,0) == 0)
	return G_lookup_key_value_from_file(path, key, value, len) == 1;
    G__file_name (path, "", file, "PERMANENT");
    return G_lookup_key_value_from_file(path, key, value, len) == 1;
}

static
equal(a,b)
    char *a, *b;
{
    if (a == NULL || b == NULL)
	return a==b;
    while (*a && *b)
	if (lower(*a++) != lower(*b++))
	    return 0;
    if (*a || *b)
	return 0;
    return 1;
}

static
lower(c)
    char c;
{
    if (c >= 'A' && c <= 'Z')
	c += 'a' - 'A';
    return c;
}
