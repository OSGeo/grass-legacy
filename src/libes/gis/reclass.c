#include "gis.h"

FILE *fopen_cellhd_old();
FILE *fopen_cellhd_new();

G_is_reclass (name, mapset, rname, rmapset)
    char *name, *mapset;
    char *rname, *rmapset;
{
    FILE *fd;
    int type;

    fd = fopen_cellhd_old (name, mapset);
    if (fd == NULL)
	return -1;
    
    type = reclass_type (fd, rname, rmapset);
    fclose (fd);
    if (type < 0)
	return -1;
    else
	return type != 0;
}

G_get_reclass (name, mapset, reclass)
    char *name, *mapset;
    struct Reclass *reclass;
{
    FILE *fd;
    int stat;

    fd = fopen_cellhd_old (name, mapset);
    if (fd == NULL)
	return -1;
    reclass->type = reclass_type (fd, reclass->name, reclass->mapset);
    if (reclass->type <= 0)
    {
	fclose (fd);
	return reclass->type;
    }

    switch (reclass->type)
    {
    case RECLASS_TABLE:
	stat = get_reclass_table (fd, reclass);
	break;
    default:
	stat = -1;
    }

    fclose (fd);
    if (stat < 0)
    {
	char msg[100];
	if (stat == -2)
	    sprintf(msg, "Too many reclass categories for [%s in %s]",
		    name, mapset);
	else
	    sprintf(msg, "Illegal reclass format in cell header for [%s in %s]",
		    name, mapset);
	G_warning (msg);
	stat = -1;
    }
    return stat;
}

G_free_reclass (reclass)
    struct Reclass *reclass;
{
    switch (reclass->type)
    {
    case RECLASS_TABLE:
	if (reclass->num > 0)
	    free (reclass->table);
	reclass->num = 0;
	break;
    default:
	break;
    }
}

static
reclass_type (fd, rname, rmapset)
    FILE *fd;
    char *rname, *rmapset;
{
    char buf[128];
    char label[128], arg[128];
    int i;
    int type;

/* Check to see if this is a reclass file */
    if (fgets(buf,sizeof(buf),fd) == NULL)
	return 0;
    if (strncmp(buf,"reclas",6))
	return 0;
/* later may add other types of reclass */
    type = RECLASS_TABLE;

/* Read the mapset and file name of the REAL cell file */
    *rname = *rmapset = 0;
    for (i=0; i<2; i++)
    {
	if (fgets(buf,sizeof buf,fd) == NULL)
	    return -1;
	if(sscanf(buf,"%[^:]:%s", label, arg) != 2)
	    return -1;
	if (! strncmp(label, "maps", 4))
	    strcpy(rmapset, arg) ;
	else if (! strncmp(label, "name", 4))
	    strcpy(rname, arg) ;
	else
	    return -1;
    } 
    if (*rmapset && *rname)
	return type;
    else
	return -1;
}

static
FILE *
fopen_cellhd_old (name, mapset)
    char *name;
    char *mapset;
{
    return G_fopen_old ("cellhd", name, mapset);
}

G_put_reclass (name, reclass)
    char *name;
    struct Reclass *reclass;
{
    FILE *fd;
    long min, max;
    char msg[100];

    switch (reclass->type)
    {
    case RECLASS_TABLE:
	if (reclass->min > reclass->max || reclass->num <= 0)
	{
	    G_fatal_error ("Illegal reclass request");
	    return -1;
	}
	break;
    default:
	G_fatal_error ("Illegal reclass type");
	return -1;
    }
    fd = fopen_cellhd_new (name);
    if (fd == NULL)
    {
	sprintf (msg, "Unable to create cell header for [%s in %s]",
		name, G_mapset());
	G_warning (msg);
	return -1;
    }

    fprintf (fd, "reclass\n");
    fprintf (fd, "name: %s\n", reclass->name);
    fprintf (fd, "mapset: %s\n", reclass->mapset);

/* find first non-zero entry */
    for (min = 0; min < reclass->num; min++)
	if (reclass->table[min])
	    break;
/* find last non-zero entry */
    for (max = reclass->num-1; max >= 0; max--)
	if (reclass->table[max])
	    break;

/*
 * if the resultant table is empty, write out a dummy table
 * else write out the table
 *   first entry is #min
 *   rest are translations for cat min+i
 */
    if (min > max)
	fprintf (fd, "0\n");
    else
    {
	fprintf (fd, "#%ld\n", (long) reclass->min + min);
	while (min <= max)
	    fprintf (fd, "%ld\n", (long) reclass->table[min++]);
    }
    fclose (fd);
    return 1;
}

static
FILE *
fopen_cellhd_new (name)
    char *name;
{
    return G_fopen_new ("cellhd", name);
}

static
get_reclass_table (fd, reclass)
    FILE *fd;
    struct Reclass *reclass;
{
    char buf[128];
    int n;
    int first;
    long cat;
    long len;

/*
 * allocate the table, expanding as each entry is read
 * note that G_realloc() will become G_malloc() if ptr in
 * NULL
 */
    reclass->min = 0;
    reclass->table = NULL;
    n = 0;
    first = 1;
    while (fgets (buf, sizeof buf, fd))
    {
	if (first)
	{
	    first = 0;
	    if (sscanf (buf, "#%ld", &cat) == 1)
	    {
		reclass->min = cat;
		continue;
	    }
	}
	if (sscanf (buf, "%ld", &cat) != 1)
	    return -1;
	n++;
	len = (long) n * sizeof (CELL);
	if (len != (int)len)		/* check for int overflow */
	{
	    if (reclass->table != NULL)
		free (reclass->table);
	    return -2;
	}
	reclass->table = (CELL *) G_realloc (reclass->table, (int)len);
	reclass->table[n-1] = cat;
    }
    reclass->max = reclass->min + n - 1;
    reclass->num = n;
    return 1;
}
