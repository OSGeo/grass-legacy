#include <string.h>
#include "gis.h"

static char *NULL_STRING = "null";
static int reclass_type(FILE *,char *,char *);
static FILE *fopen_cellhd_old( char *, char *);
static FILE *fopen_cellhd_new(char *);
static int get_reclass_table(FILE *, struct Reclass *);

int G_is_reclass (char *name, char *mapset, char *rname, char *rmapset)
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

int G_is_reclassed_to (char *name, char *mapset, int *nrmaps, char ***rmaps)
{
    FILE *fd;
    int i, j, k, l;
    char buf1[256], buf2[256], buf3[256], *p;

    strcpy(buf2, name);
    if ((p = strchr(buf2, '@')))
        *p = 0;

    sprintf (buf1, "%s/%s/cell_misc/%s/reclassed_to",
    		G__location_path(), mapset, buf2);

    fd = fopen(buf1, "r");

    if (fd == NULL)
    {
        return -1;
    }

    if (rmaps)
        *rmaps = NULL;
    for (i=0; !feof(fd) && fgets(buf2, 255, fd); )
    {
	l = strlen(buf2);
	for (j=0, k=0; j<l; j++)
	{
	    if(buf2[j] == '#' ||
		((buf2[j] == ' ' || buf2[j] == '\t' || buf2[j] == '\n') && k))
	        break;
	    else
	    if(buf2[j] != ' ' && buf2[j] != '\t')
		buf3[k++] = buf2[j];
	}

	if (k)
	{
	    buf3[k] = 0;
	    i++;
	    if (rmaps)
	    {
	        *rmaps = (char **) G_realloc(*rmaps, i*sizeof(char *));
	        (*rmaps)[i-1] = (char *) G_malloc(k+1);
	        strncpy((*rmaps)[i-1], buf3, k);
	        (*rmaps)[i-1][k] = 0;
	    }
	}
    }

    if (nrmaps)
        *nrmaps = i;

    if (i && rmaps)
    {
	i++;
	*rmaps = (char **) G_realloc(*rmaps, i*sizeof(char *));
	(*rmaps)[i-1] = NULL;
    }

    return i;
}

int G_get_reclass (char *name, char *mapset, struct Reclass *reclass)
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
	    sprintf(msg, "Illegal reclass format in header file for [%s in %s]",
		    name, mapset);
	G_warning (msg);
	stat = -1;
    }
    return stat;
}

int G_free_reclass (struct Reclass *reclass)
{
    switch (reclass->type)
    {
    case RECLASS_TABLE:
	if (reclass->num > 0)
	    G_free (reclass->table);
	reclass->num = 0;
	break;
    default:
	break;
    }

    return 0;
}

static int reclass_type( FILE *fd,char *rname,char *rmapset)
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

static FILE *fopen_cellhd_old( char *name, char *mapset)
{
    return G_fopen_old ("cellhd", name, mapset);
}

int G_put_reclass (char *name, struct Reclass *reclass)
{
    FILE *fd;
    long min, max;
    int i;
    char buf1[256], buf2[256], buf3[256], *p;

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
	G_warning ("Unable to create header file for [%s in %s]",
		name, G_mapset());
	return -1;
    }

    fprintf (fd, "reclass\n");
    fprintf (fd, "name: %s\n", reclass->name);
    fprintf (fd, "mapset: %s\n", reclass->mapset);

/* find first non-null entry */
    for (min = 0; min < reclass->num; min++)
	if (!G_is_c_null_value(&reclass->table[min]))
	    break;
/* find last non-zero entry */
    for (max = reclass->num-1; max >= 0; max--)
	if (!G_is_c_null_value(&reclass->table[max]))
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
	{
	    if (G_is_c_null_value(&reclass->table[min]))
	       fprintf (fd, "%s\n", NULL_STRING);
            else 
	       fprintf (fd, "%ld\n", (long) reclass->table[min]);
	    min++;
        }
    }
    fclose (fd);

    strcpy(buf2, reclass->name);
    if ((p = strchr(buf2, '@')))
        *p = 0;

    sprintf (buf1, "%s/%s/cell_misc/%s/reclassed_to",
    		G__location_path(), reclass->mapset, buf2);

    fd = fopen(buf1, "a+");
    fseek (fd, 0L, SEEK_SET);

    if (fd == NULL)
    {
        G_warning ("Unable to create dependency file in [%s in %s]",
    	    buf2, reclass->mapset);
        return -1;
    }

    sprintf (buf2, "%s@%s\n", name, G_mapset());
    for (i=0; !feof(fd) && fgets(buf3, 255, fd); )
    {
        if (!(strcmp(buf2, buf3)))
        {
            i = 1;
    	break;
        }
    }

    if (!i)
    {
        fprintf (fd, "%s@%s\n", name, G_mapset());
    }

    fclose (fd);

    return 1;
}

static FILE *fopen_cellhd_new(char *name)
{
    return G_fopen_new ("cellhd", name);
}

static int get_reclass_table(FILE *fd, struct Reclass *reclass)
{
    char buf[128];
    int n;
    int first, null_str_size;
    CELL cat;
    long len;

/*
 * allocate the table, expanding as each entry is read
 * note that G_realloc() will become G_malloc() if ptr in
 * NULL
 */
    reclass->min = 0;
    reclass->table = NULL;
    null_str_size = strlen(NULL_STRING);
    n = 0;
    first = 1;
    while (fgets (buf, sizeof buf, fd))
    {
	if (first)
	{
	    first = 0;
	    if (sscanf (buf, "#%d", &cat) == 1)
	    {
		reclass->min = cat;
		continue;
	    }
	}
	if(strncmp(buf, NULL_STRING, null_str_size)==0)
	    G_set_c_null_value(&cat, 1);
        else
	{
  	    if (sscanf (buf, "%d", &cat) != 1)
	        return -1;
        }
	n++;
	len = (long) n * sizeof (CELL);
	if (len != (int)len)		/* check for int overflow */
	{
	    if (reclass->table != NULL)
		G_free (reclass->table);
	    return -2;
	}
	reclass->table = (CELL *) G_realloc ((char *) reclass->table, (int)len);
	reclass->table[n-1] = cat;
    }
    reclass->max = reclass->min + n - 1;
    reclass->num = n;
    return 1;
}
