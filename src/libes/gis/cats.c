/**********************************************************************
 *
 * Code in this file works with category files.  There are two formats:
 * Pre 3.0 direct category encoding form:
 * 
 *    2 categories
 *    Map Title
 *    Elevation: 1000.00 to 1005.00 feet
 *    Elevation: 1005.00 to 1010.00 feet
 *    Elevation: 1010.00 to 1015.00 feet
 *
 * 3.0 format
 * 
 *    # 2 categories
 *    Map Title
 *    Elevation: $1.2 to $2.2 feet       ## Format Statement
 *    5.0 1000 5.0 1005                  ## Coefficients
 *
 * The coefficient line can be followed by explicit category labels
 * which override the format label generation.
 *    0:no data
 *    2:   .
 *    5:   .                             ## explicit category labels
 *    7:   .
 *
 * In the format line
 *   $1 refers to the value num*5.0+1000 (ie, using the first 2 coefficients)
 *   $2 refers to the value num*5.0+1005 (ie, using the last 2 coefficients)
 *
 *   $1.2 will print $1 with 2 decimal places.
 *
 * Also, the form $?xxx$yyy$ translates into yyy if the category is 1, xxx 
 * otherwise. The $yyy$ is optional. Thus
 *
 *   $1 meter$?s
 *
 * will become: 1 meter (for category 1)
 *              2 meters (for category 2), etc.
 *
 * The format and coefficients above would be used to generate the
 * following statement in creation of the format appropriate category
 * string for category "num":
 *
 *   sprintf(buff,"Elevation: %.2f to %.2f feet", num*5.0+1000, num*5.0*1005)
 *
 * Note: while both the format and coefficent lins must be present
 *       a blank line for the fmt will effectively suppress automatic
 *       label generation
 * 
 **********************************************************************
 *
 *  G_read_cats (name, mapset, pcats)
 *      char *name                   name of cell file
 *      char *mapset                 mapset that cell file belongs to
 *      struct Categories *pcats     structure to hold category info
 *
 *  Reads the category information associated with cell file "name"
 *  in mapset "mapset" into the structure "pcats".
 *
 *  returns:    0  if successful
 *             -1  on fail
 *
 *  note:   a warning message is printed if the file is
 *          "missing" or "invalid".
 **********************************************************************
 *
 *  G_read_vector_cats (name, mapset, pcats)
 *      char *name                   name of vector file
 *      char *mapset                 mapset that vector file belongs to
 *      struct Categories *pcats     structure to hold category info
 *
 *  Reads the category information associated with vector file layer "name"
 *  in mapset "mapset" into the structure "pcats".
 *
 *  returns:    0  if successful
 *             -1  on fail
 *
 *  note:   a warning message is printed if the file is
 *          "missing" or "invalid".
 **********************************************************************
 *
 * char *
 * G_get_cat (num, pcats)
 *      CELL num                     category number
 *      struct Categories *pcats     structure to hold category info
 *
 * Returns pointer to a string describing category.
 *
 **********************************************************************
 *
 * char *
 * G_get_cats_title (pcats)
 *      struct Categories *pcats     structure to hold category info
 *
 * Returns pointer to a string with title
 *
 **********************************************************************
 *
 * G_init_cats (ncats, title, pcats)
 *      CELL ncats                   number of categories
 *      char *title                  cell title
 *      struct Categories *pcats     structure to hold category info
 *
 * Initializes the cats structure for subsequent calls to G_set_cat()
 *
 **********************************************************************
 *
 * G_set_cats_fmt (fmt, m1, a1, m2, a2, pcats)
 *      char *fmt                    user form of the equation format
 *      float m1,a1,m2,a2            coefficients
 *      struct Categories *pcats     structure to hold category info
 *
 * configures the cats structure for the equation. Must be called
 * after G_init_cats().
 *
 **********************************************************************
 *
 * G_set_cats_title (title, pcats)
 *      char *title                  cell file title
 *      struct Categories *pcats     structure holding category info
 *
 * Store title as cell file in cats structure
 * Returns nothing.
 *
 **********************************************************************
 *
 * G_set_cat (num, label, pcats)
 *      CELL num                     category number
 *      char *label                  category label
 *      struct Categories *pcats     structure to hold category info
 *
 * Adds the string buff to represent category "num" in category structure
 * pcats.
 *
 * Returns: -1 too many cats, 1 ok.
 *
 **********************************************************************
 *
 *  G_write_cats (name, pcats)
 *      char *name                   name of cell file
 *      struct Categories *pcats     structure holding category info
 *
 *  Writes the category information associated with cell file "name"
 *  into current mapset from the structure "pcats".
 *
 *   returns:    1  if successful
 *              -1  on fail
 **********************************************************************
 *
 *  G_write_vector_cats (name, pcats)
 *      char *name                   name of vector file
 *      struct Categories *pcats     structure holding category info
 *
 *  Writes the category information associated with vector file "name"
 *  into current mapset from the structure "pcats".
 *
 *   returns:    1  if successful
 *              -1  on fail
 **********************************************************************
 *
 * G_free_cats (pcats)
 *      struct Categories *pcats     structure holding category info
 *
 * Releases memory allocated for the cats structure
 **********************************************************************/

#include "gis.h"
G_read_cats (name, mapset, pcats)
    char *name ;
    char *mapset ;
    struct Categories *pcats ;
{
    char err[100];
    char *type;
    CELL G__read_cats();

    switch (G__read_cats ("cats", name, mapset, pcats, 1))
    {
    case -2:
	    type = "missing";
	    break;
    case -1:
	    type = "invalid";
	    break;
    default:
	    return 0;
    }

    sprintf(err,"category support for [%s] in mapset [%s] %s",
		    name, mapset, type);
    G_warning (err);
    return -1;
}

G_read_vector_cats (name, mapset, pcats)
    char *name ;
    char *mapset ;
    struct Categories *pcats ;
{
    char err[100];
    char *type;
    CELL G__read_cats();

    switch (G__read_cats ("dig_cats", name, mapset, pcats, 1))
    {
    case -2:
	    type = "missing";
	    break;
    case -1:
	    type = "invalid";
	    break;
    default:
	    return 0;
    }

    sprintf(err,"category support for vector file [%s] in mapset [%s] %s",
		    name, mapset, type);
    G_warning (err);
    return -1;
}

CELL
G_number_of_cats (name, mapset)
    char *name ;
    char *mapset ;
{
    struct Categories pcats ;
    CELL G__read_cats();
    return (G__read_cats ("cats", name, mapset, &pcats, 0));
}

CELL
G__read_cats (element, name, mapset, pcats, full)
    char *element;
    char *name ;
    char *mapset ;
    struct Categories *pcats ;
{
    FILE *fd ;
    char buff[1024] ;
    CELL cat;
    int old;
    long num;


    if (!(fd = G_fopen_old (element, name, mapset)))
	return -2 ;

/* Read the number of categories */
    if (G_getl(buff,sizeof buff,fd) == NULL)
	goto error;

    if (sscanf ( buff, "# %ld"   , &num) == 1)
	old = 0;
    else if (sscanf ( buff, "%ld"   , &num) == 1)
	old = 1;
    else
	goto error;
    if (num < 0)
	goto error;
    if (!full)
    {
	fclose (fd);
	return (CELL) num;
    }

/* Read the title for the file */
    if (G_getl(buff,sizeof buff,fd) == NULL)
	goto error;
    G_strip (buff);
/*    G_ascii_check(buff) ; */
    G_init_cats ((CELL)num, buff, pcats);
    if (!old)
    {
	char fmt[256];
	float m1,a1,m2,a2;
	if (G_getl(fmt,sizeof fmt,fd) == NULL)
		goto error;
/* next line contains equation coefficients */
	if (G_getl(buff,sizeof buff,fd) == NULL)
		goto error;
	if(sscanf(buff, "%f %f %f %f", &m1, &a1, &m2, &a2) != 4)
		goto error;
	G_set_cats_fmt (fmt, m1, a1, m2, a2, pcats);
    }

/* Read all category names */
    for (cat=0;;cat++) 
    {
	char label[1024];
	if (G_getl(buff, sizeof buff, fd) == 0)
	    break;
	if (old)
	    G_set_cat (cat, buff, pcats) ;
	else
	{
	    *label = 0;
	    if (sscanf (buff, "%1s", label) != 1)
		continue;
	    if(*label == '#')
		continue;
	    *label = 0;
	    if (sscanf (buff, "%ld:%[^\n]", &num, label) < 1)
		goto error;
	    G_set_cat ((CELL)num, label, pcats);
	}
    }
    G_sort_cats (pcats);

    fclose (fd);
    return 0 ;
error:
    fclose (fd);
    return -1 ;
}

char *
G_get_cats_title (pcats)
    struct Categories *pcats ;
{
    static char *none = "";
    return pcats->title ? pcats->title : none;
}

char *
G_get_cat (num, pcats)
    CELL num ;
    struct Categories *pcats ;
{
    static char label[1024] ;
    char *f, *l, *v;
    int i;
    float a[2];
    char fmt[30], value[30];

/* first search the list of labels */
    *label = 0;
    if (find_cat(num, pcats, &i))
    {
	if (pcats->list[i].label != NULL)
	    return (pcats->list[i].label);
	return label;
    }

/* generate the label */
    if ((f = pcats->fmt) == NULL)
	return label;

    a[0] = num*pcats->m1+pcats->a1 ;
    a[1] = num*pcats->m2+pcats->a2 ;
    l = label;
    while (*f)
    {
	if (*f == '$')
	{
	    f++;
	    if (*f == '$')
		*l++ = *f++;
	    else if (*f == '?')
	    {
		f++;
		get_cond (&f, v = value, num);
		while (*v)
		    *l++ = *v++;
	    }
	    else if (get_fmt (&f, fmt, &i))
	    {
		sprintf (v = value, fmt, a[i]);
		while (*v)
		    *l++ = *v++;
	    }
	    else
		*l++ = '$';
	}
	else
	{
	    *l++ = *f++;
	}
    }
    *l = 0;
    return label;
}

static
get_fmt (f, fmt, i)
    char **f;
    char *fmt;
    int *i;
{
    char *ff;

    ff = *f;
    if (*ff == 0) return 0;
    if (*ff == '$')
    {
	*f = ff+1;
	return 0;
    }
    switch (*ff++)
    {
    case '1': *i = 0; break;
    case '2': *i = 1; break;
    default: return 0;
    }
    *fmt++ = '%';
    *fmt++ = '.';
    if (*ff++ != '.')
    {
	*f = ff-1;
	*fmt++ = '0';
	*fmt++ = 'f';
	*fmt = 0;
	return 1;
    }
    *fmt = '0';
    while (*ff >= '0' && *ff <= '9')
	*fmt++ = *ff++;
    *fmt++ = 'f';
    *fmt = 0;
    *f = ff;
    return 1;
}

static
get_cond (f, value, num)
    char **f;
    char *value;
    CELL num;
{
    char *ff;

    ff = *f;
    if (num == 1)
    {
	while (*ff)
	    if (*ff++ == '$')
		break;
    }

    while (*ff)
	if (*ff == '$')
	{
	    ff++;
	    break;
	}
	else
	    *value++ = *ff++;

    if (num != 1)
    {
	while (*ff)
	    if (*ff++ == '$')
		break;
    }
    *value = 0;
    *f = ff;
}

G_set_cat (num, label, pcats)
    CELL num ;
    char *label ;
    struct Categories *pcats ;
{
    int n;
    if (find_cat (num, pcats, &n))
    {
	if (pcats->list[n].label != NULL)
	    free (pcats->list[n].label) ;
    }
    else
    {
	n = pcats->count++;
	if (pcats->count > pcats->nalloc)
	{
	    long len;
	    long nalloc;

	    nalloc = pcats->nalloc + 256;
	    len = (long) nalloc * sizeof(struct Cat_List) ;
	    if (len != (int) len) /* make sure len doesn't overflow int */
	    {
		pcats->count--;
		return -1;
	    }
	    pcats->list = (struct Cat_List *) G_realloc(pcats->list, (int)len);
	    pcats->nalloc = nalloc;
	}
    }
    pcats->list[n].num = num;
    pcats->list[n].label = G_store(label) ;
    G_newlines_to_spaces (pcats->list[n].label);
    G_strip (pcats->list[n].label);
    if (num > pcats->num)
	pcats->num = num;
    return 1;
}

G_write_cats (name, cats)
    char *name ;
    struct Categories *cats ;
{
    return G__write_cats ("cats", name, cats);
}

G_write_vector_cats (name, cats)
    char *name ;
    struct Categories *cats ;
{
    return G__write_cats ("dig_cats", name, cats);
}

G__write_cats (element, name, cats)
    char *element ;
    char *name ;
    struct Categories *cats ;
{
    FILE *fd ;
    int i,n;

    if (!(fd = G_fopen_new (element, name)))
	return -1;

/* write # cats - note # indicate 3.0 or later */
    fprintf(fd,"# %ld categories\n", (long) cats->num);

/* title */
    fprintf(fd,"%s\n", cats->title!=NULL?cats->title:"") ;

/* write format and coefficients */
    fprintf(fd,"%s\n", cats->fmt!=NULL?cats->fmt:"") ;
    fprintf(fd,"%.2f %.2f %.2f %.2f\n",
	    cats->m1, cats->a1, cats->m2, cats->a2) ;

/* write the cat numbers:label */
    G_sort_cats (cats);
    n = cats->count;
    for (i = 0; i < n; i++)
    {
	if ((cats->fmt && cats->fmt[0])
	||  (cats->list[i].label && cats->list[i].label[0]))
	{
	    fprintf(fd,"%ld:%s\n", (long) cats->list[i].num,
		cats->list[i].label!=NULL?cats->list[i].label:"") ;
	}
    }
    fclose (fd) ;
    return(1) ;
}

G_init_cats (num, title, pcats)
    CELL num;
    char *title;
    struct Categories *pcats;
{
    G_set_cats_title (title, pcats);
    pcats->list = NULL;
    pcats->count = 0;
    pcats->nalloc = 0;
    pcats->num = num < 0 ? 0 : num;
    pcats->fmt = NULL;
    pcats->m1 = 0.0;
    pcats->a1 = 0.0;
    pcats->m2 = 0.0;
    pcats->a2 = 0.0;
}

G_set_cats_title (title, pcats)
    char *title;
    struct Categories *pcats;
{
    if (title == NULL) title="";
    pcats->title = G_store (title);
    G_newlines_to_spaces (pcats->title);
    G_strip (pcats->title);
}

G_set_cats_fmt (fmt, m1, a1, m2, a2, pcats)
    char *fmt;
    float m1,a1,m2,a2;
    struct Categories *pcats;
{
    pcats->m1 = m1;
    pcats->a1 = a1;
    pcats->m2 = m2;
    pcats->a2 = a2;

    pcats->fmt = G_store (fmt);
    G_newlines_to_spaces (pcats->fmt);
    G_strip(fmt);
}

G_sort_cats (pcats)
    struct Categories *pcats;
{
    int cmp();
    if (pcats->count > 1)
	qsort (pcats->list, pcats->count, sizeof (struct Cat_List), cmp);
}

static
cmp (a, b)
    struct Cat_List *a, *b;
{
    if(a->num < b->num)
	return -1;
    if(a->num > b->num)
	return 1;
    return 0;
}

G_free_cats (pcats)
    struct Categories *pcats;
{
    int i;

    if (pcats->title != NULL)
    {
	free (pcats->title);
	pcats->title = NULL;
    }
    if (pcats->fmt != NULL)
    {
	free (pcats->fmt);
	pcats->fmt = NULL;
    }
    if (pcats->count > 0)
    {
	for (i = 0; i < pcats->count; i++)
	    if (pcats->list[i].label != NULL)
		free (pcats->list[i].label);
	free (pcats->list);
	pcats->list = NULL;
    }
    pcats->count = 0;
    pcats->num = 0;
    pcats->nalloc = 0;
}

static
find_cat (num, pcats, i)
    CELL num;
    struct Categories *pcats;
    int *i;
{
    int n;
    for (n = 0; n < pcats->count; n++)
	if (pcats->list[n].num == num)
	{
		*i = n;
		return 1;
	}
    return 0;
}
