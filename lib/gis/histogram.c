#include "gis.h"
#include "glocale.h"
#include <stdlib.h>

#define LIST struct Histogram_list

static FILE *fopen_histogram_new(char *);
static int cmp(const void *, const void *);
static int cmp_count (const void *, const void *);

/*!
 * \brief initializes the histogram structure
 * 
 * initializes the histogram structure for calls to G_set_histogram()
 * and G_add_histogram()
 * \param  histogram
 * \return int
 */

int G_init_histogram (
    struct Histogram *histogram)
{
    histogram->num = 0;
    histogram->list = NULL;

    return 0;
}

/*!
 * \brief read the histogram information
 *
 *  Reads the histogram information associated with map layer "map"
 *  in mapset "mapset" into the structure "histogram".
 *
 *  note:   a warning message is printed if the file is missing or incorrect
 * \param name: name of map
 * \param mapset: mapset that map belongs to 
 * \param histogram: struct for histogram
 * \return 1  if successful,
 *               0  if no histogram file,
 *              -1  on fail
 */

int G_read_histogram (
    char *name,char *mapset,
    struct Histogram *histogram)
{
    FILE *fd;
    long cat;
    long count;
    char buf[200];

    fd = NULL;

    G_init_histogram (histogram);

    sprintf (buf,"cell_misc/%s", name);
    if (G_find_file (buf, "histogram", mapset) == NULL)
    {
	sprintf (buf, _("Histogram for [%s in %s] missing (run r.support)"), name, mapset);
	G_warning (buf);
	return 0;
    }
    fd = G_fopen_old (buf, "histogram", mapset);
    if (!fd)
    {
	sprintf (buf, _("Can't read histogram for [%s in %s]"), name, mapset);
	G_warning (buf);
	return -1;
    }

    while (fgets (buf, sizeof buf, fd))
    {
	if (sscanf (buf, "%ld:%ld", &cat, &count) != 2)
	{
	    G_free_histogram (histogram);
	    fclose (fd);
	    sprintf (buf,_("Invalid histogram file for [%s in %s]"), name, mapset);
	    G_warning (buf);
	    return -1;
	}
	G_extend_histogram ((CELL)cat, count, histogram);
    }
    fclose (fd);
    if (histogram->num == 0)
    {
	sprintf (buf,_("Invalid histogram file for [%s in %s]"), name, mapset);
	G_warning (buf);
	return -1;
    }

    G_sort_histogram  (histogram);
    return 1;
}

/*!
 * \brief Writes the histogram information
 *
 *  Writes the histogram information associated with map layer "name"
 * \param name: name of map
 * \param histogram: struct for histogram
 * \return  1  if successful,
 *              -1  on fail
 */

int G_write_histogram (
    char *name,
    struct Histogram *histogram)
{
    FILE *fd;
    int n;
    LIST *list;

    fd = fopen_histogram_new (name);
    if (fd == NULL)
	return -1;

    list = histogram->list;
    for (n = 0; n < histogram->num; n++)
    {
	if (list[n].count)
	    fprintf (fd, "%ld:%ld\n", (long)list[n].cat, list[n].count);
    }
    fclose (fd);
    return 1;
}

/*!
 * \brief Writes the histogram information to file
 *
 * \param name: name of map
 * \param statf
 * \return 
 */

int G_write_histogram_cs (
    char *name,
    struct Cell_stats *statf)
{
    FILE *fd;
    CELL cat;
    long count;

    fd = fopen_histogram_new (name);
    if (fd == NULL)
	return -1;
    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
    {
	if (count > 0)
	    fprintf (fd, "%ld:%ld\n", (long)cat, count);
    }
    fclose (fd);
    return 1;
}

/*!
 * \brief 
 *
 * \param 
 * \return 
 */
int G_make_histogram_cs (
    struct Cell_stats *statf,
    struct Histogram *histogram)
{
    CELL cat;
    long count;

    G_init_histogram (histogram);
    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
	G_add_histogram (cat, count, histogram);
    G_sort_histogram (histogram);

    return 0;
}

/*!
 * \brief Sorts the histogram in ascending order by counts then category
 *
 *  Sorts the histogram in ascending order by counts then category.
 *  No combining is done.
 * \param histogram: struct for histogram
 * \return  1  if successful,
 *              -1  on fail
 */
int G_get_histogram_num (struct Histogram *histogram)
{
    return histogram->num;
}

/*!
 * \brief Returns cat for the nth element in the histogram
 *
 *  Returns cat for the nth element in the histogram
 * \param histogram: struct for histogram
 * \return CELL
 */
CELL G_get_histogram_cat (int n, struct Histogram *histogram)
{
    if (n < 0 || n >= histogram->num)
	return 0;
    return histogram->list[n].cat;
}

/*!
 * \brief Returns count for the nth element in the histogram
 *
 *  Returns count for the nth element in the histogram
 * \param n: nth element
 * \param histogram: struct for histogram
 * \return count
 */
long G_get_histogram_count (int n, struct Histogram *histogram)
{
    if (n < 0 || n >= histogram->num)
	return 0;
    return histogram->list[n].count;
}

/*!
 * \brief frees the memory allocated for the histogram
 *
 * frees the memory allocated for the histogram
 * \param histogram: struct for histogram
 * \return 
 */
int G_free_histogram ( struct Histogram *histogram)
{
    if (histogram->num > 0)
	free (histogram->list);
    histogram->num = 0;
    histogram->list = NULL;
    return 1;
}

/*!
 * \brief Sorts the histogram
 *
 *  Sorts the histogram in ascending order by category,
 *  combining (by adding) elements that have the same category.
 * \param histogram: struct for histogram
 * \return  0  if successful,
 *              1  on fail
 */
int G_sort_histogram ( struct Histogram *histogram)
{
    int a,b,n;
    LIST *list;

/* if histogram only has 1 entry, nothing to do */
    if ((n = histogram->num) <= 1) return 1;

    list=histogram->list;

/* quick check to see if sorting needed */
    for (a = 1; a < n; a++)
	if (list[a-1].cat >= list[a].cat)
	    break;
    if (a >= n) return 1;

/* sort */
    qsort (list, n, sizeof(LIST), &cmp);

/* sum duplicate entries */
    for (a = 0, b = 1; b < n; b++)
	if (list[a].cat != list[b].cat)
	{
	    a++;
	    list[a].count = list[b].count;
	    list[a].cat   = list[b].cat;
	}
	else
	{
	    list[a].count += list[b].count;
	}
    histogram->num = a + 1;

    return 0;
}

static int cmp(const void *aa, const void *bb)
{
    const LIST *a = aa, *b = bb;
    if (a->cat < b->cat)
	return -1;
    if (a->cat > b->cat)
	return 1;
    return 0;
}

/*!
 * \brief Sorts the histogram by counts
 *
 *  Sorts the histogram in ascending order by counts then category.
 *  No combining is done.
 * \param histogram: struct for histogram
 * \return  0  if successful,
 *              1  on fail
 */
int G_sort_histogram_by_count ( struct Histogram *histogram)
{
    int n;
    LIST *list;

/* if histogram only has 1 entry, nothing to do */
    if ((n = histogram->num) <= 1) return 1;

    list=histogram->list;

/* sort */
    qsort (list, n, sizeof(LIST), &cmp_count);

    return 0;
}

static int cmp_count(const void *aa, const void *bb)
{
    const LIST *a = aa, *b = bb;
    if(a->count < b->count)
	return -1;
    if(a->count > b->count)
	return 1;
    if(a->cat < b->cat)
	return -1;
    if(a->cat > b->cat)
	return 1;
    return 0;
}

static FILE *fopen_histogram_new ( char *name)
{
    char buf[100];
    FILE *fd;

    sprintf (buf,"cell_misc/%s", name);
    fd = G_fopen_new (buf, "histogram");
    if (fd == NULL)
    {
	sprintf (buf,_("can't create histogram for [%s in %s]"), name, G_mapset());
	G_warning (buf);
    }
    return fd;
}

/*!
 * \brief Removes the histogram
 *
 *  Removes the histogram information associated with map layer "name"
 * \param name: name of map
 * \return 0
 */

int G_remove_histogram(name)
    char *name;
{
    char buf[100];
    sprintf (buf,"cell_misc/%s", name);
    G_remove(buf, "histogram");

    return 0;
}

/*!
 * \brief adds count to the histogram value for cat
 *
 *  adds count to the histogram value for cat
 * \param cat: category
 * \param count
 * \param histogram: struct for histogram
 * \return 0  if successful,
 *              1  on fail
 */
int G_add_histogram (
    CELL cat,
    long count,
    struct Histogram *histogram)
{
    int i;

    for (i = 0 ; i < histogram->num; i++)
	if (histogram->list[i].cat == cat)
	{
	    histogram->list[i].count += count;
	    return 1;
	}

    G_extend_histogram (cat, count, histogram);

    return 0;
}

/*!
 * \brief sets the histogram value for cat to count
 *
 *  sets the histogram value for cat to count
 * \param cat: category
 * \param count
 * \param histogram: struct for histogram
 * \return 0  if successful,
 *              1  on fail
 */
int G_set_histogram (
    CELL cat,
    long count,
    struct Histogram *histogram)
{
    int i;

    for (i = 0 ; i < histogram->num; i++)
	if (histogram->list[i].cat == cat)
	{
	    histogram->list[i].count = count;
	    return 1;
	}

    G_extend_histogram (cat, count, histogram);

    return 0;
}

/*!
 * \brief 
 *
 * \param cat: category
 * \param count
 * \param histogram: struct for histogram
 * \return 
 */
int G_extend_histogram (
    CELL cat,
    long count,
    struct Histogram *histogram)
{
    histogram->num++;
    histogram->list =
	(LIST *) G_realloc ((char *) histogram->list, histogram->num*sizeof (LIST));
    histogram->list[histogram->num-1].cat = cat;
    histogram->list[histogram->num-1].count = count;

    return 0;
}

/*!
 * \brief 
 *
 * \param histogram: struct for histogram
 * \return 
 */
int G_zero_histogram ( struct Histogram *histogram)
{
    int i;

    for (i = 0; i < histogram->num; i++)
	histogram->list[i].count = 0;

    return 0;
}
