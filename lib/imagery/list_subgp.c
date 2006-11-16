#include <string.h>
#include <grass/imagery.h>

/*!
 * \brief Prints maps in a subgroup (fancy version)
 *
 * \param group group name
 * \param subgroup subgroup name
 * \param ref group reference (set with I_get_subgroup_ref())
 * \param fd where to print (typically stdout)
 * \return 0
 */
int I_list_subgroup (
    char *group,
    char *subgroup,
    struct Ref *ref,
    FILE *fd)
{
    char buf[80];
    int i;
    int len, tot_len;
    int max;

    if (ref->nfiles <= 0)
    {
	fprintf (fd, "subgroup [%s] of group [%s] is empty\n",
	    subgroup, group);
	return 0;
    }
    max = 0;
    for (i=0; i < ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", ref->file[i].name, ref->file[i].mapset);
	len = strlen(buf)+4;
	if (len > max) max = len;
    }
    fprintf (fd, "subgroup [%s] of group [%s] references the following cellfiles\n", subgroup, group);
    fprintf (fd, "-------------\n");
    tot_len = 0;
    for (i=0; i < ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", ref->file[i].name, ref->file[i].mapset);
	tot_len += max;
	if (tot_len > 78)
	{
	    fprintf (fd, "\n");
	    tot_len = max;
	}
	fprintf (fd, "%-*s", max, buf);
    }
    if (tot_len)
	fprintf (fd, "\n");
    fprintf (fd, "-------------\n");

    return 0;
}

/*!
 * \brief Prints maps in a subgroup (simple version)
 *
 * Same as I_list_subgroup(), but without all the fancy stuff.
 * Prints one map per line in map@mapset form.
 *
 * \param ref group reference (set with I_get_subgroup_ref())
 * \param fd where to print (typically stdout)
 * \return 0
 */
/* same as above, but one map per line in map@mapset form */
int I_list_subgroup_simple(struct Ref *ref, FILE *fd)
{
    return I_list_group_simple(ref, fd);
}
