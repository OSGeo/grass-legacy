/* this routine
 *  makes sure that pt1 and pt2 share same attribitues
 *   (using the list of fields from command line)
 *
 */

#include "glob.h"

static int *field_list = NULL;
static int nfields = 0;

init_attributes (f)
    char **f;
{
    nfields = 0;
    if (f)
	field_list = (int *) get_field_list (f, &nfields);
}

attributes_are_the_same (pt1, pt2)
    int pt1, pt2;
{
    char buf1[4096], buf2[4096];
    char word1[1024], word2[1024];
    int f;

    if(have_attributes())
    {
	read_point (pointlist.fd, pointlist.offset[pt1], buf1, sizeof buf1);
	read_point (pointlist.fd, pointlist.offset[pt2], buf2, sizeof buf2);
	for (f = 0; f < nfields; f++)
	{
	    if (!get_field (buf1, field_list[f], pointlist.fs, word1))
		return 0;
	    if (!get_field (buf2, field_list[f], pointlist.fs, word2))
		return 0;
	    if (strcmp (word1, word2) != 0)
		return 0;
	}
    }
    return 1;
}

have_attributes()
{
    return nfields > 0;
}
