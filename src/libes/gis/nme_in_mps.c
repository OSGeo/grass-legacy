/*****************************************************************
 * G__name_in_mapset (name_in, name_out, mapset)
 *
 * checks to see if 'name_in' is in the format: <name> in <mapset>
 *
 * returns
 *    1 (TRUE)  name_in is in this format.
 *              name_out will contain the simple <name>
 *              mapset will contain <mapset>
 *    0 (FALSE) name_in is not in this format
 *              name_out and mapset are undefined (changed)
 ****************************************************************/

G__name_in_mapset (name_in, name_out, mapset)
    char *name_in;
    char *name_out;
    char *mapset;
{
    char in[20];

    *in = 0;
    return (sscanf (name_in,"%s %s %s", name_out, in, mapset) == 3 &&
	    strcmp (in,"in") == 0);
}
