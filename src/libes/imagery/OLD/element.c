/***********************************************************
* I_group_element (dir, element)
*
* build the name of the database imagery group element given
* the lower level directory name
*
* the group database resides under the mapset in group
* and the dir is tacked on the end: group/<dir>
*
* notes:
*  dir a simple name refers to the group as a whole
*  complex directory names refer to sub elements under the group
*  dir == "" returns "group" (good for listing groups)
************************************************************/
I_group_element (dir, element)
    char *dir;
    char *element;
{
    if (*dir)
	sprintf (element, "group/%s", dir);
    else
	sprintf (element, "group");
    G__make_mapset_element (element);
}

I_subgroup_element (group, dir, element)
    char *group;
    char *dir;
    char *element;
{
    if (*dir)
	sprintf (element, "group/%s/subgroup/%s", group, dir);
    else
	sprintf (element, "group/%s/subgroup", group);
    G__make_mapset_element (element);
}
