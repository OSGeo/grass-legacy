/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int cell_open(name,mapset)

	To open an old map with name and in the mapset and returns the
	file id.
*/

int cell_open(name,mapset)
char *name;
char *mapset;
{
    int fd;
    char buf[100];


    if ((fd = G_open_cell_old (name, mapset)) < 0)
    {
	sprintf(buf, "unable to open file [%s]\n", name);
	G_fatal_error(buf);
	exit(0);
    }

    return(fd);
}
