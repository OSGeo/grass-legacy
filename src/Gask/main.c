#include "gis.h"

#define PGM argv[0]
#define TYPE argv[1]
#define PROMPT argv[2]
#define ELEMENT argv[3]
#define DESC argv[4]
#define OUT argv[5]

main(argc,argv) char *argv[];
{
    char file[1024], name[40], *mapset;
    char *prompt;
    FILE *fd;
    G_gisinit (PGM);

    if (argc != 6)
	usage(PGM);


    fd = fopen (OUT, "w");
    if (fd == NULL)
    {
	fprintf (stderr, "%s - ", PGM);
	perror (OUT);
	exit(1);
    }
    if (strcmp (prompt=PROMPT,"-")==0)
	prompt = "";

    if (strcmp (TYPE, "old") == 0)
	mapset = G_ask_old (prompt, name, ELEMENT, DESC);
    else if (strcmp (TYPE, "new") == 0)
	mapset = G_ask_new (prompt, name, ELEMENT, DESC);
    else if (strcmp (TYPE, "any") == 0)
	mapset = G_ask_any (prompt, name, ELEMENT, DESC, 0);
    else if (strcmp (TYPE, "mapset") == 0)
	mapset = G_ask_in_mapset (prompt, name, ELEMENT, DESC);
    else
	usage(PGM);
    if (mapset)
    {
	fprintf (fd, "name='%s'\n",name);
	fprintf (fd, "mapset='%s'\n",mapset);
	G__file_name (file, ELEMENT, name, mapset);
	fprintf (fd, "file='%s'\n",file);
	G__make_mapset_element (ELEMENT);
    }
    else
    {
	fprintf (fd, "name=\n");
	fprintf (fd, "mapset=\n");
	fprintf (fd, "file=\n");
    }
    fclose (fd);
    exit(0);
}
usage(me) char *me;
{
    fprintf (stderr, "usage: %s type prompt element description file\n", me);
    fprintf (stderr, "where type is one of [old,new,any,mapset]\n");
    exit(1);
}
