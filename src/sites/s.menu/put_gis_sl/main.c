#include "site_dir.h"
#include "gis.h"
#include "site.h"
main(argc, argv)    char *argv[];
{
    char name[100];
    FILE *fd;
    char desc[200];
    char temp[3];
    SITE_LIST site_list;

    if (argc != 2)
        exit(-1);

    G_gisinit (argv[0]);
    initialize_site_list (&site_list);
    if (!get_site_list (&site_list, argv[1]))
        exit (-1);

    if (!G_ask_any ("enter a site list name", name, SITE_DIR, "site list", NULL))
        exit (0);
    
    fd = G_fopen_new (SITE_DIR, name);
    if (!fd)
    {
        printf ("\n** can't create site list file %s\n", name);
        exit(-1);
    }

    printf("\n");
    do
    {
        if (site_list.desc[0] != 0)
        {
	    printf("<%s> description: %s\n\n", name, site_list.desc);
	    printf("if you want to retain this description, hit RETURN\n");
	    printf("otherwise enter a new description: ");
        }
        else
	    printf("enter a description for <%s>: ", name);

    }
    while (!G_gets(desc));

    if (sscanf (desc, "%1s", temp) == 1)
        strcpy (site_list.desc, desc);

    strcpy (site_list.name, name);

    fd = G_fopen_new (SITE_DIR, name);
    if (!fd)
    {
        printf ("\n** can't create site list file %s\n", name);
        exit(-1);
    }

    if(write_site_list (&site_list, fd, 0, 0))
        printf("<%s> site list saved\n", name);
    fclose (fd);
    exit(1);    /* driver will ask hitreturn */
}
