#include "gis.h"

int ncolors;
int masking_on;
char *LOCATION;
char *MAPSET;
char *MASK;
char *recordfile;

main(argc,argv) char *argv[];
{
    FILE *scriptfd;
    char *scriptfile;
    int cf;
    char *env, *getenv();
    int n;
    int background;

    G_gisinit (argv[0]);

    recordfile = G_tempfile();
    if((scriptfd = fopen(recordfile,"w")) != NULL)
    {
	fclose (scriptfd); /* use scriptfd as temp fd */
	scriptfd = NULL;
    }

    LOCATION = G_location();
    MAPSET   = G_mapset();
    MASK     = G_mask_info();

    masking_on = strcmp (MASK,"none") != 0 ;

    Pconnect();
    Plock();
    ncolors = Pncolors();

    scriptfile = G_tempfile();
    if ((scriptfd = fopen (scriptfile, "w")) == NULL)
    {
	perror (scriptfile);
	exit(1);
    }

    if (env = getenv ("STARTPANEL"))
    {
	if (sscanf (env, "%d", &n) == 1 || n > 0)
	    fprintf (scriptfd, "startpanel %d\n", n);
	else
	{
	    printf ("WARNING: illegal STARTPANEL(%s) - ignored\n", env);
	    sleep(3);
	}
    }
    if (env = getenv ("ENDPANEL"))
    {
	if (sscanf (env, "%d", &n) == 1 || n > 0)
	    fprintf (scriptfd, "endpanel %d\n", n);
	else
	{
	    printf ("WARNING: illegal ENDPANEL(%s) - ignored\n", env);
	    sleep(3);
	}
    }


    newscreen (0);
    cf = ask_cell (scriptfd);
    ask_vectors (scriptfd);
    ask_sites (scriptfd);
    ask_labels (scriptfd);
    newscreen (1);
    ask_scale (scriptfd);
    ask_grid (scriptfd);
    newscreen (1);
    ask_legend (scriptfd,cf);
    newscreen (1);
    background = ask_background (scriptfd);

    fclose (scriptfd);
    unlink (recordfile);

    hitreturn();
    Pdisconnect();
    Pmap (scriptfile, background);
}
