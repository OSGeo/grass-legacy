#include "gis.h"

/*
int ncolors;
*/
int masking_on;
char *LOCATION;
char *MAPSET;
char *MASK;
char *recordfile;

main(argc, argv) 
int argc;
char *argv[];
{
    FILE *scriptfd, *out_fp;
    char scriptfile[128];
    char buf[256];
    char *env, *getenv();
    char ps_outfile[128];
    char dummy[4];
    int cf;
    int n;
    int background;

    G_gisinit(argv[0]);

    recordfile = G_tempfile();
    if ((scriptfd = fopen(recordfile, "w")) != NULL)
    {
	fclose(scriptfd); /* use scriptfd as temp fd */
	scriptfd = NULL;
    }

    LOCATION = G_location();
    MAPSET   = G_mapset();
    MASK     = G_mask_info();

    masking_on = strcmp(MASK, "none") != 0;

    strcpy(scriptfile, G_tempfile());
    if ((scriptfd = fopen(scriptfile, "w")) == NULL)
    {
	perror(scriptfile);
	exit(1);
    }

/*
    if (env = getenv("STARTPANEL"))
    {
	if (sscanf(env, "%d", &n) == 1 || n > 0)
	    fprintf(scriptfd, "startpanel %d\n", n);
	else
	{
	    printf("WARNING: illegal STARTPANEL(%s) - ignored\n", env);
	    sleep(3);
	}
    }
    if (env = getenv("ENDPANEL"))
    {
	if (sscanf(env, "%d", &n) == 1 || n > 0)
	    fprintf(scriptfd, "endpanel %d\n", n);
	else
	{
	    printf("WARNING: illegal ENDPANEL(%s) - ignored\n", env);
	    sleep(3);
	}
    }
*/


    newscreen(0);
    ask_header(scriptfd);
    cf = ask_cell(scriptfd);
    ask_vectors(scriptfd);
    ask_sites(scriptfd);
    ask_labels(scriptfd);
    newscreen(1);
    ask_scale(scriptfd);
    ask_grid(scriptfd);
    newscreen(1);
    ask_legend(scriptfd, cf);
    fprintf(scriptfd, "end\n");
    newscreen(1);
    background = ask_background(scriptfd);
    fclose(scriptfd);
    unlink(recordfile);


    while (1)
    {
	printf("\nenter name of PostScript output file: ");
    	gets(buf);
    	G_strip(buf);
    	if (strlen(buf) == 0)
	{
	    printf("** no file selected **\n");
	    continue;
	}
    	if (sscanf(buf, "%s%1s", ps_outfile, dummy) != 1) 
	{
	    printf("** invalid file name **\n");
	    continue;
	}
    	if ((out_fp = fopen(ps_outfile, "w")) == NULL)
	{
	    printf("\ncan't open %s\n", ps_outfile);
	    continue;
	}
    	fclose(out_fp);
	break;
    }

    printf("\n");
    if (yes("do you want to save the script file"))
    {
	char save_name[128];

	printf("\nenter name for script file: ");
	gets(save_name);
 	strcpy(buf, "mv ");
	strcat(buf, scriptfile);
	strcat(buf, " ");
	strcat(buf, save_name);
	system(buf);
	strcpy(scriptfile, save_name);
    }

    printf("\n");
    hitreturn();

    PSmap(scriptfile, ps_outfile, background);
}

