#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "gis.h"
#include "ps_map.h"

/*
int ncolors;
*/
int masking_on;
char *LOCATION;
char *MAPSET;
char *MASK;
char *recordfile;

int main (int argc, char *argv[])
{
    FILE *scriptfd, *out_fp;
    char scriptfile[128];
    char buf[256];
    char ps_outfile[128];
    char dummy[4];
    int cf;
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
	    fprintf (stdout,"WARNING: illegal STARTPANEL(%s) - ignored\n", env);
	    sleep(3);
	}
    }
    if (env = getenv("ENDPANEL"))
    {
	if (sscanf(env, "%d", &n) == 1 || n > 0)
	    fprintf(scriptfd, "endpanel %d\n", n);
	else
	{
	    fprintf (stdout,"WARNING: illegal ENDPANEL(%s) - ignored\n", env);
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
	fprintf (stdout,"\nenter name of PostScript output file: ");
    	fgets(buf,256,stdin);
    	G_strip(buf);
    	if (strlen(buf) == 0)
	{
	    fprintf (stdout,"** no file selected **\n");
	    continue;
	}
    	if (sscanf(buf, "%s%1s", ps_outfile, dummy) != 1) 
	{
	    fprintf (stdout,"** invalid file name **\n");
	    continue;
	}
    	if ((out_fp = fopen(ps_outfile, "w")) == NULL)
	{
	    fprintf (stdout,"\ncan't open %s\n", ps_outfile);
	    continue;
	}
    	fclose(out_fp);
	break;
    }

    fprintf (stdout,"\n");
    if (yes("do you want to save the script file"))
    {
	char save_name[128];

	fprintf (stdout,"\nenter name for script file: ");
	fgets(save_name,128,stdin);
        G_squeeze(save_name); /* added to remove \n, RB Jan 2000 */ 
 	strcpy(buf, "mv ");
	strcat(buf, scriptfile);
	strcat(buf, " ");
	strcat(buf, save_name);
	system(buf);
	strcpy(scriptfile, save_name);
    }

    fprintf (stdout,"\n");
    hitreturn();

    PSmap(scriptfile, ps_outfile, background);

    return 0;
}

