#include <string.h>
#include <stdlib.h>
#include "imagery.h"
#include "vask.h"
#include "gis.h"

#define MAX 11
static char prefix[100];
static int prefix_ok(int *,int);
static int ask_bandname_prefix(int *,int);

char *I_bandname (int n)
{
    static char name[30];

    sprintf (name, "%s.%d",prefix,n+1);
    return name;
}

char *I_bandname_prefix()
{
    return prefix;
}

int I_set_band_prefix(char *p)
{
	strcpy (prefix, p);

	return 0;
}

int *I_ask_bands (int nbands)
{
    char **x;
    int *num;
    int i;
    int line, col;
    int any;

    V_clear ();
    V_line (1, "Please mark an x by the bands you want extracted");

    num = (int *) G_malloc (nbands * sizeof(int));
    x = (char **) G_malloc (nbands * sizeof(char *));

    for (i = 0; i < nbands; i++)
    {
	line = i%15 + 3;
	col  = (i/15)*7 + 2;
	x[i] = G_malloc (2);
	x[i][0] = 0;
	if (i > 35) continue;	/* limit in V_const() is 40 */
	num[i] = i+1;
	V_const (&num[i], 'i', line, col+2, 4);
	V_ques (x[i], 's', line, col, 1);
    }
    I_v_exec();

    any = 0;
    for (i = 0; i < nbands; i++)
    {
	if (x[i][0])
	{
	    num[i] = 1;
	    any = 1;
	}
	else
	    num[i] = 0;
	free (x[i]);
    }
    free (x);
    if (!any)
    {
	fprintf (stderr,"no bands selected\n");
	exit(0);
    }

    ask_bandname_prefix (num, nbands);
    return num;
}

static int ask_bandname_prefix(int *num,int nbands)
{
    for(;;)
    {
	if (!I_ask_group_any ("select a prefix/group for the band cell files to be created",prefix))
	    exit(0);
	if (strlen (prefix) > MAX)
	    fprintf (stderr,"\n** prefix too long. %d chars maximum\n",MAX);
	else if (prefix_ok(num,nbands))
	    break;
    }

	return 0;
}

static int prefix_ok(int *num,int nbands)
{
    int i;
    int any;
    char *name;

    any = 0;
    for (i=0; i < nbands; i++)
    {
	if (num[i] && G_find_cell (name = I_bandname(i), G_mapset()))
	{
	    if (!any)
	    {
		fprintf (stderr,"\n\n** the following cell files already in exist your mapset\n\n");
		any = 1;
	    }
	    fprintf (stderr," %s", name);
	}
    }
    if (!any) return 1;

    fprintf (stderr,"\n\nIf you proceed, these files will be overwritten. ");
    return G_yes("Proceed? ", -1);
}
