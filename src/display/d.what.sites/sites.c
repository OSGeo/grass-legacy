#include "stdio.h"
#include "stdlib.h"
#include "sites.h"

static int Snum;
static FILE *Sitefd;
static Site **Sites = NULL; 
static struct Cell_head *Wind;

int G_get_site (FILE *inputstream, double *x, double *y, char **desc);

#define SITE_BLOCK 512

int open_sites(char *sname)
{
    char *site_map;
    
        fprintf(stderr, "\n");

        if ((site_map = G_find_file2 ("site_lists", sname, "")) == NULL){
	    fprintf (stderr,  "Could not find file '%s'\n", sname);
	    return 0;
	}

	if ((Sitefd = G_fopen_sites_old (sname, site_map)) == NULL){
	    fprintf (stderr, "can't open sites file [%s]\n", sname);
	    return 0;
	}
	fprintf(stderr,"loading %s ...\n", sname);
        fflush(stderr);

	return 1;
}

static Site *site_get(FILE *inputstream)
{
    char *desc;
    double x, y;
    Site *s;

    if (G_get_site (inputstream, &x, &y, &desc) <= 0) return NULL;
    if ((s = malloc(sizeof(Site))) == NULL ||
        (s->desc = malloc(strlen(desc)+1)) == NULL) {
        if (s != NULL) free(s);
        fprintf(stderr,"Site memory allocation failed\n");
        return NULL;
    }
    s->x = x;
    s->y = y;
    strcpy(s->desc, desc);
    return s;
}

int load_sites(struct Cell_head *wind, int verbose)
{
    int s_alloc=0, outside=0;
    Site *s;

    Wind = wind;

    Snum = 0;

    while ((s = site_get (Sitefd)) != NULL) {
	if (s->x >= wind->west  && s->x <= wind->east  &&
            s->y >= wind->south && s->y <= wind->north) {
	    if (Snum == s_alloc) {
		s_alloc += SITE_BLOCK;
		if((Sites = (Site **)realloc(Sites, s_alloc * sizeof(Site *))) == NULL){
                    fprintf(stderr,"Site memory allocation failed\n");
		    return 0;
		}    
	    }
	    Sites[Snum++] = s;
	} else {
            free(s->desc);
            free(s);
	    outside++;
	}
    }
    
    if(verbose){
	fprintf(stderr,"Total sites inside  region: %d\n", Snum);
	fprintf(stderr,"Total sites outside region: %d\n", outside);
    }

    return 1;
}

void free_sites(void)
{
    int i;

    for (i = 0; i < Snum; i ++) {
        free(Sites[i]->desc);   
        free(Sites[i]);   
    }
    free(Sites);
}

Site *closest_site(double pick_e, double pick_n)
{
    int i;
    double dsq, mdsq, de, dn;
    int idx = -1;

    /* pick_e already "adjusted? */

    for (i = 0 ; i < Snum; i++){
	de = pick_e - G_adjust_easting (Sites[i]->x, Wind);
	dn = pick_n - Sites[i]->y;
	dsq = de * de + dn * dn; 
	if( idx < 0 || dsq < mdsq){
	    mdsq = dsq;
	    idx = i;
	}
    }

    return Sites[idx];
}

char *site_format(Site *s)
{
  return s->desc;
}

