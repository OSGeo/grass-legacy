/* Adapted from SG3d
** Uses cache of sites in region now instead of rereading 
**   file on each display.
*/

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "site.h"
#include "local_proto.h"

#define SITE_BLOCK 512
#define MAX_ST_ATTS 20

/* 
 ***********************************************************
 ********************* OPTIONS *****************************
*/

/* report the min & max string lengths of string attributes */
#define REPORT_STR

/* try to read label information from header */
#define TRY_LABELS

/* Number of sites to cache (currently either all if CacheSites > 0 
   or else none) - no need to cache for s.info since only read 
   them once anyway, unless you want more stats (q1,med,q3) */
static int CacheSites=0;
/* 
static int CacheSites=100000;
*/

/* report Quartiles - MUST USE CACHING */
static int Do_Q = 0;

/*
***********************************************************
*/

extern struct Cell_head    Wind;

double fabs();
double atof();

static Site_head Shd;
static Site **CurSites;
static Site *Sstat_min, *Sstat_max;
static FILE *Sitefd;

static Site **TmpSites, *Sstat_q1, *Sstat_med, *Sstat_q3;

static int Site_file = 0;
static int Shh=0;
static int NSites=0;
static int Snumstrings, Snumvals;

static char Sname[128];

int 
set_quartiles (int do_q)
{
    Do_Q = do_q;
    if(Do_Q) CacheSites=100000;   /* must use caching! */
    return(Do_Q);
}

int 
set_quiet (int shh)
{
    Shh = shh;
    return(Shh);
}

/*
 ************************************************************
 * loads & caches sites, adding them to stats as it goes 
 * If in_region set, will only select those in current region
 ************************************************************
*/
int load_cached_sites (FILE *sfd, int in_region)
{
int ndim, nstr, ndec, ret, badformat=0;
RASTER_MAP_TYPE rtype;
int s_alloc=0, snum=0, outside=0, tot_mem=0;

    rtype = -1;
    G_site_describe (sfd, &ndim, &rtype, &nstr, &ndec);
    if(!Shh){
	fprintf(stderr,"NDIM=%d, RTYPE = %d, NSTR=%d, NDEC=%d\n", 
		ndim, rtype, nstr, ndec );
    }

    /* use these for allocation */

    if((CurSites = (Site **)malloc(SITE_BLOCK*sizeof(Site *)))
	     == NULL){
	fprintf(stderr,"site malloc failed-not enough memory");
	return(0);
    }
    s_alloc = SITE_BLOCK;

    CurSites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);

    while((ret = G_site_get (sfd, CurSites[snum])) != -1){

	if(-2 == ret){
	    badformat++;
	    fprintf(stderr, "Bad format found for site: #%i (ignored)\n", snum + 1);
	    continue;
	}
	
	if( !in_region || G_site_in_region (CurSites[snum], &Wind) ){

	    if(nstr) compress_cached_site(CurSites[snum]);
	    addto_stats(CurSites[snum], !snum);
	    tot_mem += site_mem(CurSites[snum]);
	    snum++;

	    if(snum > CacheSites){  
	    /* currently CacheSites # is not enforced */
	    }
	    
	    if (snum == s_alloc){   /* need more memory */
		
		if((CurSites = (Site **)realloc(CurSites,
			(s_alloc + SITE_BLOCK)*sizeof(Site *))) ==NULL){
		    fprintf(stderr,"site malloc failed-not enough memory");
		    return(0);
		}    
		s_alloc += SITE_BLOCK;
	    }
	    CurSites[snum] = G_site_new_struct (rtype, ndim, nstr, ndec);
	    if(!Shh)
		if(!(snum%1000)) fprintf(stderr,"%10d\b\b\b\b\b\b\b\b\b\b", snum);
	}
	else{
	    outside++;
	}
    }
    G_site_free_struct (CurSites[snum]);
    NSites = snum;

    if(!Shh){
	fprintf(stderr,"Total sites cached: %d\n", NSites);
	fprintf(stderr,"Minimum sites memory used: %.3f Kb\n", tot_mem/1000.);
	fprintf(stderr,"Total sites outside region: %d\n", outside);
	if(badformat)
	    fprintf(stderr,"Total sites ignored (format mismatch): %d\n", 
		    badformat);
    }

    if(Do_Q)
    {
    int i,j;

    if((TmpSites = (Site **)malloc(NSites*sizeof(Site *))) == NULL){
	fprintf(stderr,"site malloc failed-not enough memory");
	return(0);
    }
    Sstat_q1 = G_site_new_struct (rtype, ndim, nstr, ndec);
    Sstat_med = G_site_new_struct (rtype, ndim, nstr, ndec);
    Sstat_q3 = G_site_new_struct (rtype, ndim, nstr, ndec);
    
    for(j=0; j< ndec; j++){
	for (i=0;i<NSites; i++){
	    if(!j)
		TmpSites[i] = G_site_new_struct (-1, 2, 0, 1);
	    TmpSites[i]->dbl_att[0] = CurSites[i]->dbl_att[j];
	}
	qsort (TmpSites, NSites, sizeof (Site *), G_site_d_cmp);

	/* used methods to match s.univar */ 
	if (!(NSites % 4)){
	    Sstat_q1->dbl_att[j] = (TmpSites[NSites/4]->dbl_att[0] +
				   TmpSites[NSites/4 - 1]->dbl_att[0])/2.0;
	    Sstat_q3->dbl_att[j] = (TmpSites[3*NSites/4]->dbl_att[0] +
				   TmpSites[3*NSites/4 - 1]->dbl_att[0])/2.0;
	}
	else{
	    Sstat_q1->dbl_att[j] = TmpSites[(NSites-1)/4]->dbl_att[0];
	    Sstat_q3->dbl_att[j] = TmpSites[3*(NSites-1)/4]->dbl_att[0];
	}

	if (!(NSites % 2))
	    Sstat_med->dbl_att[j] = (TmpSites[NSites/2]->dbl_att[0] +
				   TmpSites[NSites/2 - 1]->dbl_att[0])/2.0;
	else
	    Sstat_med->dbl_att[j] = TmpSites[(NSites-1)/2]->dbl_att[0];

    }

    for (i=0;i<NSites; i++)
	G_site_free_struct (TmpSites[i]);
    free(TmpSites);
    TmpSites = NULL;

    }

    return 0;
}

int load_sites_stats (FILE *sfd, int in_region)
{
int ndim, nstr, ndec;
RASTER_MAP_TYPE rtype;
int snum=0, outside=0;
Site *tsite;
int badformat=0, ret;

    rtype = -1;
    G_site_describe (sfd, &ndim, &rtype, &nstr, &ndec);
    if(!Shh){
	fprintf(stderr,"NDIM=%d, RTYPE = %d, NSTR=%d, NDEC=%d\n", 
		ndim, rtype, nstr, ndec );
    }
    tsite = G_site_new_struct (rtype, ndim, nstr, ndec);

    while((ret = G_site_get (sfd, tsite)) != -1){
	
	if(-2 == ret){
	    badformat++; 
            fprintf(stderr, "Bad format found for site: #%i (ignored)\n", snum + 1);
	    continue;
	}
	
	if( !in_region || G_site_in_region (tsite, &Wind) ){

	    addto_stats(tsite, !snum);
	    snum++;
	    if(!Shh)
		if(!(snum%1000)) fprintf(stderr,"%10d\b\b\b\b\b\b\b\b\b\b", snum);
	}
	else{
	    outside++;
	}
    }
    G_site_free_struct (tsite);
    NSites = snum;

    if(!Shh){
	fprintf(stderr,"Total sites read: %d\n", NSites);
	fprintf(stderr,"Total sites outside region: %d\n", outside);
	if(badformat)
	    fprintf(stderr,"Total sites ignored (format mismatch): %d\n", 
		    badformat);
    }

    return 0;
}


int site_mem (Site *s)
{
int i, tot;

    tot = sizeof(Site);
    tot += s->dim_alloc * sizeof(double);
    tot += s->dbl_alloc * sizeof(double);
    tot += s->str_alloc * sizeof(char *);

#ifdef REPORT_STR
    for (i=0; i<s->str_alloc; i++){
	tot += strlen(s->str_att[i]);
	tot++; 
    }
#endif

    return(tot);

}

int compress_cached_site (Site *s)
{
char *tofree;
int i;

    for (i=0; i<s->str_alloc; i++){
	tofree=s->str_att[i];
#ifdef REPORT_STR
	s->str_att[i]=G_store(tofree);
#else
	s->str_att[i]=NULL;
#endif
	free(tofree);
    }

    return 0;
}

int addto_stats (Site *s, int reset)
{
int i;

    if(reset){

	Sstat_max->ccat = Sstat_min->ccat = s->ccat;
	Sstat_max->fcat = Sstat_min->fcat = s->fcat;
	Sstat_max->dcat = Sstat_min->dcat = s->dcat;

	for (i=0; i<s->dbl_alloc; i++)
	    Sstat_max->dbl_att[i] = Sstat_min->dbl_att[i] = s->dbl_att[i];

	for (i=0; i<s->dim_alloc; i++)
	    Sstat_max->dim[i] = Sstat_min->dim[i] = s->dim[i];

#ifdef REPORT_STR
	for (i=0; i<s->str_alloc; i++){
	    strcpy(Sstat_min->str_att[i],s->str_att[i]);
	    strcpy(Sstat_max->str_att[i],s->str_att[i]);
	}
#endif

	Sstat_max->east = Sstat_min->east = s->east;
	Sstat_max->north = Sstat_min->north = s->north;

	return(0);
    }
    
    switch(s->cattype){
	case  CELL_TYPE:
	    if(s->ccat < Sstat_min->ccat)
		Sstat_min->ccat = s->ccat;
	    else if(s->ccat > Sstat_max->ccat)
		Sstat_max->ccat = s->ccat;
	    break;
	case FCELL_TYPE:
	    if(s->fcat < Sstat_min->fcat)
		Sstat_min->fcat = s->fcat;
	    else if(s->fcat > Sstat_max->fcat)
		Sstat_max->fcat = s->fcat;
	    break;
	case DCELL_TYPE:
	    if(s->dcat < Sstat_min->dcat)
		Sstat_min->dcat = s->dcat;
	    else if(s->dcat > Sstat_max->dcat)
		Sstat_max->dcat = s->dcat;
	    break;
    }
    for (i=0; i<s->dbl_alloc; i++){
	if(s->dbl_att[i] < Sstat_min->dbl_att[i])
	    Sstat_min->dbl_att[i] = s->dbl_att[i];
	else if(s->dbl_att[i] > Sstat_max->dbl_att[i])
	    Sstat_max->dbl_att[i] = s->dbl_att[i];
    }
    for (i=0; i<s->dim_alloc; i++){
	if(s->dim[i] < Sstat_min->dim[i])
	    Sstat_min->dim[i] = s->dim[i];
	else if(s->dim[i] > Sstat_max->dim[i])
	    Sstat_max->dim[i] = s->dim[i];
    }
#ifdef REPORT_STR
    for (i=0; i<s->str_alloc; i++){
	if(strlen(s->str_att[i]) < strlen(Sstat_min->str_att[i]))
	    strcpy(Sstat_min->str_att[i],s->str_att[i]);
	else if(strlen(s->str_att[i]) > strlen(Sstat_max->str_att[i]))
	    strcpy(Sstat_max->str_att[i],s->str_att[i]);
    }
#endif

    if(s->east < Sstat_min->east)
	Sstat_min->east = s->east;
    else if(s->east > Sstat_max->east)
	Sstat_max->east = s->east;

    if(s->north < Sstat_min->north)
	Sstat_min->north = s->north;
    else if(s->north > Sstat_max->north)
	Sstat_max->north = s->north;

    return(0);

}

#define TRY_LABELS

void sites_describe (void)
{
char rtypestr[32];
int i;
int got_labels=0;
#ifdef TRY_LABELS
char dimlabel[MAX_ST_ATTS][40];
char catlabel[40];
char strlabel[MAX_ST_ATTS][40];
char declabel[MAX_ST_ATTS][40];
int ndiml, nstrl, ndecl;
#endif
FILE *report=stdout;


    if(!Site_file){
	fprintf(stderr, "\n\t - - - NO SITES FILE LOADED - - -\n\n");
	return;
    }
/*
    if(!CacheSites){
	fprintf(stderr, "\n\t - - - NO SITES FILE CACHED - - -\n");
	return;
    }
*/

#ifdef TRY_LABELS
    ndiml = nstrl = ndecl = 0;
    if(Shd.form && Shd.labels){
    char *pf, *pl;
    int ret;

        pf=Shd.form;
        pl=Shd.labels;
	i = 0;
	while(pf[i]){
	    switch (pf[i]) {
		case '|':
		    ret = sscanf(pl, "%[^|#@%]s", dimlabel[ndiml]);
/*
fprintf(stderr,"%d %s\n", ndiml, dimlabel[ndiml]);
*/
		    ndiml += ret;
		    pl = strchr(pl, pf[i]) + 1;
		    break;
		case '#':
		    ret = sscanf(pl, "#%[^|#@%]s", catlabel);
/*
fprintf(stderr,"%s\n", catlabel);
*/
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
		case '@':
		    ret = sscanf(pl, "@%[^|#@%]s", strlabel[nstrl]);
/*
fprintf(stderr,"%d %s\n", nstrl, strlabel[nstrl]);
*/
		    nstrl += ret;
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
		case '%':
		    ret = sscanf(pl, "%%%[^|#@%]s", declabel[ndecl]);
/*
fprintf(stderr,"%d %s\n", ndecl, declabel[ndecl]);
*/
		    ndecl += ret;
		    if(pf[i+1] && pl[1])  pl = strchr(pl+1, pf[i+1]) ;
		    break;
	    }
	    if(!pl){
		break;
	    }
	    i++;
	}

	if((ndiml == Sstat_min->dim_alloc + 2) && 
	   (nstrl == Snumstrings) && 
	   (ndecl == Sstat_min->dbl_alloc)) got_labels = 1;
/*
fprintf(stderr,"%d:    %d %d %d\n", got_labels, Sstat_min->dim_alloc + 2,
 Snumstrings,Sstat_min->dbl_alloc);
*/
    }
#endif

    switch(Sstat_min->cattype){
	case  CELL_TYPE:
	    strcpy(rtypestr,"CELL_TYPE");
	    break;
	case FCELL_TYPE:
	    strcpy(rtypestr,"FCELL_TYPE");
	    break;
	case DCELL_TYPE:
	    strcpy(rtypestr,"DCELL_TYPE");
	    break;
	default:
	    strcpy(rtypestr,"NO CATEGORY");
	    break;
    }

    fprintf(report, 
	"----------------------------------------------------------\n\n");
    fprintf(report, "SITES FILENAME: %s\n", Sname);
    fprintf(report, "--------------\n");

    fprintf(report, "\n");

    /* for now just regurgitate header - hopefully will be able to 
       match labels to fields at some point - not yet supported by site lib*/

    fprintf(report, "Header Information:\n");
    fprintf(report, "------------------\n");
    if(Shd.name)
	fprintf(report, "\tname          %s\n", Shd.name);
    if(Shd.desc)
	fprintf(report, "\tdescription   %s\n", Shd.desc);
    if(Shd.form)
	fprintf(report, "\tformat        %s\n", Shd.form);
    if(Shd.labels)
	fprintf(report, "\tlabels        %s\n", Shd.labels);
    if(Shd.stime)
	fprintf(report, "\ttime          %s\n", Shd.stime);

    fprintf(report, "\n");

    fprintf(report, "Number of DIMENSIONS:         %d\n", 
		   Sstat_min->dim_alloc + 2);
    fprintf(report, "--------------------\n");
    fprintf(report, "\t      \t - - MIN - -\t - - MAX - -\n");
    fprintf(report, "\tdim  1\t%12f\t%12f\t%s\n", 
		     Sstat_min->east, Sstat_max->east,
		     got_labels? dimlabel[0]: "Easting");
    fprintf(report, "\tdim  2\t%12f\t%12f\t%s\n", 
		     Sstat_min->north, Sstat_max->north,
		     got_labels? dimlabel[1]: "Northing");
    for(i=0; i<Sstat_min->dim_alloc; i++){
	fprintf(report, "\tdim%3d\t%12f\t%12f\t%s\n", i+3,
		     Sstat_min->dim[i], Sstat_max->dim[i],
		     got_labels? dimlabel[i+2]: "");
    }

    fprintf(report, "\n");

    fprintf(report, "Type of CATEGORY information: %s\n", rtypestr);
    fprintf(report, "----------------------------\n");
    if(Sstat_min->cattype != -1)
	fprintf(report, "\t      \t - - MIN - -\t - - MAX - -\n");
    if(Sstat_min->cattype == CELL_TYPE)
	fprintf(report, "\t      \t%12d\t%12d\t%s\n", 
		     Sstat_min->ccat, Sstat_max->ccat, got_labels? catlabel:"");
    else if(Sstat_min->cattype == FCELL_TYPE)
	fprintf(report, "\t      \t%12f\t%12f\t%s\n", 
		     Sstat_min->fcat, Sstat_max->fcat, got_labels? catlabel:"");
    else if(Sstat_min->cattype == DCELL_TYPE)
	fprintf(report, "\t      \t%12f\t%12f\t%s\n", 
		     Sstat_min->dcat, Sstat_max->dcat, got_labels? catlabel:"");

    fprintf(report, "\n");

    fprintf(report, "Number of DOUBLE attributes:  %d\n", Sstat_min->dbl_alloc);
    fprintf(report, "---------------------------\n");
    if(Sstat_min->dbl_alloc){
	if(Do_Q){
	    fprintf(report,"\t      \t - - MIN - -\t - - Q1 - - \t - - MED - -\t - - Q3 - - \t - - MAX - -\n");
	    for(i=0; i<Sstat_min->dbl_alloc; i++)
		fprintf(report, "\tdbl%3d\t%12g\t%12g\t%12g\t%12g\t%12g\t%s\n",
		    i+1, Sstat_min->dbl_att[i], Sstat_q1->dbl_att[i], 
		    Sstat_med->dbl_att[i], Sstat_q3->dbl_att[i], 
		    Sstat_max->dbl_att[i],
		    got_labels? declabel[i]: "");
	}
	else{
	    fprintf(report, "\t      \t - - MIN - -\t - - MAX - -\n");
	    for(i=0; i<Sstat_min->dbl_alloc; i++)
		fprintf(report, "\tdbl%3d\t%12g\t%12g\t%s\n", i+1,
		     Sstat_min->dbl_att[i], Sstat_max->dbl_att[i],
		     got_labels? declabel[i]: "");
	}
    }

    fprintf(report, "\n");

    fprintf(report, "Number of STRING attributes:  %d\n", Snumstrings);
    fprintf(report, "---------------------------\n");
#ifdef REPORT_STR
    if(Sstat_min->str_alloc)
	fprintf(report, "\t      \t - MIN_LEN -\t - MAX_LEN -\n");
    for(i=0; i<Snumstrings; i++){
	fprintf(report, "\tstr%3d\t %6ld     \t %6ld     \t%s\n", i+1, 
		     strlen(Sstat_min->str_att[i]), 
		     strlen(Sstat_max->str_att[i]),
		     got_labels? strlabel[i]: "");
    }
#else
    if(Sstat_min->str_alloc)
	fprintf(report, "\t      \t - - MIN - -\t - - MAX - -\n");
    for(i=0; i<Snumstrings; i++){
	fprintf(report, "\tstr%3d\t%12s\t%12s\t%s\n", i+1, "", "",
		     got_labels? strlabel[i]: "");
    }
#endif

    /* add label IDs for each later when supported by sites lib */

    fprintf(report, "\n");
    fprintf(report, "TOTAL SITES COUNTED: %d\n", NSites);
    fprintf(report, 
	"----------------------------------------------------------\n\n");

}

int free_cached_sites (void)
{

    while (NSites > 0){
	G_site_free_struct (CurSites[--NSites]);
    }

    free(CurSites);
    CurSites = NULL;

    return 0;
}


int do_new_sites (char *name, char *mapset, int in_region)
{
char *site_map;
int ndim, rtype; 
static int first=1;
   
    if(Site_file){
	G_site_free_struct (Sstat_min);
	G_site_free_struct (Sstat_max);

	if(CacheSites)
	    free_cached_sites();
	else
	    fclose(Sitefd);

	Site_file = 0;
    }

    if(mapset != NULL && name != NULL) {
	site_map = mapset;
	strcpy(Sname, name);
    }
    else{
	site_map = G_ask_sites_old("enter name of sites file", Sname);
	    if (site_map == NULL){
		fprintf (stderr,  "Could not find file '%s'", Sname);
		return(0);
	    }
    }

    Sitefd = G_sites_open_old (Sname, site_map);
    if (Sitefd == NULL){
	fprintf (stderr, "can't open sites file [%s]", Sname);
	return(0);
    }
    G_site_get_head(Sitefd, &Shd);

    if(first){
	first=0;
	/* */
    }

    G_site_describe (Sitefd, &ndim, &rtype, &Snumstrings, &Snumvals);

    Sstat_min = G_site_new_struct (rtype, ndim, Snumstrings, Snumvals);
    Sstat_max = G_site_new_struct (rtype, ndim, Snumstrings, Snumvals);

    if(CacheSites){
	load_cached_sites(Sitefd,in_region);
	fclose(Sitefd);
    }
    else{
	load_sites_stats(Sitefd,in_region);
	fclose(Sitefd);
    }

    Site_file = 1;

    if(!Shh){
	if(CacheSites)
	    fprintf(stderr,"%s loaded.\n", Sname);
	else
	    fprintf(stderr,"%s read.\n", Sname);
    }

    return(1);
    
}


