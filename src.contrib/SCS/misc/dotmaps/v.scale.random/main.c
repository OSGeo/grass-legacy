/* main.c */  
                                                                    
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include  "gis.h"
#define MAIN
#define UNIT_FILE "PROJ_UNITS"
#define CONVFT  30.480060960121920243
#define CONVCM  100.0
#define DEFSIZE 0.2
#define COVER 0.75

int main (int argc, char *argv[]) 
{
    int i, names=1, in_stat, dot_cnt;
    char buffr[100], *ptr, *mapset, proj_in[20], poly[100];
	char output[100], unitfile[100], tmp_file[100], catnum[3];
    struct Option *siteopt, *mapopt, *dotopt, *scaleopt;
	struct Option *odotopt, *sizeopt, *covopt;
    struct Flag *n_flag;
	struct Key_Value *in_proj_keys;
	double rsize, scale, factor, size, mult, nmult, tsize, tcov;
	FILE *vdot, *popen(), *tmp, *fd;
    struct GModule *module;
    
    G_gisinit (argv[0]);
    
    /* Set description */
    module              = G_define_module();
    module->description = ""\
    "Creates a site_lists file of randomly placed symbols within a GRASS vector area.";
     
	 /* set up the options and flags for the command line parser */

    mapopt = G_define_option();
    mapopt->key             = "map";
    mapopt->type            =  TYPE_STRING;
    mapopt->required        =  YES;
    mapopt->description     = "input vector file name";

    siteopt = G_define_option();
    siteopt->key             = "site";
    siteopt->type            =  TYPE_STRING;
    siteopt->required        =  YES;
    siteopt->description     = "output site_list file name";

    dotopt = G_define_option();
    dotopt->key             = "dot";
    dotopt->type            =  TYPE_STRING;
    dotopt->required        =  YES;
    dotopt->description     = "name of file containing labels and counts";

    odotopt = G_define_option();
    odotopt->key             = "outdot";
    odotopt->type            =  TYPE_STRING;
    odotopt->required        =  NO;
    odotopt->description     = "name of new file to contain scaled counts";

    scaleopt = G_define_option();
    scaleopt->key             = "scale";
    scaleopt->type            =  TYPE_DOUBLE;
    scaleopt->required        =  YES;
	scaleopt->options		  = "1-999999999999";
    scaleopt->description     = "The denominator of the map scale";

    covopt = G_define_option();
    covopt->key             = "cover";
    covopt->type            =  TYPE_DOUBLE;
    covopt->required        =  NO;
	covopt->options		  = "0-1";
    covopt->description     = "The fraction of the most dense area to cover with dots";

    sizeopt = G_define_option();
    sizeopt->key             = "size";
    sizeopt->type            =  TYPE_DOUBLE;
    sizeopt->required        =  NO;
	sizeopt->options		  = "0-100";
    sizeopt->description     = "The size of each dot in cm";
    
    n_flag = G_define_flag();
    n_flag->key              = 'n';
    n_flag->description      = "Use category numbers NOT names ";



    if (G_parser (argc, argv))
	    exit (-1);
	     
           /* start checking options and flags */
    if (n_flag->answer) 
		sprintf(catnum,"-n");
	else
		catnum[0] = 0;
/********** Get scale, target size, and tarket coverage *************/
	sscanf(scaleopt->answer,"%lf",&scale);

	if (sizeopt->answer)
		sscanf(sizeopt->answer,"%lf",&tsize);
	else tsize = DEFSIZE;

	if (covopt->answer)
		sscanf(covopt->answer,"%lf",&tcov);
	else tcov = COVER;
/*************** END of get ****************/

           /* check for input vector file name and mapset */
    mapset = G_find_vector (mapopt->answer, "") ;
    if (mapset == NULL)
	{
	sprintf(buffr,"Vector file [%s] not available in search list",
	    mapopt->answer);
	G_fatal_error(buffr) ;
	exit(-1);
	}

           /* check for input dot file name and mapset */
    if (access(dotopt->answer,0) != 0)
	{
	sprintf(buffr,"dot file [%s] not found", dotopt->answer);
	G_fatal_error(buffr) ;
	exit(-1);
	}
     
	/* check to see if there is a site file */
	sprintf(output,"%s/%s/site_lists/%s",G_location(),G_mapset(),siteopt->answer);
	if (access(output,0) == 0)
	{
	sprintf(buffr,"site file [%s] already exists", siteopt->answer);
	G_fatal_error(buffr) ;
	exit(-1);
	}


	   /*** Get projection info for input mapset ***/
	G__file_name (unitfile, "", UNIT_FILE, mapset);
	while (access(unitfile,0) != 0)
		{
		fprintf(stderr,"ifile %s\n",unitfile);
		fprintf(stderr,"%s file not found  for mapset %s\n",UNIT_FILE,mapset);
		fprintf(stderr,
		"Projections are: utm,aea,stp,ll,lcc,merc,tmerc,xxx\n");
		fprintf(stderr,"Enter projection for input mapset : ");
		scanf("%s",proj_in);   
		fprintf(stderr,"Running m.setproj\n");
		sprintf(buffr,"m.setproj set=%s proj=%s",mapset,proj_in);
		system(buffr);
		}
	in_proj_keys = G_read_key_value_file(unitfile,&in_stat);
	if (in_stat != 0)
		{
		sprintf(buffr,"ERROR in reading mapset %s unit file %s \n",mapset,UNIT_FILE);
		G_fatal_error(buffr) ;
		}
	sprintf(proj_in,"%s", G_find_key_value("units",in_proj_keys));
	if ( strcmp(proj_in,"meters") == 0) 
		factor = CONVCM;
	else if ( strcmp(proj_in,"feet") == 0)
		factor = CONVFT;
	else {
		sprintf(buffr,"Can not handle %s",proj_in);
		G_fatal_error(buffr) ;
		}

		/* do initial run of v.dots */
	sprintf(buffr,"v.dots %s -s map=%s dot=%s",catnum,mapopt->answer,dotopt->answer);
	if ( vdot = popen(buffr,"r"))
		{
		fgets(buffr,20,vdot);
/*DEBUG*/fprintf(stderr,"GOT_%s\n",buffr);
		sscanf(buffr,"%lf",&rsize);
		pclose(vdot);
		}

		/* calculate size */
size = (sqrt(rsize) * factor)/scale;
/*fprintf(stderr,"\nfactor %f SIZE = %f\n",factor,size);*/
if ((mult = (tsize/tcov)/size) < 1){
	size = size / mult;
	mult = 1.0;
	}
else {
	mult = floor(mult + 0.5);
	size = tsize;
	}

/*fprintf(stderr,"\ntsize %f cover = %f\n",factor,size);*/
fprintf(stderr,"\n\n\tOne dot representing %.0f unit(s)\n\twill allow a dot size of %.2f cm\n\tcovering %.2f of the most dense area.\n",mult,size,tcov);

if (mult > 1.0) {
	fprintf(stderr,"\nHow many units do you want represented by one dot [%.0f]: ",mult);
	fgets(buffr,50,stdin);
	if (strlen(buffr) > 0)
		sscanf(buffr,"%lf",&nmult);
	else
		nmult = mult;
	}
else
	nmult = mult = 1.0;

size = size * (nmult / mult);

fprintf(stderr,"\n\tOne dot represents %.0f unit(s)\n",nmult);
if (isatty(1))
	fprintf (stdout,"\tYour dot size is %.2f cm\n",size);
else fprintf (stdout,"%.2f",size);


                     /* open the dots file */
    if ((fd = fopen(dotopt->answer,"r")) == NULL)
       {
       G_fatal_error("Reading dot file.") ;
       return(-1) ;
       }

	if (odotopt->answer)
		sprintf(tmp_file,"%s",odotopt->answer);
	else
		sprintf(tmp_file,"%s",G_tempfile());
                     /* open a tmp file */
    if ((tmp = fopen(tmp_file,"w")) == NULL)
       {
       G_fatal_error("Creating output dot file.") ;
       return(-1) ;
       }

			/* process dot file */
     while(fgets (buffr, sizeof(buffr), fd) )
           {
		   i = 0;
		   while (buffr[i] != ':') i++;
		   buffr[i] = 0;
		   sscanf(buffr,"%s",poly);
		   ptr = (char *) (buffr +i +1);
	      sscanf(ptr,"%lf",&mult);
		  mult = mult / nmult;
		  dot_cnt = (int) floor(mult + 0.5);
		  if (dot_cnt > 0)
		  fprintf(tmp,"%s:%d\n",poly,dot_cnt);
		  }
	fclose(tmp);
	fclose(fd);

	sprintf(buffr,"v.dots -v %s site=%s map=%s dot=%s",catnum,siteopt->answer,mapopt->answer,tmp_file);
	system(buffr);
	if (! odotopt->answer)
		{
		sprintf(buffr,"rm %s",tmp_file);
		system(buffr);
		}

    exit(0);
}
