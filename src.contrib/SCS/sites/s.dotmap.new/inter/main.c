/* %W% %G% */
/* main.c */  
                                                                    
#include <stdio.h>
#include <string.h>
#include <math.h>
#include  "gis.h"
#define MAIN
#define UNIT_FILE "PROJ_UNITS"
#define CONVFT  30.480060960121920243
#define CONVCM  100.0
#define DEFSIZE 0.2
#define COVER 0.75


double sfactor();

static void process(in,out,nmult)
char *in, *out;
double nmult;
{
char buffr[100],poly[100],*ptr;
int i,dot_cnt;
double mult;
FILE *fd, *tmp;

/*DEBUG fprintf(stderr,"%s %s %lf\n",in,out,nmult);*/
    if ((fd = fopen(in,"r")) == NULL)
       {
       G_fatal_error("Reading dot file.") ;
       exit(-1) ;
       }

    if ((tmp = fopen(out,"w")) == NULL)
       {
       G_fatal_error("Creating output dot file.") ;
       exit(-1) ;
       }

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
}

main (argc,argv)
int argc;
char *argv[];

{
    int i, names=1, in_stat, dot_cnt,FIRST;
    char buffr[100], *ptr, *mapset, proj_in[20], tmp1_file[100],dig_stat[100];
	char output[100], unitfile[100], tmp_file[100], catnum[3];
    struct Option *siteopt, *mapopt, *dotopt, *scaleopt;
	struct Option *odotopt, *sizeopt, *covopt;
    struct Flag *n_flag;
	struct Key_Value *in_proj_keys;
	double area, rsize, scale, factor, size, mult, nmult, tsize, tcov;
	FILE *vdot, *popen(), *stat_fd;

    G_gisinit (argv[0]);
     
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
	scaleopt->options  = "1-999999999999";
    scaleopt->description     = "The denominator of the map scale";

    covopt = G_define_option();
    covopt->key             = "cover";
    covopt->type            =  TYPE_DOUBLE;
    covopt->required        =  NO;
	covopt->options  = "0-1";
    covopt->description     = "The fraction of the most dense area to cover with dots";

    sizeopt = G_define_option();
    sizeopt->key             = "size";
    sizeopt->type            =  TYPE_DOUBLE;
    sizeopt->required        =  NO;
	sizeopt->options  = "0-100";
    sizeopt->description     = "The size of each dot in cm";
    
    n_flag = G_define_flag();
    n_flag->key              = 'n';
    n_flag->description      = "Use category numbers NOT names ";



    if (G_parser (argc, argv))
	    exit (-1);
	     
           /* start checking options and flags */
    if (n_flag->answer) 
sprintf(catnum,"n");
	else
catnum[0] = 0;
/********** Get scale, target size, and target coverage *************/
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

/********* Get/create dig_stats file  ********************/
	G__file_name(dig_stat,"dig_stats",mapopt->answer,mapset);
	while (access(dig_stat,0) != 0)
{
fprintf(stderr,"CREATING a dig_stats file for %s\nThis ma take a while\n",mapopt->answer);
sprintf(buffr,"v.mk_stats -ns map=%s",mapopt->answer);
}
    if ((stat_fd = fopen(dig_stat,"r")) == NULL)
       {
       G_fatal_error("Opening dig_stat file.") ;
       exit(-1) ;
       }
	

/* do initial run  */
	sprintf(tmp_file,"%s",dotopt->answer);
	sprintf(tmp1_file,"%s",G_tempfile());
size = factor * tsize / tcov;
fprintf(stderr,"Calling sfact\n");
if (n_flag->answer) i = 1;
mult = sfactor(scale,size,mapopt->answer,mapset,dotopt->answer,i);

fprintf(stderr,"\n\n\tOne dot representing %.0lf unit(s)\n\twill allow a dot size of %.2lf cm\n\tcovering %.2lf of the most dense area.\n",mult,tsize,tcov);
 
 if (mult > 1.0) {
	fprintf(stderr,"\nHow many units do you want represented by one dot [%.0lf]: ",mult);
 	gets(buffr,50);
	if (strlen(buffr) > 0)
		sscanf(buffr,"%lf",&nmult);
 	else
 		nmult = mult;
	 }
else
	nmult = mult = 1.0;
  
size = tsize * (nmult / mult);
   
fprintf(stderr,"\n\tOne dot represents %.0lf unit(s)\n",nmult);
if (isatty(1))
	   printf("\tYour dot size is %.2lf cm\n",size);
else printf("%.2lf",size);

 
  
if (odotopt->answer)
	sprintf(tmp1_file,"%s",odotopt->answer);
else
	sprintf(tmp1_file,"%s",G_tempfile());
   
process(tmp_file,tmp1_file,nmult);
fprintf(stderr,"s.makedots -vs%s site=%s map=%s dot=%s",catnum,siteopt->answer,mapopt->answer,tmp1_file);
/*
sprintf(buffr,"s.makedots -vs%s site=%s map=%s dot=%s",catnum,siteopt->answer,mapopt->answer,tmp1_file);
system(buffr);
*/

}
