/*  @(#)imp_main.c	2.2  3/08/91  GRASS 4.0  
/*  @(#)imp_main.c	2.1  7/11/89  
* Created by: R.L.Glenn
*
* modified:  changed input arguement handling for 4.0
*                                     R.Glenn, SCS  3-08-91
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.in.scsgef [-o]  gef=ascii-gef-name
*                               output=vector-name
*                               subj=subject-file-name
*
*    flags:
*         -o      : old scsgef format
*
*/

#include <stdio.h>
#define MAIN
#include "format.h"
#include "gis.h"

#define METERS_PER_INCH	0.0254

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	int ier, old_cat_cnt=0, recd, cnt=1 ;
	char  *ascii, *digit ;
	FILE *gef_info, *gef_file,*gef_feat,*cat ;
	FILE *dig_asc,*dig_att,*fopen(), *category_file;
	char filename[128], *tmp_name, category_fname[128] ;
	char  *rindex(), *G_tempfile() ;
	char buff[128], gef_name[100], dig_name[100];
	struct bdig_head head;
        struct Option *inopt, *outopt, *subjopt;
        struct Flag *o_flag;
	extern	int	gef_format ;
	extern	int	cat_cnt ;

	setbuf(stdout, 0) ;

        G_gisinit (argv[0]);
		
                 /* set up the options and flags for the command line parser */

        o_flag = G_define_flag();
        o_flag->key              = 'o';
        o_flag->description      = "Is the SCS-GEF in the OLD format(24 char)";

        inopt = G_define_option();                                 
        inopt->key              = "gef";
        inopt->type             =  TYPE_STRING;
        inopt->required         =  NO;
        inopt->description      = "ascii SCS-GEF file name";

        outopt = G_define_option();
        outopt->key              = "output";
        outopt->type             =  TYPE_STRING;
        outopt->required         =  NO;
        outopt->description      = "vector file name";

        subjopt = G_define_option();
        subjopt->key              = "subj";
        subjopt->type             =  TYPE_STRING;
        subjopt->required         =  NO;
        subjopt->description      = "category file name";

                /* heeeerrrrrre's the   PARSER */                
        if (G_parser (argc, argv))
                exit (-1);

        if (o_flag->answer) gef_format = 0;
        else  gef_format = 1;

	sprintf(gef_name,"%s",inopt->answer);
	sprintf(dig_name,"%s",outopt->answer);
				/* rindex is for ATT System V */
	if ( ! (ascii= rindex(gef_name, '/')))
		ascii = gef_name ;
	else
		++ascii ;

	if ( ! (digit= rindex(dig_name, '/')))
		digit = dig_name ;
	else
		++digit ;
        
                            /* Make reg dir, if not existing */
 	G__make_mapset_element("reg") ;

/* get header and complete dig head info, create reg points file */
        if (get_dig_head(gef_name, digit) < 0) exit(-1);     

/* Print warning */
        sprintf(filename,"%s.info\0",gef_name);
	if ( (gef_info = fopen(filename, "r")) == NULL)
	   {
	   printf("Can't find create information file <%s>\n", filename) ;
	   return (-1);
	   }
	/* Open file for reading */
	sprintf(filename,"%s\0",gef_name);
	if ( (gef_file = fopen(filename, "r")) == NULL)
	{
		printf("Can't find input GEF file <%s>\n", filename) ;
		exit(-1) ;
	}

                            /* Make Master Category dir, if not existing */
 	G__make_mapset_element("SUBJ") ;

                            /* open category file for reading, if it exists*/
	sprintf(category_fname,"%s\0",subjopt->answer);
	tmp_name = G_tempfile();
  	if ((ier = access(category_fname,0)) != -1)
	   {      /* the category file exists, copy categ. to tmp */
           if ( (category_file = fopen (category_fname,"r")) == NULL)
	      {
		printf("Can't find %s\n", category_fname) ;
		exit(-1) ;
	      }
           if ( (cat = fopen (tmp_name,"a")) == NULL)
	      {
		printf("Can't open new file %s\n", tmp_name) ;
		exit(-1) ;
	      }
			     /*  get current category count */
	   fgets (buff, 72, category_file);
	   sscanf(buff,"%*s%d%*s",&old_cat_cnt);
                             /*  read past next four records */
	   for (recd=0;recd<=3;++recd)
	       fgets (buff, 72, category_file);
	                /*  copy remaining records to tmp */
           for (recd=0;;++recd)
               {
	       if (!fgets (buff, 72, category_file)) goto cat_end;
	       fputs (buff,cat);
	       }

cat_end:   fclose (category_file);
           }
			/* create a temporary file for categories */
        else if ( (cat = fopen (tmp_name,"w")) == NULL)
              {
		printf("Can't open new file %s\n", tmp_name) ;
		exit(-1) ;
	      }
	fclose (cat);
        cat_cnt = old_cat_cnt;

  	G_clear_screen();

	printf("\nConverting the gef import file: %s\n", ascii);
	printf("     to digit file: %s\n\n", digit);

                            /* Make dig_ascii dir, if not existing */
 	G__make_mapset_element("dig_ascii") ;

	/* Open file for writing */
	sprintf(filename,"%s/dig_ascii/%s\0",getenv("LOCATION"),digit);
	if ( (dig_asc = fopen(filename, "w")) == NULL)
		{
			printf("Can't open %s\n", "dig_asc") ;
			exit(-1) ;
		}
                            /* Make dig_att dir, if not existing */
 	G__make_mapset_element("dig_att") ;
	sprintf(filename,"%s/dig_att/%s\0",getenv("LOCATION"),digit);
	if ( (dig_att = fopen(filename, "w")) == NULL)
		{
			printf("Can't open %s\n", "dig_att") ;
			exit(-1) ;
		}
	/* Read and write the main body */
	if (imp_gef(gef_info,gef_file,tmp_name,dig_asc,dig_att) == -1)
		{
			printf("Error in translating gef import\n") ;
			exit (-1) ;
		}
        /* make a final mapset master category file 
		              put in first three records  */
 	category_file = fopen(category_fname,"w");
	cat = fopen(tmp_name,"r");
	rewind (cat);
	sprintf (buff,"#%5d categories\n",cat_cnt);
	fputs(buff,category_file);
	sprintf(buff,"Title %s\n",dig_name);
	fputs(buff,category_file);
	sprintf(buff,"\n0.00 0.00 0.00 0.00\n0:no data\n");
	fputs(buff,category_file);
         /*   now append the cat data  */
	for (recd=0;;++recd)
	     {
	     if (!fgets (buff, 72, cat)) goto tmp_end;
	     fputs (buff,category_file);
	     }
tmp_end: fclose (category_file);

                            /* Make dig_cats dir, if not existing */
 	G__make_mapset_element("dig_cats") ;
/* copy the master category file to this dig-map-file category file */
        sprintf(buff,"cp %s %s/dig_cats/%s\n\0",category_fname,getenv("LOCATION"),digit);
        system (buff); 

        fclose (gef_info) ;
  	sprintf(buff,"rm %s.info\n",gef_name); 
	system(buff); 
        fclose (gef_file);
	fclose (cat) ;
        fclose (dig_asc) ;
        fclose (dig_att) ;

	exit(0);
}
