/* %W% %G% */
/* proj.c    1.0   5/29/91
*    Created by : M.L.Holko , Soil Conservation Service, USDA
*                 derived from Vproj and original v.proj
*    Purpose: Productivity tool
*	      Provides a means of changing to a new projection
*             to match disparent mapset data
*
*  ------Rev 3.1 arguements --------------------------------------
*    Input arguements:
*           Vproj name=map_name proj=projection_name 
*
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*          v.projINV or
*          v.projFOR map=vector file to read
*                    omap=vector map to write
*                    iset=mapset for input vector file
*                    oset=mapset for output vector file
*                    parms=file which contains proj parameters
*					 stp=is the projection a State Plane
*
*/

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"

#define	B_DIR  "dig"
#define	ATT_DIR  "dig_att"
#define	CAT_DIR  "dig_cats"
#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"
/********************* PARMS FOR PROJ **********************/ 
# define RAD_TO_DEG     57.29577951308232
#define STP_FT 	0.30480060960121920243
typedef struct { double u, v; }		UV;

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points ;

/**** routines for projection change ********/
static UV (*proj1)();      /* projection function 1*/
static UV (*proj2)();      /* projection function 2*/
static double STP = 1.0;   /* State Plane factor to feet */

do_INV(u,v) double *u, *v;
{
UV data;    /*        data structure supplied to proj conv. routines */
/*DEBUG fprintf(stderr,"IN1 %lf %lf\n",*u,*v);*/
	data.u = *u * STP; data.v = *v * STP;
	data = (*proj1)(data);
	*u = data.u * RAD_TO_DEG; *v = data.v * RAD_TO_DEG;
/*DEBUG fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}

do_FOR(u,v) double *u, *v;
{
UV data;    /*        data structure supplied to proj conv. routines */
/*DEBUG fprintf(stderr,"IN2 %lf %lf\n",*u,*v);*/
	data.u = *u / RAD_TO_DEG; data.v = *v / RAD_TO_DEG;
	data = (*proj2)(data);
	*u = data.u / STP; *v = data.v / STP;
/*DEBUG fprintf(stderr,"OUT %lf %lf\n",*u,*v);*/
}
/***********  End PROJECTION CHANGE  ********************/

main (argc,argv)
int argc;
char *argv[];

{
	FILE *tf, *wnd;
	int i, in_stat, out_stat, cnt=0, type, cat, cmd1_cnt, cmd2_cnt;
	int day, yr, Out_proj;
	int out_zone = 0, vect_read;
	char ctype[1], cmnd1[200], cmnd2[200] ;
	char out_lon0[5], out_lat0[5];
	char answer[50], buffa[256], buffb[256], *value1;
	char bin_file[256], att_file[100], cat_file[100], date[40], mon[4];
	char *mapset, *omapset, *new_data, *G_tempfile(), *gets(), *tmpfile;
	char omap_name[20], map_name[20], iset_name[20], oset_name[20];
	char proj_out[20], proj_in[20];
	char opath[256], ipath[256], *value, *key ;
	char *p;
        long offset;
        double HE, HW, HS, HN, E, W, S, N, X, Y, num;
        FILE *in, *out1;
        FILE *ls, *spath;
        FILE *popen();
        P_LINE *Lines;
        struct Option *osetopt, *mapopt, *isetopt, *omapopt, *statepl, *parms;
        /********************** FOR USGS PROJ CALLS ***********************/
        extern UV (*ProjSetup())();
        static char *oform = (char *)0; /* output format */

        G_gisinit (argv[0]);

/****      DEBUG  
i = -1;
while(++i < argc) fprintf(stderr,"%s  ",argv[i]);
fprintf(stderr,"\n");
 END DEBUG ****/
     
		 /* set up the options and flags for the command line parser */

        omapopt = G_define_option();
        omapopt->key             = "omap";
        omapopt->type            =  TYPE_STRING;
        omapopt->required        =  YES;
        omapopt->description     = "output  vector file name";

        mapopt = G_define_option();
        mapopt->key             = "map";
        mapopt->type            =  TYPE_STRING;
        mapopt->required        =  YES;
        mapopt->description     = "input vector file name";

        isetopt = G_define_option();
        isetopt->key             =  "inset";
        isetopt->type            =  TYPE_STRING;
        isetopt->required        =  NO;
        isetopt->description     =  "mapset containing INput vector map";

        osetopt = G_define_option();
        osetopt->key             =  "outset";
        osetopt->type            =  TYPE_STRING;
        osetopt->required        =  NO;
        osetopt->description     =  "mapset to contain OUTput vector map";

        parms = G_define_option();
        parms->key             =  "parms";
        parms->type            =  TYPE_STRING;
        parms->required        =  YES;
        parms->description     =  "file with proj type parameters";

        statepl = G_define_option();
        statepl->key             =  "stp";
        statepl->type            =  TYPE_STRING;
        statepl->required        =  NO;
        statepl->description     =  "is the projection state plane";

 
	   /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		    exit (-1);
		     
           /* start checking options and flags */

									  
           /* set input vector file name and mapset */
	sprintf(map_name,"%s",mapopt->answer);
	sprintf(omap_name,"%s",omapopt->answer);

	if (isetopt->answer)
		sprintf(iset_name,"%s",isetopt->answer);
	else
		sprintf(iset_name,"%s",G_mapset());
	if (osetopt->answer)
		sprintf(oset_name,"%s",osetopt->answer);
	else 
		sprintf(oset_name,"%s",G_mapset());

	if (statepl->answer)
		STP = STP_FT;
	else
		STP = 1.0;


           /* Make sure map is available */
	mapset = G_find_vector (map_name, iset_name) ;
	if (mapset == NULL)
	{
		sprintf(buffb,"Vector file [%s] not available",map_name);
		G_fatal_error(buffb) ;
	}

           /* See if map is already in output mapset */
	omapset = G_find_vector (omap_name, oset_name) ;
	if (omapset != NULL)
	{
		sprintf(buffb,"Vector file [%s] already in mapset [%s]",
					 omap_name, iset_name);
		G_fatal_error(buffb) ;
	}

	sprintf(opath,"%s/%s",G_location_path(),oset_name);
	if (access(opath,0) != 0)
	   {
	   fprintf(stderr,
		 "\nMapset [%s] does NOT exist, creating new mapset\n",opath);
	   sprintf(buffb,"mkdir %s\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig_att",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig_att\n",opath);
	   system(buffb);
	   }
	sprintf(buffa,"%s/dig_cats",opath);
	if (access(buffa,0) != 0)
	   {
	   sprintf(buffb,"mkdir %s/dig_cats\n",opath);
	   system(buffb);
	   }


	cmnd1[0] = '\0';
	cmnd2[0] = '\0';
	sprintf(buffa,"%s/etc/v.projINV",G_gisbase());
        /********** get input proj parameters **********************/
        if ((tf = fopen(parms->answer,"r")) == NULL) 
	        G_fatal_error("Error can not open temp file\n");
        if (strcmp(argv[0],buffa) == 0) 
	        fgets(cmnd1,256,tf);
        else
	        fgets(cmnd2,256,tf);

if (strlen(cmnd1) > 0) p = (char *) cmnd1;
else p = (char *) cmnd2;
while ( strlen(p) > 0){
if (strncmp(p,"+zone=",5) == 0){
	p = (char *) p + 6;
	sscanf(p,"%d",&out_zone);
	}
p = (char *) (p + 1);
}

if (strlen(cmnd2) > 0) {
        p = (char *) cmnd2;
        while ( strlen(p) > 0){
        if (strncmp(p,"+proj=",5) == 0){
	        p = (char *) p + 6;
	        sscanf(p,"%s",proj_out);
	        }
        p = (char *) (p + 1);
        }
}
else  {
sprintf(proj_out,"ll");
out_zone = 0;
}

/*DEBUG fprintf(stderr,"1__ %s\n",cmnd1);*/
/*DEBUG fprintf(stderr,"2__ %s\n",cmnd2);*/
 
        fprintf(stderr,"\nLoading <%s> vector information.\n",map_name);

              /* Do initial read of input DIGIT file */
        if ((vect_read = Vect_open_old(&Map,map_name, mapset)) < 0 )
          {
          G_fatal_error("Reading input file.") ;
          return(-1) ;
          }
        if (vect_read < 1)
          {
          G_fatal_error("You must run v.support on this file.") ;
          return(-1) ;
          }
                     /* Open output proj  coord. file */
	if ((vect_read =  scs_open_new(&Out_Map, omap_name, oset_name)) < 0) 
	   {
	   sprintf(buffb,"Can't create new output file <%s in %s> \n", 
                                               omap_name, oset_name) ;
	   G_fatal_error(buffb) ;
	   }

                     /* Read and write header info */
        sprintf(date,"%s",G_date());
        sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
        if (yr < 2000) yr = yr - 1900;
        else yr = yr - 2000;
        sprintf(date,"%s %d %d",mon,day,yr);
        Vect_copy_head_data(&Map.head, &Out_Map.head);	

	strcpy(Out_Map.head.date,date);
	strcpy(Out_Map.head.your_name,omap_name);
	strcpy(Out_Map.head.line_3,buffa);

        fprintf(stderr,"\nCreating dig file...\n");

             /* Do the header portion */
        HE = Map.head.E;
        HS = Map.head.S;
        HW = Map.head.W;
        HN = Map.head.N;

/*DEBUG		fprintf(stderr,"N_%lf S_%lf E_%lf W_%lf\n",HN,HS,HE,HW);*/

        oform = "%.10f";
        /*SE*/
        if (strlen(cmnd1)) 
           {
		   set_proj(cmnd1,1);
			do_INV(&HE,&HS);
           }
        if (strlen(cmnd2))
           {
		   set_proj(cmnd2,2);
        do_FOR(&HE, &HS); 
           }
        E = HE;
        S = HS;
        HE = Map.head.E;
        HS = Map.head.S;

        /*NE*/
        if (strlen(cmnd1)) 
           {
		   do_INV(&HE, &HN);  
           }
        if (strlen(cmnd2))
           {
		   do_FOR(&HE, &HN); 
           }
        N = HN;
        HN = Map.head.N;
        if (HE < E) Out_Map.head.E = E;
        else Out_Map.head.E = HE;

        /*SW*/
        if (strlen(cmnd1)) 
           {
		   do_INV(&HW, &HS);  
           }
        if (strlen(cmnd2))
           {
		   do_FOR(&HW, &HS); 
           }
        W = HW;
        if (S < HS) Out_Map.head.S = S;
        else Out_Map.head.S = HS;
        HN = Map.head.N;
        HW = Map.head.W;

        /*NW*/
        if (strlen(cmnd1)) 
           {
		   do_INV(&HW, &HN);  
           }
        if (strlen(cmnd2))
           {
		   do_FOR(&HW, &HN); 
           }

        if (HN < N) Out_Map.head.N = N;
        else Out_Map.head.N = HN;
        if (HW > W) Out_Map.head.W = W;
        else Out_Map.head.W = HW;
/*DEBUG		fprintf(stderr,"N_%lf S_%lf E_%lf W_%lf\n",HN,HS,HE,HW);*/
/*DEBUG		fprintf(stderr,"N_%lf S_%lf E_%lf W_%lf\n\n",N,S,E,W);*/

	Out_Map.head.plani_zone = out_zone;

		/* create WIND file, if not existing */
	sprintf(buffa,"%s/WIND",opath);
	if (access(buffa,0) != 0)
	   {
	   if (strncmp(proj_out,"ll",2) == 0) Out_proj = 3;
	   else Out_proj = 99;
           if ((wnd = fopen(buffa,"w")) == NULL) 
	        G_fatal_error(" Can not open NEW WIND file\n");
	   fprintf(wnd,"proj:       %d\n",Out_proj);
	   fprintf(wnd,"zone:       %d\n",out_zone);
	   num = (int)(Out_Map.head.N +1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (wnd, "north:      %s\n", buffb);
	   num = (int)(Out_Map.head.S -1);
           G_format_northing (num,buffb,Out_proj);
           fprintf (wnd, "south:      %s\n", buffb);
	   num = (int)(Out_Map.head.E +1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (wnd, "east:       %s\n", buffb);
	   num = (int)(Out_Map.head.W -1);
           G_format_easting (num,buffb,Out_proj);
           fprintf (wnd, "west:       %s\n", buffb);
	   if (Out_proj == 3)
	      i = (int)((Out_Map.head.E - Out_Map.head.W) * 60.);
           else
	      i = (int)((Out_Map.head.E - Out_Map.head.W) / 100.);
	   fprintf (wnd, "cols:       %d\n",i);
	   if (Out_proj == 3)
	      i = (int)((Out_Map.head.N - Out_Map.head.S) * 60.);
           else
	      i = (int)((Out_Map.head.N - Out_Map.head.S) / 100.);
	   fprintf (wnd, "rows:       %d\n",i);
	   fprintf (wnd, "e-w resol:  100\n");
	   fprintf (wnd, "n-s resol:  100\n");
	   fclose(wnd);
           }
  
                     /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();


                     /* Cycle through all lines */
	i = 0;
        while(1)
        {
	    ++i;
                                 /* read line */

	    type = Vect_read_next_line (&Map, Points);
            if (type >= DEAD_LINE) continue;

	    if (type == -1)
			G_fatal_error("Reading input dig file.") ;
	    if ( type == -2) break;

            fprintf(stderr,"\b\b\b\b\b%5d",i); 
            if (strlen(cmnd1)) 
               {
               for (cnt=0; cnt<Points->n_points; cnt++)
                  {
                  X = Points->x[cnt];
                  Y = Points->y[cnt];
				  do_INV(&X, &Y);
                  Points->x[cnt] = X;
	          Points->y[cnt] = Y;
                  }
               }

            if (strlen(cmnd2))
               {
               for (cnt=0; cnt<Points->n_points; cnt++)
                  {
                  X = Points->x[cnt];
                  Y = Points->y[cnt];
				  do_FOR(&X, &Y);
                  Points->x[cnt] = X;
	          Points->y[cnt] = Y;
                  }
               }

                          /* write line */
            Vect_write_line (&Out_Map, type, Points); 
            }  /* end lines section */

        Vect_close (&Out_Map); 
        Vect_close (&Map); 

        fprintf(stderr,"\nCreating Att file...\n");

		/* update the dig_att file by removing lower case atts*/
        G__file_name(att_file, ATT_DIR, map_name, mapset);
	if ( (in = fopen (att_file, "r")) == NULL)
		G_fatal_error("Reading input att file.") ;
         
        new_data = G_tempfile();
		     /* Open output "temp dig_att" file */
	if ( (out1 = fopen (new_data, "w")) == NULL)
	   {
	   sprintf(buffb,"Can't create temporary output att file <%s> \n", 
                                               new_data) ;
	   G_fatal_error(buffb) ;
	   }
 
        while(fgets(buffa,80,in))
           {
	   if (strncmp(buffa,"A",1) == 0 ||
	       strncmp(buffa,"L",1) == 0 ||
	       strncmp(buffa,"P",1) == 0 )
		 {
                 sscanf(buffa,"%s %lf %lf %d", ctype, &X, &Y, &cat);
                 if (strlen(cmnd1)) 
                    {
					do_INV(&X, &Y);  
                    }
                 if (strlen(cmnd2))
                    {
					do_FOR(&X, &Y); 
                    }

                          /* write line */
                  sprintf(buffb,"%1s %14.10lf %14.10lf %7d\n",ctype,X,Y,cat);
                  fputs(buffb,out1);
	          }
	    }
	fclose (out1);
        fclose (in);

                     /* copy temp dig_att file to original */
	sprintf(buffa, "cp %s %s/%s/dig_att/%s",
			     new_data,G_location_path(),oset_name,omap_name);
        if (system( buffa) )
	   {
	   fprintf(stderr,
            "ERROR(%s):  Could not create file: '%s/dig_att/%s'\n"
			, argv[0], iset_name,omap_name) ;
           unlink(out1);
	   exit(-1) ;
	   }
        unlink(out1);

		/* get the dig_cats file name*/
        if (access(G__file_name(cat_file, CAT_DIR, map_name, mapset), 0) == 0)
	   {
           fprintf(stderr,"\nCreating category file...\n");
                /* copy dig_cats file to this location */
	   sprintf(buffa,"cp %s %s/%s/dig_cats/%s",
			     cat_file,G_location_path(),oset_name,omap_name);

           if (system( buffa) )
	      {
	      fprintf(stderr,
               "WARNING(%s):  Could not create file: '%s/dig_cats/%s'\n"
			, argv[0], iset_name,omap_name) ;
	      }
	   }


        sprintf(buffb,"rm %s",new_data);
        system(buffb);

        
	exit(0);
}

/* copy of Vect_open_new, modified to open map in another mapset */
scs_open_new (Map, name, mapset)
    struct Map_info *Map;
    char *name, *mapset;
{
    char name_buf[200];
    int f_p;
    FILE *fp;

    Vect_init ();	/* init vector system */

/*DEBUG debugf ("Openning file %s level 1 WRITE\n", name);*/
/*  if (NULL == (fp = G_fopen_new ("dig", name)))*/

    G__file_name (name_buf, "dig", name, mapset);
    if (access(name_buf,0) != 0)
          close (creat (name_buf, 0666));
    if (NULL == (fp = fopen(name_buf, "w")))
          return -1;

    Map->dig_fp = fp;

    G__file_name (name_buf, "dig", name, mapset);
    Map->digit_file = G_store (name_buf); /*need?*/
    Map->open  = VECT_OPEN_CODE;
    Map->level = LEVEL_1;
    Map->mode  = MODE_WRITE;
    Map->name = G_store (name);
    Map->mapset = G_store (mapset);
    Map->Constraint_region_flag = 0;	/* these do not apply to to write, but*/
    Map->Constraint_type_flag   = 0;    /* init them anyway                   */

    Vect__init_head (&(Map->head));
    Vect__write_head_binary (Map, &(Map->head)); /* note dig_ => dig__*/

    return 1;
}

set_proj(cmnd,n)
char *cmnd;
int n;
{
char *p;
/*DEBUG fprintf(stderr,"\nSET_UP __ %s\n",cmnd);*/
	p = strtok(cmnd," \t");
	load_opt(p, 0);
	while ((p = strtok(NULL," \t")) != NULL)
        	load_opt(p, 0);
	if (n == 1) proj1 = ProjSetup(1);
	else proj2 = ProjSetup(0);
}
