/* %W% %G% */
/* do_dot.c    1.0   4/01/91
*                                                                       
*     Purpose                                                           
*        Extract edge and polygon information, based upon a user defined
*        list, output sites(points) in a psuedo-random pattern,      
*         within the bounds of the polygon.                             
*                                                                       
*/                                                                       

#include <ctype.h>
#include <string.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"
#include "dots.h"

struct Map_info Map;
struct Plus_head Plus ;
struct Categories cats ;
char  buf[1024] ;

do_dots(in_name, out_name, dot_name, name_opt, verbose)
char  *in_name, *out_name, *dot_name;
int name_opt, verbose;
{
    int i, ii, cat, icat, area_num, dot_cnt, cnt, ret, size_opt=0;
    int line, linea, poly_num, vect_read;
    char buff[300], *mapset, label[100];
    char *tmp_file, site_name[200],stat_name[200];
    char *ptr, *poly, *dots, chr;
    double N,S,E,W,f_area,t_area;
    FILE *fd, *tmp, *of, *sf;
	P_AREA *Areas;


	dot_ratio = 999999999.;
    mapset = G_find_file("dig",in_name,"");
 
    if (strlen(out_name) == 0) size_opt = 1;

    if (verbose) fprintf(stderr,"\nLoading vector information.\n");
              /* Do initial read of input DIGIT file */
    if ((vect_read = Vect_open_old(&Map,in_name, mapset)) < 0 )
          {
          G_fatal_error("Reading input file.") ;
          return(-1) ;
          }
    if (vect_read < 2)
          {
          G_fatal_error("You must run v.support on this file.") ;
          return(-1) ;
          }

    G_read_vector_cats (in_name, mapset, &cats);

                     /* open the dots file */
    if ((fd = fopen(dot_name,"r")) == NULL)
       {
       G_fatal_error("Reading dot file.") ;
       return(-1) ;
       }

				/*  open dig_stats file */
	G__file_name(stat_name, "dig_stats", in_name, mapset) ;
    if ((sf = fopen(stat_name,"r")) == NULL)
          {
          G_fatal_error("Can not access dig_stats file.\nRun v.mk_stats first.") ;
          return(-1) ;
          }
                     /* open a tmp file */
    tmp_file = G_tempfile();
    if ((tmp = fopen(tmp_file,"w")) == NULL)
       {
       G_fatal_error("Creating temp file.") ;
       return(-1) ;
       }

    if (verbose) fprintf(stderr,"\nProcessing .....       ");

while(fgets (buff, sizeof(buff), fd) )
	{
/*********************** GET_AREAS *******************************/
	poly = dots = buff;
 /* separate poly name from dot count, colon delimiter (SCS version) */
	while (*dots && *dots != ':') dots++;
	if (*dots != ':') continue;
	*dots++ = 0;
	for (ptr = dots; *ptr; ptr++)
		{
		if (*ptr == '\n')
			{
			*ptr = 0;
			break;  
			}
		}
	dot_cnt = atoi(dots);

	if (name_opt) for (ii=0; ii < cats.count; ii++)
		{       /* using poly names, NOT numbers */
		if (strncmp(poly,cats.list[ii].label,strlen(poly)) == 0)
			cat = cats.list[ii].num;
		}
	else         /* using poly numbers, NOT names */
		if (!isdigit(*poly))
		 	{
			G_fatal_error("Invalid dotfile, using names, NOT numbers");
			return(-1) ;
			}
		else
			cat = atoi(poly);

	

	while (fgets(buff,300,sf) != NULL) {
		sscanf(strtok(buff,":"),"%d",&icat);
		sscanf(strtok(NULL,":"),"%s",buff);
		sscanf(strtok(NULL,":"),"%lf",&t_area);
		if (cat == icat) break;
		}
	rewind(sf);

/***********  PROCESSS AREAS ******************/
	while ((ptr = strtok(NULL," ")) != NULL)
		{
		sscanf(ptr,"%d",&area_num);
		if (verbose) fprintf(stderr,"\b\b\b\b\b%5d",area_num);
		else {
			if (chr == '\\') chr = '/'; else chr = '\\';
			fprintf(stderr,"\b%c",chr);
			}
		if (V2_get_area(&Map,area_num,&Areas) != 0)
			G_fatal_error("could not get area info\n");
		dig_find_area2 (&Map, Areas, &f_area);
		f_area = dot_cnt * (f_area/t_area);
		if ((cnt = (int)f_area) > 0) 
			{
			if ((ret = put_dots(&Map,area_num,cnt,tmp,size_opt)) != 0)
	    		{
   				if (verbose) fprintf(stderr,
	  				"\n\tarea %d, <%d>, needs %d additional dots\n",
       				area_num, cat, ret);
   				}
			}
		}
	}


    Vect_close (&Map);
    fclose(fd);
    fclose(tmp);

    if (size_opt)
      {
      if (dot_ratio != 999999999.)
	 	{
         	if (verbose)
	    	{
	    	fprintf(stderr,"\n\n");
            fprintf(stderr,
	    	 "\tsmallest area %.2lf with dot count of %d\n",
		  	min_area,area_cnt);
            fprintf(stderr,
	     	"\tthis represents a ratio of :\n");
	    	}
         	if (verbose)
	    	{
           	 fprintf(stderr, "\t%lf sq.meters per dot\n",dot_ratio);
           	 fprintf(stderr,
	   	  	"\t%lf acres per dot\n",
		  	(dot_ratio * 3.280843) / 43560.0);
            fprintf(stderr,
	     	"\t%lf hectares per dot\n",
		  	((dot_ratio * 3.280843) / 43560.0) / 2.471044);
	    	}
			if (!verbose)
				printf("%.2lf\t%d\n",min_area,area_cnt);
	 	}
      else
	 {
	 if (verbose)
	    {
            fprintf(stderr,"\n\n");
            fprintf(stderr, "\tNothing processed, check your dotfile\n");
	    }
	 }
      }
    else
      {
                     /* open the sites file */
      G__make_mapset_element("site_lists") ;
      G__file_name(site_name, "site_lists", out_name, G_mapset()) ;
      if ((of = fopen(site_name,"w")) == NULL)
          {
          G_fatal_error("Creating site_list file.") ;
          return(-1) ;
          }

      fprintf(of,
	  "#smallest area %.2lf with dot count of %d\n",min_area,area_cnt);
      if ((tmp = fopen(tmp_file,"r")) == NULL)
          {
          G_fatal_error("Reading tmp file.") ;
          return(-1) ;
          }
      rewind(tmp);
      while(1)
          {
          if (!fgets (buff, sizeof(buff), tmp)) break;
	  fprintf(of,"%s",buff);
	  }
      fclose(of);
      }
      
    unlink(tmp_file);
    exit(0);
}
