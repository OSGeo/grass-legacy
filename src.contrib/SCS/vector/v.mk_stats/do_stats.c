/* %W% %G% */
/* do_dot.c    1.0   4/01/91
*                                                                       
*     Purpose                                                           
*        Extract area and label info from the digit file
*		by cat number.
*	    The out put contains cat:total area:list (area_num  area ...)
*/                                                                       

#include <ctype.h>
#include <stdio.h>
#include <unistd.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"

struct Map_info Map;
struct Categories cats ;

do_stats(in_name, name_opt, verbose, out)
char  *in_name;
int name_opt, verbose, out;
{
    int j, i, ii, cat, area_num, cnt;
    int vect_read, area[200];
    char c, buff[100], *mapset, label[100];
    char *tmp_file, stat_name[200];
    double f_area[200],t_area;
    FILE *tmp;
	P_AREA *Areas;


    mapset = G_find_file("dig",in_name,"");
 
    if (out) 
		{
		tmp_file = G_tempfile();
		if ((tmp = fopen(tmp_file,"w")) == NULL)
			{ 
			G_fatal_error("Creating temp file.") ;
			return(-1) ;
			} 
		}



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
	if (name_opt)
		{
    	G_read_vector_cats (in_name, mapset, &cats);
		cnt = cats.count;
		}
	else
		cnt = Map.n_atts;


    if (verbose) fprintf(stderr,"\nProcessing .....       \n");

for (j=0;j <= cnt;j++)
	{
/*********************** GET_AREAS *******************************/
	if (name_opt)
		{
		sprintf(label,"%s",cats.list[j].label);
		cat = cats.list[j].num;
		}
	else
		{
		label[0] = NULL;
		cat = j;
		}

	ii = 0;
	t_area = 0;
	for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
		{
		if (! verbose)
			{
			if (i == 1) {c = '/';i = 0;}
			else {c = '\\';i = 1;}
			fprintf(stderr,"\b%c",c);
			}
		if (cat == Map.Att[Map.Area[area_num].att].cat)
			{
			area[ii] = area_num;
			if (V2_get_area(&Map,area_num,&Areas) != 0)
				G_fatal_error("could not get area info\n");
			dig_find_area2 (&Map, Areas, &(f_area[ii]));
			t_area += f_area[ii++];
			}
		}
	if (out && t_area > 0) 
		{
		fprintf(tmp,"%d:%.3lf:",cat,t_area);
		for (i=0;i < ii;i++)
		fprintf(tmp," %d %.3lf",area[i],f_area[i]);
		fprintf(tmp,"\n");
		}
	if (verbose && t_area > 0)
		{
		fprintf(stderr,"cat %d label %s total_area %lf ",cat,label,t_area);
		fprintf(stderr,"areas are:");
		for (i=0;i < ii;i++)
		fprintf(stderr,"%d,",area[i]);
		fprintf(stderr,"\n");
		}
	}


	if (out) {
		fclose(tmp);
                     /* open the stats file */
      G__make_omapset_element("dig_stats",mapset) ;
      G__file_name(stat_name, "dig_stats", in_name, mapset) ;
	  if (access(stat_name,F_OK))
		{
	  	sprintf(buff,"rm %s",stat_name);
	  	system(buff);
		}
	  sprintf(buff,"cp %s %s",tmp_file,stat_name);
	  system(buff);
	  sprintf(buff,"rm %s",tmp_file);
	  system(buff);
	  }
	if (! verbose) fprintf(stderr,"\b\n");
}
