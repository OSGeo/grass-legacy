/* %W% %G% */
#include <ctype.h>
#include <string.h>
#include <math.h>
#include  "gis.h"

double sfactor(scale, size,in_name,mapset,dot_name,name_opt)
double scale, size;
char *in_name, *mapset, *dot_name;
int name_opt;
{
struct Categories cats ;
FILE *fd, *sf, *tmp;
char *chr, stat_name[200], *tmp_file, buff[300];
char *ptr, *poly, *dots;
int i, ii, icat, cat;
double sfact=0.0,t_area, g_size, dot_cnt ;


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

g_size = (size * scale);
fprintf(stderr,"GSIZE %.3lf %.3lf %.3lf\n",size,scale,g_size);

while(fgets (buff, sizeof(buff), fd) )
	{
/*********************** GET_AREAS and DOT counts *******************************/
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
		sscanf(dots,"%lf",&dot_cnt);
						  
		if (name_opt) for (ii=0; ii < cats.count; ii++)
			/* using poly names, NOT numbers */
			if (strncmp(poly,cats.list[ii].label,strlen(poly)) == 0)
				cat = cats.list[ii].num;
			else         /* using poly numbers, NOT names */
				if (!isdigit(*poly))
					{
					G_fatal_error("Invalid dotfile, using names, NOT numbers");
					return(-1) ;
					}
			else
				cat = atoi(poly);

/******************************************/

/********************* READ dig_stats file ****************************/
		while ((chr = fgets(buff,300,sf)) != NULL) {

			sscanf(strtok(buff,":"),"%d",&icat);
			sscanf(strtok(NULL,":"),"%lf",&t_area);
			if (cat == icat) break;
			while (buff[strlen(buff)-1] != '\n' && chr != NULL)  chr = fgets(buff,300,sf);
			}
		rewind(sf);

/***********  PROCESSS AREAS ******************/

/*fprintf(stderr,"HERE %.3lf %.3lf %.3lf %.3lf\n",dot_cnt,g_size,t_area,sfact);*/
t_area = sqrt(t_area);

	if ( g_size/t_area < 1.0 && sfact < g_size * dot_cnt/t_area) sfact = g_size * dot_cnt/t_area;
	}

return(sfact);
}
