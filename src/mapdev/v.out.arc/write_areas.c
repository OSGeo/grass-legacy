
#include "gis.h"
#include "digit.h"
#include "gtoa.h"

write_areas(name,mapset,map,lines_file,points_file,text_file)
char   *name,
*mapset;
struct Map_info *map;
FILE   *lines_file,
*points_file,
*text_file;
{
	P_AREA *Area;
	P_ISLE *Isle;
	double ax, ay;
	double *x, *y;
	int area;
	int type;
	int offset;
	int count;
	int attflag, NumPoints ;
	char *att;
	int alloc_points;
	int n_points;
	int n_atts;
	int i,n,catflag= -1;
	plus_t zero = 0;
	struct line_pnts *Gpoints;
	struct Categories cats;
	int n_lines;
	int NumAreas;

#ifdef DEBUG
	printf("write_areas %s %s\n",name,mapset);
#endif

	catflag = G_read_vector_cats(name,mapset,&cats);
		
        if(catflag == -1)
	{
#ifdef DEBUG
		printf("   error %d reading vector cats\n",catflag);
#  endif
	}

	/* LOOP through all areas in the dig file */
	count=0;
#ifdef OLD_LIB
	NumAreas = dig_P_num_areas(map);
#else
	NumAreas = V2_num_areas(map);
#endif /*OLD_LIB*/

	for (area = 1 ; area <= NumAreas; area++)
	{
		Area = &(map->Area[area]);
		attflag=0;
		if (Area->att || area == 1)
			n_atts=1;
		else
			n_atts=0;

		/* write label-point coordinates to points_file */
		if (AREA_LABELED (Area))
		{
			count++;
			attflag=1;
			ax = map->Att[Area->att].x;
			ay = map->Att[Area->att].y;
			fprintf(points_file,"%d%c%lf%c%lf\n",count, separator, 
					    ax, separator, ay);
			lab_flg=1;
		}

		/* write category number and attribute text to text_file */
		if (attflag==1 && catflag!=-1)
		{
 		   /* get attribute text */
		   i=0; 
		   att = G_get_cat(map->Att[Area->att].cat, &cats);
		   if(space)
		   {
		      while(att[i])
		      {
			 if(att[i] == ' ') att[i] = '_';
			 i++;
                      }
                   }
		   fprintf(text_file,"%d%c%d%c%d%c%s\n",count, separator, 
				    map->Att[Area->att].cat, separator,
				    count, separator, att);
			txt_flg=1;
		}
	}

	fprintf(points_file,"END\n");
	fprintf(text_file,"END\n");
}
