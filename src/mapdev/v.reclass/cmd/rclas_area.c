static char rcsid[]="$Header: /usr3/4.0/src.contrib/SCS/vector/v.reclass/cmd/RCS/rclas_area.c,v 1.2 1992/12/13 10:35:58 grass Exp $";
/* @(#)rclas_area.c	1.1.2.1 1/14/93 */
/*  @(#)select_area.c     1.0  9/29/89   
 *  @(#)                  1.1  2/15/90    
 *  @(#)                  1.2  3/15/90    
 *  @(#)                  1.3  1/30/91  for 4.0
 *  From preliminary work (pull.vect) by Dave Gerdes - CERL
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector area records, outputting area lines
 * for any areas which match the conversion list.
 * for lines which have left and right categories in the list
 * will optionally(-d flag) NOT write to output (common remove).
 * The resulting map attribute is set to the approp. category
 * of the conversion list.
 */


#include  "gis.h"
#include "Vect.h"

#define		DIG_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define		CAT_DIR		"dig_cats"

struct Map_info Map;
struct Map_info Out_Map;
struct line_pnts *Points;
struct Plus_head Plus ;
char  buf[1024] ;

static	int   snapped = 0 ;

rclas_area(in_name, out_name, new, optiond)
int optiond;
struct Reclass *new;
char  *in_name, *out_name;
{
	FILE *out, *outc, *fp_plus, *outa;
	int line, left, right, lcat, rcat, cat1;
	int l_isle, r_isle, larea, rarea, latt, ratt, max_att=0;
	int cat, cnvto, cat_no;
	int day, yr, max, typ;
	int vect_read;
	register int area_num, i, ii, jj, kk;
	char  name[150], input_att_file[128], date[40], bufr[80] ;
	char  mon[4], *mapset, *ptr;
	char *X, *Y;
	int n_points;
	double xx, yy;
	P_AREA *Areas;
	P_LINE *Lines;
	char label[60];

	if ((mapset = G_find_file(DIG_DIR,in_name,"")) == NULL)
	{
		G_fatal_error("Can't find <%s> vector dig file.",in_name) ;
		return(-1) ;
	}

	fprintf(stderr,
	    "\nLoading vector information for <%s>.\n", in_name);

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

	/* create dig directory, if not existing */
	G__make_mapset_element("dig") ;
	G__make_mapset_element("dig_plus") ;
	G__make_mapset_element("dig_cats") ;
	G__make_mapset_element("dig_att") ;

	/* Open output "dig" file */
	if ( Vect_open_new(&Out_Map, out_name) < 0)
	{
		fprintf(stderr,"Can't create output vector file <%s> \n", out_name) ;
		return (-1);
	}

	/* Open output "dig_att" file */
	G__file_name(name, ATT_DIR, out_name, G_mapset()) ;
	if ( (outa = fopen (name, "w")) == NULL)
	{
		fprintf(stderr,"Can't create output attribute file <%s> \n", name) ;
		return (-1);
	}


	/* Initialize the Point structure, ONCE */
	Points = Vect_new_line_struct();

	/* Read and write header info */
	sprintf(date,"%s",G_date());
	sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
	if (yr < 2000) yr = yr - 1900;
	else yr = yr - 2000;
	sprintf(date,"%s %d %d",mon,day,yr);
	Vect_copy_head_data(&Map.head, &Out_Map.head);
	strcpy( Out_Map.head.date,date);
	strcpy( Out_Map.head.your_name,out_name);
	strcpy( Out_Map.head.map_name,"Created by v.reclass");

	fprintf(stderr,"\n\tProcessing ");

	/* --------------------- Lines Section ------------------------------- */
	/* Cycle through all lines */
	for (ii=1; ii <= Map.n_lines; ii++)
	{
		G_percent(ii, Map.n_lines, 10);
		Lines = &(Map.Line[ii]);
		/* skip anything other than area lines */
		if (Lines->type != AREA)   continue;

		/* get the category for areas left & right */
		left = right = 0;
		lcat = rcat = 0;
		l_isle = r_isle = -1;

		if (Lines->left != 0)
		{
			if (Lines->left > 0)
			{
				lcat = Map.Att[Map.Area[Lines->left].att].cat;
				larea = Lines->left;
			}
			else
			{
				larea = abs(Lines->left);
				l_isle = Map.Isle[larea].area;
				lcat = Map.Att[Map.Area[l_isle].att].cat;
				larea = l_isle;
			}
		}

		if (Lines->right != 0)
		{
			if (Lines->right > 0)
			{
				rcat = Map.Att[Map.Area[Lines->right].att].cat;
				rarea = Lines->right;
			}
			else
			{
				rarea = abs(Lines->right);
				r_isle = Map.Isle[rarea].area;
				rcat = Map.Att[Map.Area[r_isle].att].cat;
				rarea = r_isle;
			}
		}

		/* compare to the conversion list */
		left = (lcat > new->max)||(lcat < new->min) ? 0 : (new->table[lcat - new->min]);
		right = (rcat > new->max)||(rcat < new->min) ? 0 : (new->table[rcat - new->min]);
		if (optiond)  /* if user requested -d option */
			if (left == right)
			{
#ifdef DEBUG
				printf("line= %d\n",ii);
				printf("\tlcat= %d, left= %d, Line_left= %ld, l_isle= %d\n",lcat,left,Lines->left,l_isle);
				printf("\trcat= %d, rite= %d, Line_rite= %ld, r_isle= %d\n",rcat,right,Lines->right,r_isle);
#endif

				Map.Area[larea].alive = 0;
#ifdef DEBUG
				printf("map area %d, alive= %d\n",larea,Map.Area[larea].alive);
#endif

				continue;
			}
		/* create this line */
		/* read and write line */
		if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
			fprintf (stderr, "Out of Memory\n"), exit (-1);
		Vect_write_line (&Out_Map, Map.Line[ii].type, Points);


	}  /* end lines section */
	Vect_close (&Out_Map);

	/* --------------------- Attributes Section -------------------------- */
	/* Cycle through all areas */
	for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
	{
#ifdef DEBUG
		printf("area# %d alive= %dcat = %d\n",area_num,Map.Area[area_num].alive,Map.Att[Map.Area[area_num].att].cat);
#endif
		if (Map.Area[area_num].alive)
		{
			/* get the category number for area "area_num" */
			cat1 = cat_no = Map.Att[Map.Area[area_num].att].cat;

			/* compare to the conversion list */
			cat1 = (cat_no > new->max) ||(cat_no < new->min)? 0 : new->table[cat_no - new->min];
			if (!cat1) continue;

			xx = Map.Att[Map.Area[area_num].att].x;
			yy = Map.Att[Map.Area[area_num].att].y;
			typ = Map.Att[Map.Area[area_num].att].type;
#ifdef DEBUG
printf("%d %d %d %d\n", cat1 ,cat_no,new->min,area_num);
			printf("\t Create new att info for %d\n", cat1);
			printf("\t%c%14.7lf%14.6lf   %8d\n", codes(typ), xx, yy, cat1);
#endif
			fprintf(outa, "%c %14.7lf %14.6lf   %8d\n", 
			    codes(typ), xx, yy, cat1);
			/* capture the highest attribute value */
			if (cat1 > max_att) max_att = cat1;
		}
	}  /* end attributes section */

	fclose (outa);
	/*fclose (cnv);*/
	Vect_close (&Map);
#ifdef DEBUG
	printf("\nmax_att= %d\n",max_att);
#endif
	return(max_att) ;
}

codes (type)
char type;
{
	switch (type) {
	case LINE:
		return ('L');
		break;
	case AREA:
		return ('A');
		break;
	case DOT:
		return ('P');
		break;
	case DEAD_LINE:
		return ('l');
		break;
	case DEAD_AREA:
		return ('a');
		break;
	case DEAD_DOT:
		return ('p');
		break;
	default:
		return ('X');
	}
}
