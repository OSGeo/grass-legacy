/*  From preliminary work (pull.vect) by Dave Gerdes - CERL
 *  created by:         R.L.Glenn, SCS
 *  modified to function by RB 5/2000
 *
 * Function reclass() reads input vector map and writes reclassed elements to output map.
 *
 * Arguments:
 * in_name - name of input vector map
 * out_name - name of output vector map
 * new - reclass table
 * type - elements type 
 * optiond - do not output boundaries between areas with the same cat 
 *
 * Returns:
 * number of elements created or -1 on error
 */

#include <stdlib.h>
#include <string.h>
#include  "gis.h"
#include  "dbmi.h"
#include "Vect.h"

#define		DIG_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define		CAT_DIR		"dig_cats"

static int  codes();
static int  new_cat();
static int  srch();

int reclass (char *in_name, char *out_name, dbCatValI *new, int ncat, unsigned int type, int optiond)
{
	FILE *outa;
	struct Map_info Map, Out_Map;
	struct line_pnts *Points;
	int left, right, lcat, rcat, cat_num, cat1, cat_no;
	int l_isle, r_isle, larea, rarea;
	int day, yr, typ;
	int vect_read, rclelem=0;
	register int area_num, ii,jj;
	char  name[150], date[40], mon[4], *mapset, errmsg[200];
	double xx, yy;
	P_LINE *Lines;

	if ((mapset = G_find_file(DIG_DIR,in_name,"")) == NULL)
	{
		sprintf(errmsg,"Can't find <%s> vector dig file.",in_name) ;
		G_fatal_error(errmsg) ;
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

		if ( (Lines->type & type & LINE) || (Lines->type & type & DOT) )
		{
		    /* get the category */
		    cat1 = cat_num = Map.Att[Lines->att].cat;     

        	    /* compare to the conversion list */
	    	    cat1 = new_cat ( new,  ncat, cat1);
		    if (!cat1) continue;  

		    /* read and write line */
            	    if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
		            fprintf (stderr, "Out of Memory\n"), exit (-1);
	            Vect_write_line (&Out_Map, Map.Line[ii].type, Points);

        	    /* put out the attribute info for this line */
	            jj = Map.Line[ii].att;   
		
		    fprintf( outa, "%c %14.7f %14.6f   %8d\n",
		         codes(Map.Att[jj].type), Map.Att[jj].x,
		          Map.Att[jj].y,cat1); 
			  
		    rclelem++;	  
		}
		else if (Lines->type & type & AREA)
		{
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
		    left = new_cat ( new,  ncat, lcat);
		    right = new_cat ( new,  ncat, rcat);
		
		    /* if area not labeled */
		    if ( left == 0 && right == 0 )	continue;
		
		    /* if user requested -d option */
		    if (optiond)  
			if (left == right)
			{
				Map.Area[larea].alive = 0;
				continue;
			}
		    /* create this line */
		    /* read and write line */
		    if (0 > V1_read_line(&Map, Points, Map.Line[ii].offset))
			fprintf (stderr, "Out of Memory\n"), exit (-1);
		    Vect_write_line (&Out_Map, Map.Line[ii].type, Points);
		}

	}  /* end lines section */
	Vect_close (&Out_Map);


	/* ------------------ Area attributes section ------------------- */
	if (type & AREA)
	{
	    for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
	    {
		if (Map.Area[area_num].alive)
		{
			/* get the category number for area "area_num" */
			cat1 = cat_no = Map.Att[Map.Area[area_num].att].cat;

			/* compare to the conversion list */
			cat1 = new_cat ( new,  ncat, cat_no);

			if (!cat1) continue;

			xx = Map.Att[Map.Area[area_num].att].x;
			yy = Map.Att[Map.Area[area_num].att].y;
			typ = Map.Att[Map.Area[area_num].att].type;

			fprintf(outa, "%c %14.7f %14.6f   %8d\n", 
			    codes(typ), xx, yy, cat1);
			rclelem++;
		}
	    }  
	}  /* end area attributes section */

	fclose (outa);
	Vect_close (&Map);
	return(rclelem) ;
}

int
new_cat (dbCatValI *new,  int ncat, int old)
{
    dbCatValI *newcat;
    
    newcat = (dbCatValI *) bsearch((void *) &old, new, ncat, sizeof(dbCatValI), srch);  

    if ( newcat != NULL)
    {
	return (newcat->val);
    }
    return (0);
}

int
codes (int type)
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

int srch ( const void *pa, const void *pb)
{
    int       *p1 = (int *) pa;
    dbCatValI *p2 = (dbCatValI *) pb;

    if( *p1 < p2->cat)
        return -1;
    if( *p1 > p2->cat)
        return 1;
    return 0;
}
