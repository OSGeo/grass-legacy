/*
**  v.patch  input=file1,file2,.... output=composite
**
**   patch 2 or more vector files together creating composite
**
**  the program is very straight forward.  expand header, copy info from
**  each map one at a time into the new map.  then go back and write the new
**  header out.  last thing to do is to 'cat' all the dig_att files together
**
**  no checking is done for overlapping lines.
**  header information will have to be editted afterwards.
*/

/*
**  Written by Dave Gerdes  8/1988
**  US Army Construction Engineering Research Lab
*/
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"

int patch(struct Map_info *, struct Map_info *, struct line_pnts *);

int 
main (int argc, char *argv[])
{
	register int i, ret, error;
	char path[1024],filename[1024];
	char *in_name, *out_name, *mapset;
	char errmsg[200];
	FILE *Out, *In;
	struct dig_head d_head;
	struct Option *old, *new, *title;
	struct Cell_stats stats;

	struct Map_info InMap, OutMap;
	struct line_pnts *Points;

	int max_cat;
	int cat, n_files;
	char type;
	int cats_exist;
	double x, y;
	long offset;

	struct Categories *old_cats, new_cats;

	setbuf (stdout, NULL);
	G_gisinit (argv[0]);

	old = G_define_option();
	old->key		= "input";
	old->type		= TYPE_STRING;
	old->required		= YES;
	old->multiple		= YES;
	old->gisprompt		= "old,dig,vector";
	old->description	= "vector map(s)--source for composite";

	new = G_define_option();
	new->key		= "output";
	new->type		= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->gisprompt		= "any,dig,vector";
	new->description	= "new vector composite";

	title = G_define_option();
	title->key		= "title";
	title->type		= TYPE_STRING;
	title->required		= NO;
	title->multiple		= NO;
	title->answer           = "";
	title->description	= "new categories table title";

	if (G_parser (argc, argv))
		exit(-1);

	out_name = new->answer;
	/*
	if ((Out = G_fopen_vector_new (out_name)) == NULL)
	{
		fprintf (stderr, "Cannot write '%s'\n", out_name);
		exit(-1);
	}
	*/
	i=0;
        while (old->answers[i])
        {
            in_name = old->answers[i++];
	    mapset = G_find_vector(in_name,"");
	    if(strcmp(G_fully_qualified_name(in_name, mapset),
			G_fully_qualified_name(out_name, G_mapset()))
			==0)
                 G_fatal_error(
		    "The output file name must differ from input file names");
        }

	if (0 > Vect_open_new (&OutMap, out_name))
	{
	   sprintf(errmsg, "Not able to open vector file <%s>\n", out_name) ;
	   G_fatal_error (errmsg);
	}


    /*initialize Points struct*/
	Points = Vect_new_line_struct();

	/*Points.alloc_points = 0;*/
	/* place holder */
	/*obsolete with new Vect lib:
	dig_write_head_binary (Out, &d_head);
	*/

	fprintf (stdout, "\n");
	i = 0;
	while (old->answers[i])
	{
		in_name = old->answers[i++];
		fprintf (stdout, "    Patching file %s\n", in_name);
		if ((mapset = G_find_vector2 (in_name, "")) == NULL)
		{
			sprintf (errmsg, "Could not find Vector file <%s>\n", in_name);
			G_fatal_error (errmsg);
		}

		/*old method--pre Vlib*/
		/*********
		if ((In = G_fopen_vector_old (in_name, mapset)) == NULL)
		{
			fprintf (stderr, "Cannot open Vector file <%s>\n", in_name);
			continue;
		}
		if (dig_init (In))
		{
			sprintf (errmsg, "Cannot initialize Vector file <%s>\n", in_name);
			G_fatal_error (errmsg);

		}
		if (i == 1) *first time around--initialize Head struct *
			dig_read_head_binary (In, &d_head);
        **********/
		
		/*new method-with Vect lib*/
		if ((Vect_open_old (&InMap, in_name, mapset)) < 1)
		{
			fprintf (stderr, "Cannot open Vector file <%s>\n", in_name);
			continue;
		}
		if (i == 1)/*first time around, copy first in head to out head*/
		   Vect_copy_head_data (&InMap.head, &OutMap.head);


	  /*ret = patch (In, Out, &OutMap.head);*/
		ret = patch (&InMap, &OutMap, Points);
		if (ret < 0)
			fprintf (stderr, "Error reading file '%s'.  Some data may not be correct\n", in_name);

		Vect_close (&InMap);
		/* obsolete
		dig_fini (In);
		fclose (In);
		*/
	}
	n_files = i;
	/* debug: fprintf(stdout,"n_files: %d\n", n_files);*/

	/*
	strcpy (d_head.map_name, "Output from v.patch");
	strcpy (d_head.your_name, G_whoami ());
	*/
	strcpy (OutMap.head.map_name, "Output from v.patch");
	strcpy (OutMap.head.your_name, G_whoami ());

	/* obsoleted by Vlib:*/
	/* dig_write_head_binary (Out, &d_head);
	   fclose (Out); 
	 */
	Vect_close (&OutMap);
	Vect_destroy_line_struct (Points);

	fprintf (stdout, "\n    Patching Attribute files\n");

	if ((Out = G_fopen_new("dig_att", out_name)) == NULL)
	{
		fprintf (stderr, "Could not write to new attribute file\n");
		goto end;
	}

	i = 0;
	cats_exist=0;
	G_init_cell_stats(&stats);
	while (old->answers[i])
	{
	   int stat;
		in_name = old->answers[i++];
		fprintf (stdout, "    Processing attribute file %s\n", in_name);
		mapset = G_find_vector(in_name,"");
		if (G_find_file ("dig_att", in_name, mapset) == NULL)
		{
			fprintf (stderr, "Cannot find attribute file for <%s>\n", in_name);
			continue;
		}
		if ((In = G_fopen_old ("dig_att", in_name, mapset)) == NULL)
		{
			fprintf (stderr, "Cannot open attribute file for <%s>\n", in_name);
			continue;
		}
        	while ((stat = read_att(In, &type, &x, &y, &cat, &offset))!=1)
		{
		   if(stat==0) 
		   {
		      G_update_cell_stats((CELL *) &cat,1,&stats);
		      cats_exist=1;
		      write_att (Out, type, x, y, cat);
		   } 
                }
		fclose (In);
	}
	fclose (Out);

        if(cats_exist)
	{
	     char *temp_buf;
	     char cat_title[100];
	     long count;

	     fprintf (stdout,"Writing category file...\n");
	     G_rewind_cell_stats(&stats);
	     old_cats = (struct Categories*) G_malloc(sizeof(struct Categories)*n_files);
	     i=0;
	     while (old->answers[i])
             {
                 in_name = old->answers[i];
	         mapset = G_find_vector(in_name,"");
	         if(G_read_vector_cats(in_name, mapset, &(old_cats[i])))
	          G_init_cats((CELL)0,"", &(old_cats[i]));
	         i++;
	     }
	     if(strcmp(title->answer,"")==0) 
	     /* if no new categories title provided, set the new title 
		to the first existing cat title of the input files */
	     {
		 for(i=0;i<n_files;i++)
		 {
	            strcpy(cat_title, G_get_cats_title(&old_cats[i]));
		    if(strcmp("",cat_title)!=0) break;
	         }
             }
             else strcpy(cat_title, title->answer);

	     G_init_cats((CELL)0, cat_title, &new_cats);

             while(G_next_cell_stat((CELL *)&cat, &count, &stats)) 
	     {
	         i=0;
	         while (old->answers[i])
	         {
                    in_name = old->answers[i];
	   	    mapset = G_find_vector(in_name,"");
		    temp_buf = G_get_cat(cat, &(old_cats[i]));
		    if (strcmp(temp_buf,"")!=0)
		    {
		         G_set_cat(cat, temp_buf, &new_cats);
		         /* debug:  fprintf(stdout,
		        "%d:   %s\n", cat, temp_buf);*/
		         break; /* i - loop */ 
                    }
		    i++;
                 }
              }
	      for(i=0;i<n_files;i++)
	      {
                      G_free_cats(&(old_cats[i]));
              }
	      G_write_vector_cats(out_name, &new_cats);
              G_free_cats(&new_cats);
	}
	G_free_cell_stats(&stats);

end:
	fprintf (stdout, "\n");
	fprintf (stdout, "  Patch complete. You must now run v.support to build dig_plus file.\n\n");
	fprintf (stdout, "  Intersections at borders will have to be snapped. Try having\n  v.support run with a very small snapping threshold.\n\n");
	fprintf (stdout, "  Lines common between files will have to be edited.\n\n");
	fprintf (stdout, "  The header information also may have to be edited.\n\n");

	exit (0);
}

/*
patch (In, Out, d_head)
FILE *In, *Out;
struct dig_head *d_head;
*/
/*new*/
int patch (
struct Map_info *InMap,
struct Map_info *OutMap,
struct line_pnts *Points)
{
	register int itype;
	/*struct dig_head local_head;*/
	long offset, ftell ();
	int cnt = 0;

	/*dig_read_head_binary (In, &local_head);
	fseek (Out, 0l, 2);
	*/

	OutMap->head.orig_scale = GREATER (OutMap->head.orig_scale, InMap->head.orig_scale);
	/*  this is not in the file
    d_head->digit_thresh = GREATER (d_head->digit_thresh, local_head.digit_thresh);
    */
	OutMap->head.digit_thresh = 0;

	OutMap->head.map_thresh = GREATER (OutMap->head.map_thresh, InMap->head.map_thresh);
	OutMap->head.N = GREATER (OutMap->head.N, InMap->head.N);
	OutMap->head.E = GREATER (OutMap->head.E, InMap->head.E);
	OutMap->head.W = LESSER (OutMap->head.W, InMap->head.W);
	OutMap->head.S = LESSER (OutMap->head.S, InMap->head.S);

	/***
	offset = ftell (In);
	if (offset < 0)
		fprintf (stderr, "Ftell error?\n");
	while ((itype = dig__Read_line (&Points, In, offset)) > 0)
	{
		cnt++;
		dig__Write_line (Out, (char) itype, &Points);
		offset = ftell (In);
	}
	****/
	/*new with Vectlib*/
	while ((itype = Vect_read_next_line (InMap, Points)) > 0)
	{
		cnt++;
		Vect_write_line (OutMap, itype, Points);
	}

	if (itype != -2)
		return (-1);
	return (0);
}

