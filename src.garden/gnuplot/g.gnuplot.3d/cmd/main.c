#define NFILES 20

#include "gis.h"
#include "Vect.h"

main(argc, argv) 
int   argc;
char *argv[];
{
    char *mapset;
    int i;
    int nfiles;
    int withcats;
    int elev;
    struct Map_info Map;
    struct line_pnts *Points;
    struct Categories cats;
    struct Cell_head window;
    CELL *cell, value, min, max;
    struct Range range;
    int row, col;
    double drow, dcol, exag;
    int in_window;
    double east, north, ave, diff;
    int line,n;
    char buf[1024];
    char buf1[100], buf2[100];
    char label[1024];
    char **ptr;
    double *x, *y;
    double G_northing_to_row();
    double G_easting_to_col();
    struct Option *opt1, *opt2, *opt3, *opt4, *opt5;
    struct Flag *flag1, *flag2;
    char name[100],name2[100],name3[100];
    FILE *out, *out2;
    
    opt1 = G_define_option() ;
    opt1->key        = "elev" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->multiple   = NO ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of elevation raster map" ;

    opt2 = G_define_option() ;
    opt2->key        = "lines" ;
    opt2->type       = TYPE_INTEGER ;
    opt2->required   = NO ;
    opt2->answer     = "1" ;
    opt2->multiple   = NO ;
    opt2->description= "Frequency of vector lines in output" ;

    opt3 = G_define_option() ;
    opt3->key        = "vect" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = NO ;
    opt3->multiple   = YES ;
    opt3->gisprompt  = "old,map,vector" ;
    opt3->description= "Name of vector map(s)" ;
    
    opt4 = G_define_option() ;
    opt4->key        = "term" ;
    opt4->type       = TYPE_STRING ;
    opt4->required   = NO ;
    opt4->multiple   = NO ;
    opt4->description= "Name of output terminal" ;

    opt5 = G_define_option() ;
    opt5->key        = "exaggeration" ;
    opt5->type       = TYPE_DOUBLE ;
    opt5->required   = NO ;
    opt5->answer     = "2.0" ;
    opt5->description= "vertical exaggeration factor" ;

    flag1 = G_define_flag() ;
    flag1->key         = 'c' ;
    flag1->description = "Elevation from the category label in the grid cell" ;
    
    flag2 = G_define_flag() ;
    flag2->key         = 'e' ;
    flag2->description = "Show elevation surface" ;
    
    G_gisinit (argv[0]);
    
    withcats = 0;
    nfiles = 0;
    
    if (G_parser(argc, argv))
      exit(-1);
    
    cell = G_allocate_cell_buf();
    G_get_window (&window);
    
    withcats = flag1->answer;
    ptr = opt1->answers;
    sscanf(opt2->answer,"%d",&line);
    sscanf(opt5->answer,"%lf",&exag);

    strcpy (name, *ptr);
    if(NULL == (mapset = G_find_cell2 (name, "")))
      die (name, " - not found");
    if(0 > (elev = G_open_cell_old (name, mapset)))
      die ("can't open", name);
    if (withcats && G_read_cats (name, mapset, &cats) < 0)
      die (name, " - can't read category file");
   
 
    sprintf(name2,"gnu.3d");

    if((out2 = G_fopen_new("gnuplot",name2)) == NULL) 
      die ("cannot open script file ", name2);
    fprintf(out2,"cd \"%s/%s/gnuplot\"\n",G_location_path(),G_mapset());
    fprintf(out2,"set data style line\n");
    fprintf(out2,"set parametric\n");

    if ((window.north-window.south) < (window.east-window.west))
      {
	ave=(window.north+window.south)/2;
	diff=(window.east-window.west)/2;
	fprintf(out2,"set xrange[%lf:%lf]\n",window.west,window.east);
	fprintf(out2,"set yrange[%lf:%lf]\n",ave-diff,ave+diff);
      }
    else
      {
	ave=(window.west+window.east)/2;
	diff=(window.north-window.south)/2;
	fprintf(out2,"set yrange[%lf:%lf]\n",window.south,window.north);
	fprintf(out2,"set xrange[%lf:%lf]\n",ave-diff,ave+diff);
      };
    
    if ((G_read_range(name,mapset,&range))<0)
	die("cannot open range file for ",name,". Run r.support first.");
    G_get_range_min_max(&range,&min,&max);
    fprintf(out2, "set zrange[%lf:%lf]\n",(double)min,min+diff*2/exag);

    if (opt4->answer)
      fprintf(out2,"set term %s\n", opt4->answer);  
      
    fprintf(out2,"splot ");

    if (flag2->answer)
      {
        sprintf(name3,"%s.r",name);
	if((out = G_fopen_new("gnuplot",name3)) == NULL) 
	  die ("cannot open rast outpufile");
	row=1;
/*	while (row < G_window_rows())*/
	while (row < window.rows)
	  {
	    col=1;
	    if (G_get_map_row(elev, cell, row) < 0)
	      die (argv[1], " - can't read");
/*	    while (col<G_window_cols())*/
	    while (col<window.cols)
	      {
		east=G_col_to_easting(col+0.5,&window);
		north=G_row_to_northing(row+0.5,&window);
		/*printf("%d %d ",row,col);*/
		fprintf(out,"%lf %lf",east,north);
		if (withcats)
		  fprintf(out," %s\n",G_get_cat(value, &cats));
		else
		  {
		    value = cell[col];
		    fprintf(out," %ld\n",(long) value);
		  }
		  col+=line;
	      }
	    fprintf(out,"\n");
	    row+=line;
	  }
	fclose(out);
	fprintf(out2," \"%s\"",name3);
      } /* if raster is drawn */
    
    
    /* Now let's handle the vector files */
    nfiles=0;
    ptr = opt3->answers;
    Points = Vect_new_line_struct ();
    if (opt3->answers)
      {
	for (; *ptr != NULL; ptr++)
	  { 
	    char name[100];
	    strcpy (name, *ptr);
	    if(NULL == (mapset = G_find_vector2 (name, "")))
	      die (name, " - not found");
	    Vect_set_open_level (1);
	    if (1 > Vect_open_old (&Map, name, mapset))
	      die ("can't open", name);
	    
	    sprintf(name3,"%s.v",name);
	    if((out = G_fopen_new("gnuplot",name3)) == NULL)
	      die ("can't open outputfile", name3);
	    
	    if (flag2->answer || nfiles > 0)
		fprintf(out2,",");
	    nfiles++;
	    fprintf(out2,"\"%s\"", name3);
	    while (1)
	      {
		switch (Vect_read_next_line (&Map, Points))
		  {
		case -1:
		  Vect_close (&Map);
		  fclose(out);
		  fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);            return -1;
		case -2: /* EOF */
		  Vect_close (&Map);
		  fclose(out);
		  goto done;
		  }
		x=Points->x;
		y=Points->y;
		
		while(Points->n_points--)
		  {
		    G_format_northing (*y, buf1, -1);
		    G_format_easting (*x, buf2, -1);
		    
		    G_scan_northing (buf1, &north, window.proj);
		    G_scan_easting (buf2, &east, window.proj);
		    
		    drow = G_northing_to_row(north, &window);
		    dcol = G_easting_to_col(east, &window);
		    
		    if (drow == window.rows) drow--;
		    if (dcol == window.cols) dcol--;
		    
		    row = (int) drow;
		    col = (int) dcol;
		    
		    /* printf("%d %d %lf %lf\n",row,col,east,north);*/
		    if (row >= 0 && row < window.rows && col >= 0 && col < window.cols)
		      {
			fprintf(out,"%lf %lf",east,north);
			if (withcats)
			  fprintf(out," %s\n",G_get_cat(value, &cats));
			else
			  {
			    if (G_get_map_row(elev, cell, row) < 0)
			      die (argv[1], " - can't read");
			    value = cell[col];
			    fprintf(out," %ld\n",(long) value);
			  }
		      }
		    else
		      fprintf(out,"\n");
		  x++;
		  y++;
		  }
	
		fprintf(out,"\n");
	      }
	    done:
	      continue;
	  }
    
      }
    fprintf(out2,"\n");
    fclose(out2);
    sprintf(buf,"g.gnuplot -persist %s/%s/gnuplot/%s",G_location_path(),G_mapset(), name2);
    system(buf);
}

oops (line, buf, msg)
    char *buf, *msg;
{
  static int first = 1;
    if (!isatty(0))
      {
	if (first)
	  {
	    fprintf (stderr, "%s: ** input errors **\n",
		     G_program_name());
	    first = 0;
	  }
	fprintf (stderr, "line %d: %s\n", line, buf);
      }
    fprintf (stderr, "** %s **\n", msg);
}
