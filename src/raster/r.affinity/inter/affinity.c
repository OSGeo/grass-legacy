#define GLOBAL
#include "global.h"

affinity() 


{
    struct Categories cats,cats1;
    struct Colors colr;
    struct Ref group_ref;
    int nrows, ncols;
    int row;
    int band;
    int i;
    char *labels;
    double pixelnn;

    nrows = G_window_rows();
    ncols = G_window_cols();
    pixelnn = ((double)nrows)*((double)ncols);

   
  
   /* allocate memory for stats  */
   alloc_stats();


   /* initilize the stats */
printf ( "\n initializing... ");

   init_stats();

    /* statistic for all cells  */
fprintf (stderr, "\n computing affinity index... ");

   image_stats( nrows, ncols) ; 
/*
for(i=0;i<val_num[0];i++)
    printf("\n%d,%d,%d",statsb0[i].greyv,statsb0[i].count_c,statsb0[i].count_all); 
printf("\n\n"); 
for(i=0;i<val_num[0];i++)
   printf("\n%d,%d,%d",statsb0[val_num[0]+i].greyv,statsb0[val_num[0]+i].count_c,statsb0[val_num[0]+i].count_all);  */

    /* sort all the stats for every band and every training area */
fprintf (stderr, "\n sorting affinity index... ");
    sort_all_stats();

/*
for(i=0;i<val_num[0];i++)
printf("\n%d,%d,%d",statsb0[i].greyv,statsb0[i].count_c,statsb0[i].count_all); 
printf("\n\n");  
for(i=0;i<val_num[0];i++)
  printf("\n%d,%d,%d",statsb0[val_num[0]+i].greyv,statsb0[val_num[0]+i].count_c,statsb0[val_num[0]+i].count_all);  */

fprintf (stderr, "\n classifying... ");

   for(i=0;i<Ref.nfiles;i++) pixelnum[i]=pixelnn-pixelnum[i];

/* for(i=0;i<Ref.nfiles;i++) printf("nums: %f",pixelnum[i]); */

    for (row = 0; row < nrows; row++)
    {
       
      G_percent(row, nrows, 4);
    /*   G_zero_cell_buf(class_cell); */
    /*  G_zero_cell_buf(reject_cell); */
	
      for (band = 0; band < Ref.nfiles; band++)
	    if (G_get_map_row (cellfd[band], cell[band], row) < 0)
		exit1();
      classify(class_cell, reject_cell, ncols);
     
      /*    fprintf (stderr, "CLASSS %d",row); */

	G_put_map_row (class_fd, class_cell, row);
	if (reject_fd > 0)
	    G_put_map_row (reject_fd, reject_cell, row);
    } 


    G_percent(row, nrows, 4);



/* free stats  and close files*/
    free_stats();
    
    close_files();
    

/* create the cats file  for classification and reject hold map */

    G_read_cats(t_map,t_mapset,&cats1);
    
    G_init_cats((CELL)0,"affinity classification",&cats);
    G_set_cat ((CELL)0, "no data", &cats);
    for(i=0;i<clusternum;i++) {
      labels=G_get_cat((CELL)(cluster_val[i]),&cats1);
      G_set_cat((CELL)(cluster_val[i]),labels,&cats);
     }

    G_write_cats (class_name, &cats);
   
    G_free_cats (&cats); 
    G_free_cats (&cats1); 


    if (reject_fd > 0)
    {
       char title[100];
  
	sprintf (title, "Rejection Probability for %s", class_name);

	G_init_cats((CELL)21,title,&cats);
	G_set_cats_title (title, &cats);
	G_set_cat ((CELL)0, "no data", &cats);
	G_set_cat ((CELL)1, "0.01", &cats);
	G_set_cat ((CELL)2, "0.02", &cats);
	G_set_cat ((CELL)3, "0.03", &cats);
	G_set_cat ((CELL)4, "0.04", &cats);
	G_set_cat ((CELL)5, "0.05", &cats);
	G_set_cat ((CELL)6, "0.06", &cats);
	G_set_cat ((CELL)7, "0.07", &cats);
	G_set_cat ((CELL)8, "0.08", &cats);
	G_set_cat ((CELL)9, "0.09", &cats);
	G_set_cat ((CELL)10, "0.10", &cats);
	G_set_cat ((CELL)11, "0.12", &cats);
	G_set_cat ((CELL)12, "0.14", &cats);


	G_set_cat ((CELL)13, "0.16", &cats);
	G_set_cat ((CELL)14, "0.18", &cats);
	G_set_cat ((CELL)15, "0.20", &cats);
	G_set_cat ((CELL)16, "0.25", &cats);
	G_set_cat ((CELL)17, "0.30", &cats);
	G_set_cat ((CELL)18, "0.40", &cats);
        G_set_cat ((CELL)19, "0.50", &cats);
	G_set_cat ((CELL)20, "1.00", &cats);
	G_set_cat ((CELL)21, "bad", &cats);

	G_write_cats (reject_name, &cats);
	G_free_cats (&cats);


	G_make_grey_scale_colors (&colr, (CELL) 1, (CELL) 16);

	G_set_color ((CELL)0, 0, 255, 0, &colr);
	G_set_color ((CELL)17, 255, 0, 0, &colr);
	G_write_colors (reject_name, G_mapset(), &colr);
	G_free_colors (&colr);
    } 


/*  associate the output files with the group  */
    I_get_group_ref (group, &group_ref);
    I_add_file_to_group_ref (class_name, G_mapset(), &group_ref);
    if (reject_cell)
	I_add_file_to_group_ref (reject_name, G_mapset(), &group_ref);
    I_put_group_ref (group, &group_ref);
  /* ?????  make_history(class_name, group, subgroup, sigfile);    */
    return 0;
}
