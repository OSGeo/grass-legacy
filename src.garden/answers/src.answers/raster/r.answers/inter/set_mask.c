/* %G% %W% */
  /*
	
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

        function:  set_mask 
        called by: main, copy_project and step_1 

        scan thru mask layer, create reclass rules to make MASK, create
        the project.ELEMENT map for user reference...

*/

#include "answers.h"

set_mask()
{

    int i,
        j,
        ct,
        ct2;
    int ele_num_fd,
        wshd_fd;
    char buf[200];
    char reclass_file[200];
    char element_name[80];
    FILE *reclass_fp,
        *data_fp;
    CELL *wshd_buf,
        *ele_num_buf;
    CELL min, max;
    struct Categories cats;
    struct Colors colr;
    struct Range range;



/* if complete[1] is not 1, then that means we haven't scanned
   the mask layer to get reclass rules, or we have changed something
   like resolution or region or mask layer, thus we need to do
   it over again. Otherwise we can set the mask from the reclass rules
   stored in a file in the project data directory */
   
    if (complete[1] != 1)
    {
        printf("\nScanning mask, creating element map and reclass rules.\n\n");
	data_fp = G_fopen_new(data_dir, "in_row_col");
	if (!data_fp)
	{
            fprintf(stderr, 
            "\n\7WARNING: could not create <in_row_col> in project database\n");
            hit_return();
            return(1);
        }
	sprintf(element_name, "%s.ELEMENT", proj_name);
	ele_num_fd = G_open_cell_new(element_name);
	if (ele_num_fd < 0)
	{
	    fprintf(stderr, 
            "\n\7WARNING: could not create new map <%s.ELEMENT> in <%s>\n",
            proj_name, G_mapset());
            hit_return();
            return(1);
	}
	wshd_fd = G_open_cell_old(mask_layer, mask_mapset);
	if (wshd_fd < 0)
	{
	    fprintf(stderr, 
            "\n\7WARNING: could not read map <%s> in <%s>\n",
            mask_layer, mask_mapset);
            hit_return();
            return(1);
	}

	ele_num_buf = G_allocate_cell_buf();
	wshd_buf = G_allocate_cell_buf();

/* create a file with rules for reclassing the watershed area into MASK layer */

	reclass_fp = G_fopen_new(data_dir, "reclass");
	if (!reclass_fp)
	{
            fprintf(stderr, 
            "\n\7WARNING: could not create <reclass> in project database\n");
            hit_return();
            return(1);
        }

	ct = 1;
	for (i = 0; i < window.rows; i++)
	{
	    G_get_map_row_nomask(wshd_fd, wshd_buf, i);
	    G_zero_cell_buf(ele_num_buf);

	    for (j = 0; j < window.cols; j++)
	    {
		if (wshd_buf[j] > 0)
		{
		    ele_num_buf[j] = ct;
		    ct++;
		    fprintf(reclass_fp, "%d=1\n", wshd_buf[j]);
		}
	    }
	    G_put_map_row(ele_num_fd, ele_num_buf);
	}
	
/* double check the number of cells "found" in the watershed previously, 
   since there is a chance that it is off by a strange artifact of playing
   with the region */
   
        cells_in_wshd = ct - 1;
   
	fclose(reclass_fp);
	G_close_cell(ele_num_fd);
	G_close_cell(wshd_fd);
	
/* set colors to rainbow and class 0 to black */

        G_init_colors (&colr);
        G_read_range(element_name, G_mapset(), &range);
        G_get_range_min_max(&range, &min, &max);
        G_make_rainbow_colors (&colr, min, max);
        G_set_color (0, 0, 0, 0, &colr);
        G_write_colors (element_name, G_mapset(), &colr);
        G_free_colors (&colr);

/* write the cats -- label each element's cat label to the row
   and col in which it resides. also write out the first part of the
   element info into a file (in_row_col) in the data dir. */

	ele_num_fd = G_open_cell_old(element_name, G_mapset());
        G_read_cats(element_name, G_mapset(), &cats);

        sprintf(buf, "ANSWERS on GRASS Project <%s> Element Map", proj_name);
        G_set_cats_title (buf, &cats);
        G_set_cat((CELL)0, "no data", &cats);

        ct2 = 1;
	for (i = 0; i < window.rows; i++)
	{
	    G_get_map_row_nomask(ele_num_fd, ele_num_buf, i);
	    for (j = 0; j < window.cols; j++)
	    {
		if (ele_num_buf[j] > 0)
		{
                    sprintf(buf, "row %d, col %d", i, j);
                    G_set_cat(ele_num_buf[j], buf, &cats);
/* if last element, print "9" as flag to ANSWERS */
                    if (ct2 == ct - 1)
                        fprintf(data_fp, "%3d%3d 9\n", i, j);
                    else
                        fprintf(data_fp, "%3d%3d  \n", i, j);
                    ct2++;
                }
	    }
        }
        fclose(data_fp);
        G_write_cats (element_name, &cats);
	G_free_cats(&cats);
	G_close_cell(ele_num_fd);
	
        printf("Created new map <%s.ELEMENT> in <%s>.\n", proj_name, G_mapset());
        printf("This map can serve as a guide to ANSWERS' references to\n");
        printf("watershed elements. (raster cells are numbered sequentially,\n");
        printf("their category descriptions give row and column values).\n\n");

        hit_return();
    }

    if (complete[1] != 1)
        printf("\nSetting current mask to watershed area.\n");

/* build path for reclass rule file */

    sprintf(reclass_file, "%s/%s/answers/data/%s/reclass", G_location_path(),
    proj_mapset, proj_name);

/* build command to call r.reclass */
    sprintf(buf,"r.reclass input=%s output=MASK < %s",
    mask_layer, reclass_file);

    if (G_system(buf) != 0)
    {
	fprintf(stderr, "\n\7WARNING: Could not reclass <%s> into project MASK\n", 
	mask_layer);
	fprintf(stderr, "using the following command:\n");
        fprintf(stderr, "%s\n", buf);
        hit_return();
	return(1);
    }
    return (0);
}

