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

        function: map_sediment
        called by: answers_run 
        
        read the part of the answers output for element erosion/deposition.
        create new raster maps (names given by user).
*/

#include "answers.h"

map_sediment()
{

    int i;
    int j;
    int there;
    int enter_names;
    int ct;
    int loss_map_fd;
    int deposit_map_fd;
    int chnl_new_fd;
    int chnl_old_fd;
    int wshd_fd;
    int element_num;
    int chnl_num;
    int no_deposit;
    int no_loss;
    int no_chnl;
    float element_value;
    float chnl_value;
    char buf[200];
    FILE *data_fp;
    FILE *chnl_data_fp;
    CELL *wshd_buf;
    CELL *loss_cell_buf;
    CELL *deposit_cell_buf;
    CELL *chnl_new_buf;
    CELL *chnl_old_buf;
    CELL min, max, ave;
    struct Colors colr;
    struct Range range;
    struct Categories cats;

    G_clear_screen();

    printf("ANSWERS on GRASS %s Project\n\n", proj_name);
    printf("Now ready to extract data for sediment movement (if any) from\n");
    printf("the ANSWERS output. Three new raster maps will be created:\n");
    printf("net sediment loss, net sediment deposition, and deposition\n");
    printf("in channel elements.\n\n");

/* set default names */
   
    sprintf(loss_layer, "%s.loss", proj_name);
    sprintf(deposit_layer, "%s.deposit", proj_name);
    sprintf(chnl_deposit_layer, "%s.chnl", proj_name);

    printf("Default names for these maps are:\n\t%s\n\t%s\n\t%s\n\n",
    loss_layer, deposit_layer, chnl_deposit_layer);

    there = 0;
    if (G_find_cell(loss_layer, G_mapset()))
    {
        there = 1;
        printf("<%s> already exists in <%s>.\n", loss_layer,
        G_mapset());
    }
    if (G_find_cell(deposit_layer, G_mapset()))
    {
        there = 1;
        printf("<%s> already exists in <%s>.\n", deposit_layer,
        G_mapset());
    }
    if (G_find_cell(chnl_deposit_layer, G_mapset()))
    {
        there = 1;
        printf("<%s> already exists in <%s>.\n", chnl_deposit_layer,
        G_mapset());
    }
    printf("\n");
    
    enter_names = 0;
    if (there == 1)
    {
        if(!G_yes("Overwrite existing maps?", 1))
            enter_names = 1;
    }
    else
    {
        if(G_yes("Do you wish to use names different from the default?", 0))
            enter_names = 1;
    }
    complete[12] = complete[13] = complete[14] = 0;
    if (enter_names)
    {
    G_clear_screen();
    printf("\nEnter the new map names:\n\n");

    if(!G_ask_cell_new("Enter the name for the sediment *loss* map",
    loss_layer))
    {
        printf("\nno new map name given\ncancelling the creation of maps.\n\n");
        hit_return();
        complete[13] = 0;
        menu();
    }
    
    if(!G_ask_cell_new("Enter the name for the sediment *deposition* map",
    deposit_layer))
    {
        printf("\nno new map name given\ncancelling the creation of maps.\n\n");
        hit_return();
        complete[12] = 0;
        menu();
    }
    
    if(!G_ask_cell_new("Enter the name for the channel deposition map",
    chnl_deposit_layer))
    {
        printf("\nno new map name given\ncancelling the creation of maps.\n\n");
        hit_return();
        complete[14] = 0;
        menu();
    }
    }
    G_clear_screen();
    printf("\nCreating sediment movement maps from ANSWERS output...\n\n");

    data_fp = G_fopen_old(data_dir, "out_sediment", proj_mapset);
    if (!data_fp)
        croak(1, "could not open <out_sediment> in project database");

    chnl_data_fp = G_fopen_old(data_dir, "out_chnl", proj_mapset);
    if (!chnl_data_fp)
        croak(1, "could not open <out_chnl> in project database");

    deposit_map_fd = G_open_cell_new(deposit_layer);
    chnl_new_fd = G_open_cell_new(chnl_deposit_layer);
    loss_map_fd = G_open_cell_new(loss_layer);

    if ((chnl_new_fd < 0) || (deposit_map_fd < 0) || (loss_map_fd < 0))
        croak(1, "could not create new raster maps");

    wshd_fd = G_open_cell_old(mask_layer, mask_mapset);
    if (wshd_fd < 0)
        croak(1, "could not open project mask map");

    chnl_old_fd = G_open_cell_old(chnl_layer, chnl_mapset);
    if (chnl_old_fd < 0)
    {
        sprintf(buf, "could not open channel map <%s>", chnl_layer);
        croak(1, buf);
    }

    deposit_cell_buf = G_allocate_cell_buf();
    loss_cell_buf = G_allocate_cell_buf();
    chnl_old_buf = G_allocate_cell_buf();
    wshd_buf = G_allocate_cell_buf();
    chnl_new_buf = G_allocate_cell_buf();
       
    fprintf(stderr, "Percent complete: ");
    no_chnl = no_deposit = no_loss = 0;
    ct = 0;
    for (i = 0; i < window.rows; i++)
    {
        G_get_map_row(wshd_fd, wshd_buf, i);
        G_get_map_row(chnl_old_fd, chnl_old_buf, i);
        G_zero_cell_buf(deposit_cell_buf);
        G_zero_cell_buf(loss_cell_buf);
        G_zero_cell_buf(chnl_new_buf);
        percent( i, window.rows, 5);

       for (j = 0; j < window.cols; j++)
        {
            if (wshd_buf[j] > 0)
	    {
	    	ct++;
                fscanf(data_fp, "%d %f", &element_num, &element_value);
                if (element_num != ct)
                {
                    printf("\n\nexpecting element %d, found %d\n",
                    ct, element_num);
                    printf("Check answers_output file for more diagnostic information\n");
                    croak(1,
                    "problem reading element data in ANSWERS output");
                }
                if(element_value > 0)
                {
                    deposit_cell_buf[j] = (int)element_value;
                    no_deposit = 1;
                }
                else if(element_value < 0)
                {
                    loss_cell_buf[j] = (int)(element_value * -1);
                    no_loss = 1;
		}
		}
            if (chnl_old_buf[j] > 0)
	    {
                fscanf(chnl_data_fp, "%d %f", &chnl_num, &chnl_value);
 
                if (chnl_num != ct)
                {
                    printf("\n\nexpecting channel element %d, found %d\n",
                    ct, chnl_num);
                    printf("Check answers_output file for more diagnostic information\n");
                    croak(1,
                    "problem reading channel element data in ANSWERS output");
                }
                if(chnl_value > 0)
                {
                    chnl_new_buf[j] = (int)chnl_value;
                    no_chnl = 1;
                }
		}
	    }
	    G_put_map_row(deposit_map_fd, deposit_cell_buf);
	    G_put_map_row(loss_map_fd, loss_cell_buf);
	    G_put_map_row(chnl_new_fd, chnl_new_buf);
	}

        percent( i, window.rows, 5);

        if (no_deposit == 1)
        {
	    G_close_cell(deposit_map_fd);
            complete[12] = 1;
        }
        else
            G_unopen_cell(deposit_map_fd);

        if (no_loss == 1)
        {
	    G_close_cell(loss_map_fd);
            complete[13] = 1;
        }
        else
            G_unopen_cell(loss_map_fd);

        if (no_chnl == 1)
        {
	    G_close_cell(chnl_new_fd);
            complete[14] = 1;
        }
        else
            G_unopen_cell(chnl_new_fd);

	G_close_cell(wshd_fd);
	G_close_cell(chnl_old_fd);

/* write the cats cell title and colors */

        if (no_deposit == 1)
        {
	    deposit_map_fd = G_open_cell_old(deposit_layer, G_mapset());
            G_read_cats(deposit_layer, G_mapset(), &cats);
            sprintf(buf, "ANSWERS %s Project Sediment Deposition (kg/ha)",
            proj_name);
            G_set_cats_title (buf, &cats);
            G_set_cat((CELL)0, "no data", &cats);
            G_write_cats (deposit_layer, &cats);
	    G_free_cats(&cats);
	    G_close_cell(deposit_map_fd);
	
            G_init_colors (&colr);
            G_read_range(deposit_layer, G_mapset(), &range);
            G_get_range_min_max(&range, &min, &max);
            ave = (long)((double)min + (double)max)/2;
            G_add_color_rule (min, 140, 140, 140, ave, 140, 140, 255, &colr);
            G_add_color_rule (ave, 140, 140, 255, max, 0, 200, 255, &colr);
            G_set_color (0, 0, 0, 0, &colr);
            G_write_colors (deposit_layer, G_mapset(), &colr);
            G_free_colors (&colr);
        }
        else
        {
            printf("\n\nNo sediment deposition reported. <%s> not created.\n",
            deposit_layer);
        }

        if (no_loss == 1)
        {
	    loss_map_fd = G_open_cell_old(loss_layer, G_mapset());
            G_read_cats(loss_layer, G_mapset(), &cats);
            sprintf(buf, "ANSWERS %s Project Sediment Loss (kg/ha)", 
            proj_name);
            G_set_cats_title (buf, &cats);
            G_set_cat((CELL)0, "no data", &cats);
            G_write_cats (loss_layer, &cats);
	    G_free_cats(&cats);
	    G_close_cell(loss_map_fd);
    	
            G_init_colors (&colr);
            G_read_range(loss_layer, G_mapset(), &range);
            G_get_range_min_max(&range, &min, &max);
            ave = (long)((double)min + (double)max)/2;
            G_add_color_rule (min, 225, 255, 0, ave, 192, 128, 64, &colr);
            G_add_color_rule (ave, 192, 128, 64, max, 255, 0, 0, &colr);
            G_set_color (0, 0, 0, 0, &colr);
            G_write_colors (loss_layer, G_mapset(), &colr);
            G_free_colors (&colr);
        }
        else
        {
            printf("\n\nNo sediment loss reported. <%s> not created.\n",
            loss_layer);
        }

        if (no_chnl == 1)
        {
	    chnl_new_fd = G_open_cell_old(chnl_deposit_layer, G_mapset());
            G_read_cats(chnl_deposit_layer, G_mapset(), &cats);
            sprintf(buf, 
            "ANSWERS %s Project Channel Sediment Deposition (kg)",
            proj_name);
            G_set_cats_title (buf, &cats);
            G_set_cat((CELL)0, "no data", &cats);
            G_write_cats (chnl_deposit_layer, &cats);
	    G_free_cats(&cats);
	    G_close_cell(chnl_new_fd);
	
            G_init_colors (&colr);
            G_read_range(chnl_deposit_layer, G_mapset(), &range);
            G_get_range_min_max(&range, &min, &max);
            ave = (long)((double)min + (double)max)/2;
            G_add_color_rule (min, 140, 140, 140, ave, 140, 140, 255, &colr);
            G_add_color_rule (ave, 140, 140, 255, max, 0, 200, 255, &colr);
            G_set_color (0, 0, 0, 0, &colr);
            G_write_colors (chnl_deposit_layer, G_mapset(), &colr);
            G_free_colors (&colr);
        }
        else
        {
            printf("\n\nNo channel deposition reported. <%s> not created.\n",
            chnl_deposit_layer);
        }

    printf("\ndone.\n");
    hit_return();
    return(0);
}

