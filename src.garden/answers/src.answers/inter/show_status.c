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

   function:  show_status
   called by: misc

   */
#include "answers.h"

show_status()

{
    int i;
    char *status_file;
    FILE *status_fp;

    G_clear_screen();
    printf("\n\npreparing status report...\n\n");

    status_file = G_tempfile();
    status_fp = fopen (status_file, "w");
    if (!status_fp)
        croak(1, "unable to create and open tempfile for status report");

    printf("\n");

    fprintf(status_fp,"\n         ANSWERS on GRASS Project Current Status\n");

    fprintf(status_fp,"\nProject: %s in %s\n", proj_name, G_mapset());

    fprintf(status_fp,"MASK:    %s in %s\n", mask_layer, mask_mapset);

    fprintf(status_fp,"REGION:  N: %-10.f S: %-10.f E: %-10.f W: %-10.f\n",
    window.north, window.south, window.east, window.west);

    fprintf(status_fp,"\nNumber of Elements in Watershed: %d\n", cells_in_wshd);

    fprintf(status_fp,
    "Element size:   %.f X %.f meters    %.2f hectares     (%.2f acres)\n",
    window.ew_res, window.ew_res, (window.ew_res * window.ew_res / 10000),
    (window.ew_res * window.ew_res / 4046.856));

    fprintf(status_fp,
    "Watershed Size:                     %.2f hectares     (%.2f acres)\n",
    (cells_in_wshd * window.ew_res * window.ew_res / 10000),
    (cells_in_wshd * window.ew_res * window.ew_res / 4046.856));

    if (complete[6] > 0)
        fprintf(status_fp,"\nWatershed outlet at row: %d column: %d \n",
        out_row, out_col);
    fprintf(status_fp,"Watershed Element Map: %s.ELEMENT in %s\n\n",
    proj_name, proj_mapset);
    fprintf(status_fp,"\nInput Maps:\n");
    fprintf(status_fp,"__________\n\n");
    fprintf(status_fp,
    "(Note: \"not set\" indicates the corresponding project step has not\n");
    fprintf(status_fp,"been completed.)\n\n");

    if (complete[2] > 0)
        fprintf(status_fp,"Soils: %s in %s\n", soil_layer, soil_mapset);
    else
        fprintf(status_fp,"Soils: not set\n");

    if (complete[3] > 0)
        fprintf(status_fp,
        "Land Cover: %s in %s\n", cover_layer, cover_mapset);
    else
        fprintf(status_fp,"Land Cover: not set\n");

    if (complete[4] > 0)
    {
        fprintf(status_fp,"Slope: %s in %s\n", slope_layer, slope_mapset);
        fprintf(status_fp,
        "Aspect: %s in %s\n", aspect_layer, aspect_mapset);
    }
    else
    {
        fprintf(status_fp,"Slope: not set\n");
        fprintf(status_fp,"Aspect: not set\n");
    }

    if (complete[5] > 0)
    {
        fprintf(status_fp,"Rain Event Name: %s\n", rain_event);
        if (strcmp (rain_layer, "none") == 0)
            fprintf(status_fp,"Rain Gauge: no map used\n");
        else
            fprintf(status_fp,
            "Rain Gauge: %s in %s\n", rain_layer, rain_mapset);
    }
    else
        fprintf(status_fp,"Rain Gauge: not set\n");

    if (complete[7] > 0)
        if (strcmp (tile_area, "layer") == 0)
            fprintf(status_fp,
            "Subsurface Drainage: %s in %s\n", tile_layer, tile_mapset);
        else
            fprintf(status_fp,
            "Subsurface Drainage: %s of watershed\n", tile_area);
    else
        fprintf(status_fp,
        "Subsurface Drainage: not set\n");

    if (complete[8] > 0)
        fprintf(status_fp,"Channel: %s in %s\n", chnl_layer, chnl_mapset);
    else
        fprintf(status_fp,"Channel: not set\n");

    if (complete[9] > 0)
        if (strcmp (chnl_slp_layer, "none") == 0)
            fprintf(status_fp,"Channel Slope: %s used\n", chnl_slp_layer);
        else
            fprintf(status_fp,"Channel Slope: %s in %s\n",
            chnl_slp_layer, chnl_slp_mapset);
    else
        fprintf(status_fp,"Channel Slope: not set\n");

    if (complete[10] > 0)
    {
        fprintf(status_fp,"Best Managemnet Practices:\n");
        for (i = 0; i < 4; i++)
        {
            if ( bmp_tbl[i].set == 1 )
                fprintf(status_fp,"    %s: %s in %s\n", bmp_tbl[i].title,
                bmp_tbl[i].layer, bmp_tbl[i].mapset);
            else
                fprintf(status_fp,"    %s: none used\n", bmp_tbl[i].title);
        }
    }
    else
        fprintf(status_fp,"Best Managemnet Practices: not set\n");

    fprintf(status_fp,"\n\n");
    if (complete[11] == 1)
    {
        fprintf(status_fp,"The Project simulation has been run.\n");
        fprintf(status_fp,"\n");
    }
    if (complete[12] == 1)
    {
        fprintf(status_fp,"Sediment Deposition Map: %s in %s\n",
        deposit_layer, G_mapset());
    }
    if (complete[13] == 1)
    {
        fprintf(status_fp,"Sediment Loss Map: %s in %s\n",
        loss_layer, G_mapset());
    }
    if (complete[14] == 1)
    {
        fprintf(status_fp,"Channel Deposition Map: %s in %s\n",
        chnl_deposit_layer, G_mapset());
    }
    fprintf(status_fp,"\n\n");

    fclose(status_fp);
    
    printf("\nstatus file completed\n\n");

    hit_return();

    user_file(status_file);

    unlink(status_file);
    return(0);
}
