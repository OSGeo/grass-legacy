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

   function:  save_project
   called by: cancel_bmp, copy_project, main, menu
   
complimentary function is read_project. purpose of this function is to 
write a file to summarize and store the current status of a project,
and to record relevant information, primarily names of layers to
be used as inputs.   

this function is called often, so that if some error occurs and causes
and abort, previous work is saved */

#include "answers.h"

save_project() 
{
    int i;
    FILE *proj_fd;

/* open the file */

    proj_fd = G_fopen_new("answers/project", proj_name);
    if (!proj_fd)
    {
        croak(2, "Could not create project file");
    }

/* print warning file header */

    fprintf(proj_fd,"ANSWERS on GRASS Project Manager (v %s) Status File\n",
    version_num);
    fprintf(proj_fd,"This file is written and read by the Project Manager\n");
    fprintf(proj_fd,"programs to record the current status of a project\n");
    fprintf(proj_fd,"EDITING THIS FILE COULD HAVE UNPREDICTABLE EFFECTS\n\n");
    
    fprintf(proj_fd,"%s\n", G_whoami());

/* write number of cells and resolution */

  fprintf(proj_fd, "%d\n", cells_in_wshd);
  fprintf(proj_fd, "%.f\n", proj_resolution);

  /* write out the info on the mask layer and mapset */

    fprintf(proj_fd,"%d\n", complete[1]);
    if (complete[1] > 0) 
        fprintf(proj_fd,"mask:  %s in %s\n", mask_layer, mask_mapset);
      
  /* write out the info on the land cover layer and mapset */

    fprintf(proj_fd,"%d\n", complete[2]);
    if (complete[2] > 0) 
        fprintf(proj_fd,"soils:  %s in %s\n", soil_layer, soil_mapset);
    
  /* write out the info on the land cover layer and mapset */
  
    fprintf(proj_fd,"%d\n", complete[3]);
    if (complete[3] > 0) 
        fprintf(proj_fd,"cover:  %s in %s\n", cover_layer, cover_mapset);
    
   /* write out the info on elevation data */

    fprintf(proj_fd,"%d\n", complete[4]);
    if (complete[4] > 0) 
    {
        fprintf(proj_fd,"aspect:  %s in %s\n", aspect_layer, aspect_mapset);
        fprintf(proj_fd,"slope:  %s in %s\n", slope_layer, slope_mapset);
    }
    
  /* write out the info on the rain gauge layer and mapset */
  
    fprintf(proj_fd,"%d\n", complete[5]);
    if (complete[5] > 0) 
    {
        if (strncmp(rain_layer, "none", 4) == 0)
            fprintf(proj_fd,"rain:  %s in nowhere\n", rain_layer);
        else
            fprintf(proj_fd,"rain:  %s in %s\n", rain_layer, rain_mapset);
        if (strcmp(rain_event, "") == 0)
            fprintf(proj_fd,"event:  unknown\n");
        else
            fprintf(proj_fd,"event:  %s\n", rain_event);
    }

/* write out outlet cell's row and column number */

    fprintf(proj_fd,"%d\n", complete[6]);
    if (complete[6] > 0) 
    {
        fprintf(proj_fd,"outlet: row %d col %d\n", out_row, out_col);
    }
    
/* write out subsurface tile drainage. if all or none of the area
   has tile drainage, we "tile_area" will be "all" or "none"  otherwise
   if it is "layer" we will save the layer name and mapset */

    fprintf(proj_fd,"%d\n", complete[7]);
    if (complete[7] > 0) 
    {
        if(strcmp(tile_area, "layer") == 0)
            fprintf(proj_fd, "tile: %s in %s\n", tile_layer, tile_mapset);
        else
            fprintf(proj_fd, "tile: %s in watershed\n", tile_area);
    }
        
      /* write out the info on the channel elements layer and mapset */

    fprintf(proj_fd,"%d\n", complete[8]);
    if (complete[8] > 0) 
    {
        fprintf(proj_fd,"channel:  %s in %s\n", chnl_layer, chnl_mapset);
    }
      
/* write out the channel slope layer and mapset. since this step is
   optional, we have may have "none" for a layer name   */

    fprintf(proj_fd,"%d\n", complete[9]);
    if (complete[9] > 0)
    {
        if (strncmp(chnl_slp_layer, "none", 4) == 0)
            fprintf(proj_fd,"channel slope:  %s in watershed\n",
            chnl_slp_layer);
        else
            fprintf(proj_fd,"channel slope:  %s in %s\n", 
            chnl_slp_layer, chnl_slp_mapset);
    }

  /* write out the info on the BMP layers and mapset */

    fprintf(proj_fd,"%d\n", complete[10]);
    if (complete[10] > 0) 
    {
        for(i = 0; i < 4; i++)
        {
            if (strncmp(bmp_tbl[i].layer, "none", 4) == 0)
                fprintf(proj_fd,"%d:  %s in nowhere\n", i+1, bmp_tbl[i].layer);
            else
                fprintf(proj_fd,"%d:  %s in %s\n",
                i+1, bmp_tbl[i].layer, bmp_tbl[i].mapset);
        }
    }

/* record if the last step (create answers input) is complete */
  
    fprintf(proj_fd,"%d\n", complete[11]);
    
/* write out the deposition map */

    fprintf(proj_fd,"%d\n", complete[12]);
    if (complete[12] > 0) 
    {
        fprintf(proj_fd,"deposit:  %s\n", deposit_layer);
    }
      
/* write out the loss map */

    fprintf(proj_fd,"%d\n", complete[13]);
    if (complete[13] > 0) 
    {
        fprintf(proj_fd,"loss:  %s\n", loss_layer);
    }
      
/* write out the channel deposition map */

    fprintf(proj_fd,"%d\n", complete[14]);
    if (complete[14] > 0) 
    {
        fprintf(proj_fd,"channel deposition:  %s\n", chnl_deposit_layer);
    }
      
    fclose(proj_fd);
    return;
}
