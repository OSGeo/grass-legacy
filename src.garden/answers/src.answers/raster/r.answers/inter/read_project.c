
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

   function:  read_project
   called by: main, get_mask, and copy_project 

   complimentary function is save_project. read_project reads the
   file in the G_mapset()/answers/project directory with the
   name of the project, which save_project previously has written.

   */

#include "answers.h"

read_project()

{
    char buf[200];
    char num_buf1[20];
    char num_buf2[20];
    char readbuf[1024];
    char checkbuf[5];
    char mapset[30];
    int verbose = 0;
    int i;
    FILE *proj_fd;


    proj_fd = G_fopen_old("answers/project", proj_name, G_mapset());
    if (!proj_fd) {
       fprintf(stderr, "\n\7WARNING: Could not open <%s> project file", proj_name);
       return(1);
    }

/* set the undescriptive error message  */

    sprintf(buf,"Error reading project information file -- corrupted format\n");

/* read the first four lines (the warning message that tells users not
   to edit the file */
   
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);
    fgets(readbuf,1024,proj_fd);
    
    if (fscanf(proj_fd, "%s\n", proj_creator) != 1)
    {
        croak(2, buf);
    }
    if (fscanf(proj_fd, "%d\n", &cells_in_wshd) != 1)
    {
        croak(2, buf);
    }
    if (fscanf(proj_fd, "%d\n", &i) != 1)
    {
        croak(2, buf);
    }
    proj_resolution = (float) i;
    
    if (verbose)
    {
    fprintf(stderr,"Reading current project status for <%s in %s>\n\n",
    proj_name, G_mapset());
    }

    if (verbose)
    fprintf(stderr,"Resolution: %.0f meters\n", proj_resolution); 

  /* get information about the project mask (step 1)  */
  
    if (fscanf(proj_fd, "%d\n", &complete[1]) != 1)
    {
        croak(2, buf);
    }
    if (complete[1] > 0) 
    {
        if (fscanf(proj_fd,"mask:  %s in %s", mask_layer, mapset) == 2)
        {
            mask_mapset = G_malloc(strlen(mapset)+1);
            strcpy(mask_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Mask: %s in %s\n",mask_layer, mask_mapset); 
        }
        else
           croak(2, buf);
    }

/* get information about the soils layer (step 2) */

    if (fscanf(proj_fd, "%d\n", &complete[2]) != 1)
    {
        croak(2, buf);
    }
    if (complete[2] > 0)
    {
        if (fscanf(proj_fd,"soils:  %s in %s\n", soil_layer, mapset) == 2)
        {
            soil_mapset = G_malloc(strlen(mapset)+1);
            strcpy(soil_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Soils: %s in %s\n",soil_layer, soil_mapset); 
        }
        else
            croak(2,buf);
    }

  /* read info about land cover layer and mapset or G_malloc space */

    if (fscanf(proj_fd, "%d\n", &complete[3]) != 1)
    {
        croak(2, buf);
    }
    if (complete[3] > 0)
    {
        if (fscanf(proj_fd,"cover:  %s in %s\n", cover_layer, mapset) == 2)
        {
            cover_mapset = G_malloc(strlen(mapset)+1);
            strcpy(cover_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Cover: %s in %s\n", cover_layer, cover_mapset); 
        }
        else
            croak(2,buf);
    }

/* get information about the elevation-based layers (step 4) */

    if (fscanf(proj_fd, "%d\n", &complete[4]) != 1)
    {
        croak(2, buf);
    }
    if (complete[4] > 0)
    {
/* read the line that contains the word "aspect: " */

        if (fscanf(proj_fd,"aspect:  %s in %s\n", aspect_layer, mapset) == 2)
        {
            aspect_mapset = G_malloc(strlen(mapset)+1);
            strcpy(aspect_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Aspect: %s in %s\n",
                aspect_layer, aspect_mapset); 
        }
        else
            croak(2, buf);

/* read the line that contains the word "slope: " */

        if (fscanf(proj_fd,"slope:  %s in %s\n", slope_layer, mapset) == 2)
        {
            slope_mapset = G_malloc(strlen(mapset)+1);
            strcpy(slope_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Slope: %s in %s\n",
                slope_layer, slope_mapset); 
        }
        else
            croak(2, buf);
    }

  /* read info about rain gauge layer and mapset or G_malloc space */

    if (fscanf(proj_fd, "%d\n", &complete[5]) != 1)
    {
        croak(2, buf);
    }
    if (complete[5] > 0)
    {
        if (fscanf(proj_fd,"rain:  %s in %s\n", rain_layer, mapset) == 2)
        {
            if (strncmp (rain_layer, "none", 4) == 0)
            {
                if (verbose)
                    printf("Rain: none\n");
            }
            else
            {
                rain_mapset = G_malloc(strlen(mapset)+1);
                strcpy(rain_mapset, mapset);
                if (verbose)
                    fprintf(stderr,"Rain: %s in %s\n",
                    rain_layer, rain_mapset); 
            }
       }
       else
           croak(2,buf);
        if (fscanf(proj_fd,"event:  %s\n", rain_event) != 1)
            strcpy(rain_event, "unknown");
    }

  /* get information about the outlet (step 6)  */

    if (fscanf(proj_fd, "%d\n", &complete[6]) != 1)
    {
        croak(2, buf);
    }

    if(complete[6] > 0)
    {
        if(fscanf(proj_fd,"outlet: row %s col %s\n", num_buf1, num_buf2) == 2)
        {
            out_row = atoi(num_buf1);
            out_col = atoi(num_buf2);
            if (verbose)
                fprintf(stderr,"Outlet at row: %d col %d\n", out_row, out_col); 
        }
        else
            croak(2,buf);
    }

  /* get information about the tile drainage areas (step 7)  */

    if (fscanf(proj_fd, "%d\n", &complete[7]) != 1)
    {
        croak(2, buf);
    }

    if (complete[7] > 0)
    {
        if (fscanf(proj_fd,"tile: %s in %s\n", tile_area, mapset) == 2)
/* if tile_area is layer, we get the name and mapset */
        {
            if ((strcmp(tile_area, "none") != 0) && 
            (strcmp(tile_area, "all") != 0))
            {
                tile_mapset = G_malloc(strlen(mapset)+1);
                strcpy(tile_mapset, mapset);
                if (verbose)
                   fprintf(stderr,"Tile: %s in %s\n",
                   tile_layer, tile_mapset); 
            }
            else
            {
                if (verbose)
                    fprintf(stderr,"%s of watershed has tile drainage\n",
                    tile_area); 
            }
        }
        else
            croak(2,buf);
    }

/* read info about channel layer and mapset or G_malloc space (step 8) */

    if (fscanf(proj_fd, "%d\n", &complete[8]) != 1)
    {
        croak(2, buf);
    }

    if(complete[8] > 0)
    {
        if (fscanf(proj_fd,"channel:  %s in %s\n", chnl_layer, mapset) == 2)
        {
            chnl_mapset = G_malloc(strlen(mapset)+1);
            strcpy(chnl_mapset, mapset);
            if (verbose)
                fprintf(stderr,"Channel: %s in %s\n", chnl_layer, chnl_mapset); 
        }
        else
            croak(2,buf);
    }

  /* read info about channel slope layer and mapset or G_malloc space */

    if (fscanf(proj_fd, "%d\n", &complete[9]) != 1)
    {
        croak(2, buf);
    }

    if(complete[9] > 0)
    {
        if(fscanf(proj_fd,"channel slope: %s in %s\n",chnl_slp_layer,mapset)==2)
        {
            if (strncmp (chnl_slp_layer, "none", 4) == 0)
            {
                if (verbose)
                    printf("Channel slope: none\n");
            }
            else 
            {
                chnl_slp_mapset = G_malloc(strlen(mapset)+1);
                strcpy(chnl_slp_mapset, mapset);
                if (verbose)
                    fprintf(stderr,"Channel slope: %s in %s\n", 
                    chnl_slp_layer, chnl_slp_mapset); 
            }
        }
        else
            croak(2,buf);
    }

  /* read info about BMPs (step 10) and mapset or G_malloc space */

    if (fscanf(proj_fd, "%d\n", &complete[10]) != 1)
    {
        croak(2, buf);
    }

    if(complete[10] > 0)
    {
        if(verbose)
        fprintf(stderr, "Best Managment Practices\n");

    for(i = 0; i < 4; i++)
    {
        if (fgets(readbuf,1024,proj_fd) == NULL)
        {
            croak(2, buf);
            return(0);
        }
        sprintf(checkbuf, "%d:", i+1);
        if (strncmp(readbuf, checkbuf, 2) != 0)
        {
            croak(2, buf);
            return(0);
        }
        if((sscanf(readbuf,"%s  %s in %s",checkbuf,bmp_tbl[i].layer,mapset)==3) 
        && (strncmp (bmp_tbl[i].layer, "none", 4) == 0))
        {
          if (verbose) printf(" - %s: none\n", bmp_tbl[i].title);
          bmp_tbl[i].set = 0;
        }
        else 
        {
            if(sscanf(readbuf,"%s  %s in %s",checkbuf,bmp_tbl[i].layer,mapset)!=3)
            {
                bmp_tbl[i].set = 0;
            }
            else
            {
                bmp_tbl[i].set = 1;
                bmp_tbl[i].mapset = G_malloc(strlen(mapset)+1);
                strcpy(bmp_tbl[i].mapset, mapset);
                if (verbose)
                    fprintf(stderr," - %s: %s in %s\n", 
                    bmp_tbl[i].title, bmp_tbl[i].layer, bmp_tbl[i].mapset); 
            }
        }
    }
    }

/* see if answers input has been created (step 11) */

    if (fscanf(proj_fd, "%d\n", &complete[11]) != 1)
    {
        croak(2, buf);
    }

/* get information about the deposition layer (step 12) */

    if (fscanf(proj_fd, "%d\n", &complete[12]) != 1)
    {
        croak(2, buf);
    }
    if (complete[12] > 0)
    {
        if(fscanf(proj_fd,"deposit:  %s\n", deposit_layer) == 1)
        {
            if (verbose)
                fprintf(stderr,"Deposit: %s in %s\n", deposit_layer, G_mapset); 
        }
        else
            croak(2,buf);
    }

/* get information about the loss layer (step 13) */

    if (fscanf(proj_fd, "%d\n", &complete[13]) != 1)
    {
        croak(2, buf);
    }
    if (complete[13] > 0)
    {
        if(fscanf(proj_fd,"loss:  %s\n", loss_layer) == 1)
        {
            if (verbose)
                fprintf(stderr,"Loss: %s in %s\n", loss_layer, G_mapset); 
        }
        else
            croak(2,buf);
    }

/* get information about the channel deposition layer (step 14) */

    fscanf(proj_fd, "%d\n", &complete[14]);
/*
    if (fscanf(proj_fd, "%d\n", &complete[14]) != 1)
    {
        croak(2, buf);
    }
*/
    if (complete[14] > 0)
    {
        if(fscanf(proj_fd,"channel deposition:  %s\n", chnl_deposit_layer) == 1)
        {
            if (verbose)
                fprintf(stderr,"Channel deposition: %s in %s\n",
                chnl_deposit_layer, G_mapset); 
        }
/*
        else
            croak(2,buf);
*/
    }
    if(verbose)
    {
       printf("Answers input step completed\n");
       printf("\n\n");
       hit_return(); 
    }

    fclose(proj_fd);
    return(0);

}
