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

   function: extract_bmp
   called by:  get_bmp

   create answers input from the bmp data we may have. since bmps
   are optional, we may have 0 to 4 bmps used. 2 of the bmps have
   additional info to add (widths of structure). all the bmps share
   a column in the answers input file. this can cause problems if
   a given cell element has more than one bmp. this process
   of extracting and distilling the bmp input is a twisted maize.

   this function is called as each bmp is set, thus we have some
   useful data still available in global variables. since all the 
   bmp info will go to one answers input column, we will save all
   the bmp's data in one file. there will be two columns of numbers
   in this file: bmp number (1-4) and width of structure (if bmp
   is 3 or 4). thus, if the user has more than one bmp, we will
   simultaneously read the current bmp's layer and the data file
   created for the previously run bmp(s). if a given cell has a
   value for another bmp, we alert user. 

   function returns 1 if unhappy; 0 if it missed the error (happy)
   */

#include "answers.h"

extract_bmp(num)

    int num;         /* number of bmp, corresponding to the array of
                        bmp_tbl[] structures. (declared in answers.h) */
{
    int map_fd;
    int mask_fd;
    int ct;
    int err;
    int i;
    int row, col;
    int datafile;
    int ask_me;
    int scan1;
    int scan2;
    int width;
    char line[100];
    FILE *data1_fp;
    FILE *data2_fp;
    CELL *map_cell;
    CELL *mask_cell;

    printf("\n\nExtracting data from <%s in %s>\n\n",
    bmp_tbl[num].layer, bmp_tbl[num].mapset);

/* open input layer */
    map_fd = G_open_cell_old(bmp_tbl[num].layer, bmp_tbl[num].mapset);
    if (  map_fd < 0 )
    {
        sprintf(line, "Could not open <%s in %s>",
        bmp_tbl[num].layer, bmp_tbl[num].mapset);
        croak(0, line);
        return(1);
    }

/* open project mask */
    mask_fd = G_open_cell_old(mask_layer, mask_mapset);
    if (  mask_fd < 0 )
    {
        sprintf(line, "Could not open <%s in %s>", mask_layer, mask_mapset);
        croak(0, line);
        return(1);
    }

/* check to see if we have previously created "in_bmp" for 
   ANSWERS input. if so, we will copy it to another file.
   datafile flag will indicate: 0 = file not previously created
   1 = file previously created   */

    if(G_find_file(data_dir, "in_bmp", G_mapset()) == NULL)
        datafile = 0;
    else
    {
        datafile = 1;
        G_rename(data_dir, "in_bmp", "in_bmp2");
        G_remove(data_dir, "in_bmp");
        data2_fp = G_fopen_old(data_dir, "in_bmp2", proj_mapset);
        if ( !data2_fp )
        {
            sprintf(line,"Could not open file <in_bmp2> in project database");
            croak(0, line);
            return(1);
        }
    }

    data1_fp = G_fopen_new(data_dir, "in_bmp");
    if ( !data1_fp )
    {
        sprintf(line, "Could not create file <in_bmp> in project database");
        croak(0, line);
        return(1);
    }
    
    if(datafile == 1)
    {
        ask_me = 0;
        printf("ANSWERS input data has been extracted for BMP data before.\n");
        printf("Therefore, as we extract data for the current BMP, we may\n");
        printf("encounter cells which currently have a value assigned for a\n");
        printf("BMP other than %s.\n", bmp_tbl[num].title);
        printf("By default, the previous BMP will be overwritten. If you\n");
        printf("wish, you can be given a choice if duplicate BMPs are found\n");
        printf("for a given cell.\n\n");
        if (G_yes("Do you wish to choose BMPs in the case of duplicates?", 0))
            ask_me = 1;
    }

    map_cell = G_allocate_cell_buf();
    mask_cell = G_allocate_cell_buf();

    ct = 1;
    scan1 = scan2 = 0;

/* here starts the shameless nested loop hell */

printf("\nworking...");
for( row = 0; row < window.rows; row++)
{
  G_get_map_row(map_fd, map_cell, row);
  G_get_map_row(mask_fd, mask_cell, row);

  for (col = 0; col < window.cols; col++)
  {
    /* only look at cells in mask */
    if(mask_cell[col] > 0)   
    {
      /* if datafile previously existed, scan in line for this element */
      if (datafile == 1)
      {
        err = fscanf(data2_fp, "%d   %d\n", &scan1, &scan2);
        if (err !=2)
        {
          sprintf(line, "Previously created bmp datafile is corrupted.\n");
          strcat(line, "Better start over.");
          cancel_bmp();
          croak(1, line);
        }
      }
      /* if we have a value in the bmp map */
      if (map_cell[col] > 0)
      {
        /* if we found a value for this element from another bmp, we ask user
           which bmp to use if the ask_me flag is set. */
        if ((scan1 != 0) && (scan1 != num+1) && (ask_me)
        && (ask_user((scan1 -1 ), num, ct) != 1))
        {
          /* put back the value we found */

          fprintf(data1_fp, "%2d %2d\n", scan1, scan2);
        }
        /* if field border or waterway bmp we have to lookup the width */
        else if ((num == 2) || (num == 3))
        {
          i = 1;
          while(1)
          {
            /* don't loop through more than number of cats in input layer */
            if(i > cat_tbl[0].cat)
            {
              fprintf(stderr, "\n\7WARNING: Can't extract value for %d\n", 
              map_cell[col]);
              hit_return();
              return(1);
            }
            /* found match. param[1] contains width */
            if (cat_tbl[i].cat == map_cell[col])
            {
              width = (int)cat_tbl[i].param[1];
              break;
            }
            /* continue lookup loop */
            i++;
          } 
          fprintf(data1_fp, "%2d %2d\n", num + 1, width);
        } 
        /* if bmp is tile outlet terrace or pond, print bmp number only */
        else if ((num == 0) || (num == 1)) 
        {
          fprintf(data1_fp, "%2d   0\n", num + 1);
        }
      }
      else   /* no bmp in current layer */
      {
      /* since no bmp in this later for this element, copy
         values from previous time (if not, scan1&2 are 0)  */

        fprintf(data1_fp, "%2d %2d\n", scan1, scan2);
      }
      /*  keep count of number of cells in mask */
      ct++;
    }
  }
}
   
    G_close_cell(mask_fd);
    G_close_cell(map_fd);
    fclose(data1_fp);
    if (datafile == 1)
    {
        fclose(data2_fp);
        G_remove(data_dir, "in_bmp2");
    }
    return(0);
}

ask_user(first, second, element)
    int first, second, element;
{
    char buf[80];
    printf("\n\n");
    printf("Watershed element: %d\n", element);
    printf("This watershed element currently contains a %s\n",
    bmp_tbl[first].title);
    printf("The <%s> layer shows this element also contains a\n",
    bmp_tbl[second].layer);
    printf("%s\n", bmp_tbl[second].title);
    printf("\n");
    printf("Since the element can have only one BMP,\n");
    sprintf(buf,"do you wish to set the element's BMP to %s?",
    bmp_tbl[second].title);
    return(G_yes(buf, 1));
}
