#include "gis.h"

/*
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. All Rights Reserved. Unless |
    |   permission is granted, this material shall not be     |
    |   copied, reproduced or coded for reproduction by any   |
    |   electrical, mechanical or chemical processes,  or     |
    |   combinations thereof, now known or later developed.   |
    +---------------------------------------------------------+
*/

int out_row, out_col;
struct Cell_head proj_head;
struct Cell_head window;
char version_num[10];
char step[15][90];
char proj_name[41];
char proj_creator[20];
char soil_layer[61];
char cover_layer[61];
char mask_layer[61];
char elevation_layer[61];
char aspect_layer[61];
char slope_layer[61];
char rain_layer[61];
char rain_event[61];
char tile_layer[61];
char chnl_layer[61];
char chnl_slp_layer[61];
char deposit_layer[61];
char chnl_deposit_layer[61];
char loss_layer[61];
char *proj_mapset;
char *soil_mapset;
char *cover_mapset;
char *mask_mapset;
char *elevation_mapset;
char *aspect_mapset;
char *slope_mapset;
char *rain_mapset;
char *tile_mapset;
char *chnl_mapset;
char *chnl_slp_mapset;
char tile_area[6];
double easting, northing;
int complete[17];
double proj_resolution;
int cells_in_wshd, rows_in_wshd, cols_in_wshd;
char data_dir[100];
struct cat_table
    {
    long cat;
    float param[9];
    char *label;
    };
struct  cat_table cat_tbl[100];

struct bmp
{
    int set;
    char title[20];
    char layer[61];
    char *mapset;
};

struct bmp bmp_tbl[4];

FILE *out_fp;

