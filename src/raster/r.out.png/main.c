/* 
 * based on r.out.ppm by
 * Written by Bill Brown, USA-CERL March 21, 1994
 * 
*/

/* Use to convert grass raster file to PNG
 * uses currently selected region
*/

#include <string.h>
#include <stdlib.h>

#include "gis.h"

#define DEF_RED 255
#define DEF_GRN 255
#define DEF_BLU 255

typedef int FILEDESC;

int main( int argc, char *argv[])
{
    struct Option       *rast, *png_file;
    struct Flag         *bequiet, *gscale;
    char                *cellmap, *map, *p, errbuf[100], ofile[1000];
    unsigned char       *set, *ored, *ogrn, *oblu;
    CELL                *cell_buf;
    FCELL               *fcell_buf;
    DCELL               *dcell_buf;
    void                *voidc;
    int                 rtype, row, col, do_stdout = 0;
    struct Cell_head    w;
    FILEDESC            cellfile = 0;
    FILE                *fp;


    G_gisinit (argv[0]);

    rast = G_define_option();
    rast->key                    = "input";
    rast->type                   = TYPE_STRING;
    rast->required               = YES;
    rast->multiple               = NO;
    rast->gisprompt              = "old,cell,Raster";
    rast->description            = "Raster file to be converted.";

    png_file = G_define_option();
    png_file->key                    = "output";
    png_file->type                   = TYPE_STRING;
    png_file->required               = NO;
    png_file->multiple               = NO;
    png_file->answer                 = "<rasterfilename>.png";
    png_file->description            
                    = "Name for new PNG file. (use out=- for stdout)";

    bequiet = G_define_flag ();
    bequiet->key = 'q';
    bequiet->description = "Run quietly";

    gscale = G_define_flag ();
    gscale->key = 'G';
    gscale->description = "Output greyscale instead of color";

    if (G_parser (argc, argv))
        exit (-1);

    /* kludge to work with r.out.mpeg */
    if(rast->answer[0] == '/')
        rast->answer++;
    
    if(strcmp(png_file->answer,"<rasterfilename>.png")){
        if(strcmp(png_file->answer,"-"))
            strcpy (ofile, png_file->answer);
        else
            do_stdout = 1;
    }
    else{
        map = p = rast->answer;
        /* knock off any GRASS location suffix */
        if ((char*)NULL != (p = strrchr (map, '@'))) {
                if (p != map)
                *p = '\0';
        }
        strcpy (ofile, map);
        strcat(ofile,".png");
    }

    /*G_get_set_window (&w); */ /* 10/99 MN: check for current region*/
    G_get_window (&w);

    if(!bequiet->answer)
        fprintf(stderr,"rows = %d, cols = %d\n", w.rows, w.cols);

    /* open cell file for reading */
    {  
        cellmap = G_find_file2 ("cell", rast->answer, "");
        if(!cellmap){
            sprintf(errbuf,"Couldn't find raster file %s", rast->answer);
            G_fatal_error(errbuf);
        }

        if ((cellfile = G_open_cell_old(rast->answer, cellmap)) == -1) 
        {
            sprintf(errbuf,"Not able to open cellfile for [%s]", rast->answer);
            G_fatal_error(errbuf);
        }
    }

    cell_buf = G_allocate_c_raster_buf();
    fcell_buf = G_allocate_f_raster_buf();
    dcell_buf = G_allocate_d_raster_buf();

    ored = (unsigned char *)G_malloc (w.cols * sizeof (unsigned char));
    ogrn = (unsigned char *)G_malloc (w.cols * sizeof (unsigned char));
    oblu = (unsigned char *)G_malloc (w.cols * sizeof (unsigned char));
    set = (unsigned char *)G_malloc (w.cols * sizeof (unsigned char));

    /* open png file for writing */
    {
        if(do_stdout) fp = stdout;
        else if(NULL == (fp = fopen(ofile, "w"))) {
            sprintf(errbuf,"Not able to open file for [%s]", ofile);
            G_fatal_error(errbuf);
        }
    }
    /* write header info */

    if(!gscale->answer)
        fprintf(fp,"P6\n");
        /* Magic number meaning rawbits, 24bit color to png format */
    else 
        fprintf(fp,"P5\n");
        /* Magic number meaning rawbits, 8bit greyscale to png format */

    if(!do_stdout){
        fprintf(fp,"# CREATOR: %s from GRASS raster file \"%s\"\n", 
                argv[0], rast->answer);
        fprintf(fp,"# east-west resolution: %f\n", w.ew_res);
        fprintf(fp,"# north-south resolution: %f\n", w.ns_res);
        fprintf(fp,"# South edge: %f\n", w.south);
        fprintf(fp,"# West edge: %f\n", w.west);
    /* comments */
    }

    fprintf(fp,"%d %d\n",w.cols,w.rows); 
    /* width & height */

    fprintf(fp,"255\n");
    /* max intensity val */


    if(!bequiet->answer)
        fprintf(stderr,"Converting %s...",rast->answer);

    {   
    struct Colors colors;

    G_read_colors (rast->answer, cellmap, &colors);

    rtype = G_raster_map_type(rast->answer, cellmap);
    if (rtype == CELL_TYPE)
        voidc = (CELL *)cell_buf;
    else if (rtype == FCELL_TYPE)
        voidc = (FCELL *)fcell_buf;
    else if (rtype == DCELL_TYPE)
        voidc = (DCELL *)dcell_buf;
    else
        exit(1);

    if(!gscale->answer){        /* 24BIT COLOR IMAGE */
        for (row = 0; row < w.rows; row++) {
            if(!bequiet->answer)
                G_percent (row, w.rows, 5);
            if (G_get_raster_row (cellfile, (void *)voidc, row, rtype) < 0)
                exit(1);
            G_lookup_raster_colors((void *)voidc, ored, ogrn, oblu, set, 
                                    w.cols, &colors, rtype);

            for(col=0; col < w.cols; col++){
                if(set[col]){
                    putc(ored[col],fp);
                    putc(ogrn[col],fp);
                    putc(oblu[col],fp);
                }
                else{
                    putc(DEF_RED,fp);
                    putc(DEF_GRN,fp);
                    putc(DEF_BLU,fp);
                }
            }
        }
    }
    else{                       /* GREYSCALE IMAGE */
        for (row = 0; row < w.rows; row++) {

            if(!bequiet->answer)
                G_percent (row, w.rows, 5);
            if (G_get_raster_row (cellfile, (void *)voidc, row, rtype) < 0)
                exit(1);
            G_lookup_raster_colors((void *)voidc, ored, ogrn, oblu, set, 
                                    w.cols, &colors, rtype);

            for(col=0; col < w.cols; col++){

#ifdef XV_STYLE
                    /*.33R+ .5G+ .17B*/
                    putc((((ored[col])*11+(ogrn[col])*16
                        +(oblu[col])*5) >> 5), fp);
#else
                    /*NTSC Y equation: .30R+ .59G+ .11B*/
                    putc((((ored[col])*19+(ogrn[col])*38
                        +(oblu[col])*7) >> 6), fp);
#endif
            }
        }
    }

    G_free_colors(&colors);

    }
    G_free(cell_buf);
    G_free(fcell_buf);
    G_free(dcell_buf);
    G_free(ored);
    G_free(ogrn);
    G_free(oblu);
    G_free(set);
    G_close_cell(cellfile);
/*
    if(!do_stdout)
*/
        fclose(fp);

    if(!bequiet->answer)
        fprintf(stderr,"\nDone.\n"); 
    
    return(1);
}

