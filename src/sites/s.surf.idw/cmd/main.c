#include "gis.h"
#include "site.h"

int search_points = 12;

int npoints = 0;
int **npoints_currcell;
int nsearch;

struct Point
{
    double north, east;
    double z;
    double dist;
};
struct Point ***points;
struct Point *list;
static struct Cell_head window;

int main(int argc, char *argv[])
{
    int fd, maskfd;
    CELL  *mask;
    DCELL *dcell;
    struct GModule *module;
    int row, col;
    int startrow, endrow, searchrow;
    int startcolumn, endcolumn, searchcolumn;
    int pointsfound, edgeregion;
    double north, east;
    double dx,dy;
    double maxdist,dist;
    double sum1, sum2, interp_value;
    int i,n,max, field;
    void read_sites();
    struct
    {
        struct Option *input, *npoints, *output, *dfield;
    } parm;

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Name of input sites map" ;
    parm.input->gisprompt  = "old,site_lists,sites" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output" ;
    parm.output->type       = TYPE_STRING ;
    parm.output->required   = YES;
    parm.output->description= "Name of output raster map" ;
    parm.output->gisprompt  = "any,cell,raster" ;

    parm.npoints = G_define_option() ;
    parm.npoints->key        = "npoints" ;
    parm.npoints->key_desc   = "count" ;
    parm.npoints->type       = TYPE_INTEGER ;
    parm.npoints->required   = NO ;
    parm.npoints->description="Number of interpolation points";
    parm.npoints->answer = "12";

    parm.dfield = G_define_option ();
    parm.dfield->key = "field";
    parm.dfield->type = TYPE_INTEGER;
    parm.dfield->answer = "1";
    parm.dfield->multiple = NO;
    parm.dfield->required = NO;
    parm.dfield->description = "which decimal attribute (if multiple)";

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =        
                    "Surface interpolation from sites data by Inverse "
                    "Distance Squared Weighting.";
                    
    if (G_parser(argc, argv))
        exit(1);

    if (G_legal_filename(parm.output->answer) < 0)
    {
        fprintf (stderr, "%s=%s - illegal name\n", parm.output->key, parm.output->answer);
        exit(1);
    }


    if(sscanf(parm.npoints->answer,"%d", &search_points) != 1 || search_points<1)
    {
        fprintf (stderr, "%s=%s - illegal number of interpolation points\n", 
                parm.npoints->key, parm.npoints->answer);
        G_usage();
        exit(1);
    }

    sscanf(parm.dfield->answer,"%d", &field);
    if (field < 1)
      G_fatal_error ("Decimal attribute field 0 doesn't exist.");    

    list = (struct Point *) G_calloc (search_points, sizeof (struct Point));


/* get the window, dimension row/column arrays */
    G_get_window (&window);
   
    npoints_currcell = (int **)G_malloc(window.rows * sizeof(int));
    points = (struct Point ***)G_malloc(window.rows * sizeof(struct Point));

    for(row = 0; row < window.rows; row++)
    {
        npoints_currcell[row] = (int *)G_malloc(window.cols * sizeof(int));
        points[row] = (struct Point **)G_malloc(window.cols * sizeof(struct Point));
       
        for(col = 0; col < window.cols; col++)
        {
            npoints_currcell[row][col] = 0;
            points[row][col] = NULL; 
        }
    }

/* read the elevation points from the input sites file */
    read_sites (parm.input->answer, field);

    if (npoints == 0)
    {
        fprintf (stderr, "%s: no data points found\n", G_program_name());
        exit(1);
    }
    nsearch = npoints < search_points ? npoints : search_points;

/* allocate buffers, etc. */
   
    dcell=G_allocate_d_raster_buf();
    
    if ((maskfd = G_maskfd()) >= 0)
        mask = G_allocate_cell_buf();
    else
        mask = NULL;


    fd=G_open_raster_new(parm.output->answer, DCELL_TYPE);
    if (fd < 0)
    {
        fprintf (stderr, "%s: can't create %s\n", G_program_name(), parm.output->answer);
        exit(1);
    }

                                  
    fprintf (stderr, "Interpolating raster map <%s> ... %d rows ... ",
        parm.output->answer, window.rows);

    north = window.north + window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
        fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", window.rows-row);

        if (mask)
        {
            if(G_get_map_row(maskfd, mask, row) < 0)
                exit(1);
        }
        north -= window.ns_res;
        east = window.west - window.ew_res/2.0;
        for (col = 0; col < window.cols; col++)
        {
            east += window.ew_res;
                /* don't interpolate outside of the mask */
            if (mask && mask[col] == 0)
            {
                G_set_d_null_value(&dcell[col], 1);
                continue;
            }
           
            /* If current cell contains more than nsearch points just average
             * all the points in this cell and don't look in any others */
           
            if ((pointsfound = npoints_currcell[row][col]) >= nsearch)
            {
                sum1 = 0.0;
                for (i = 0; i < pointsfound; i++)                
                    sum1 += points[row][col][i].z;
               
                interp_value = sum1/pointsfound;
            }
            else
            {
                startrow = endrow = row;
                startcolumn = endcolumn = col;
               
                while(pointsfound < nsearch)
                {
                   
                   /* Keep widening the search window in which we are looking
                    * for the 'nsearch' nearest points, until we find them */
                   edgeregion = 0;
                   if (startrow>0) startrow--; else edgeregion=1;
                   if (startcolumn>0) startcolumn--; else edgeregion=1;
                   if (endrow<(window.rows-1)) endrow++; else edgeregion=1;
                   if (endcolumn<(window.cols-1)) endcolumn++; else edgeregion=1;
                   
                   if (edgeregion == 1)
                   {
                       /* If we've hit the edge of the region, easier to 
                        * scan through all the cells in the search window */
                       pointsfound = 0;
                       for (searchrow = startrow; searchrow <= endrow; searchrow++)
                       {
                           for (searchcolumn = startcolumn; searchcolumn <= endcolumn; searchcolumn++)
                               pointsfound += npoints_currcell[searchrow][searchcolumn];
                       }
                   }
                   else
                   {
                       /* else can just add on the new points in the outermost
                        * rows and columns */
                       for (searchrow = startrow; searchrow <= endrow; searchrow++)                       
                           pointsfound = pointsfound 
                             + npoints_currcell[searchrow][startcolumn] 
                             + npoints_currcell[searchrow][endcolumn];
                       
                       for (searchcolumn = (startcolumn+1); searchcolumn <= (endcolumn-1); searchcolumn++)
                           pointsfound = pointsfound
                             + npoints_currcell[startrow][searchcolumn]
                             + npoints_currcell[endrow][searchcolumn];                       
                   }
                }
               
                /* Check distances and find the points to use in interpolation */
                i = 0;
                for (searchrow = startrow; searchrow <= endrow; searchrow++)
                {
                    for (searchcolumn = startcolumn; searchcolumn <= endcolumn; searchcolumn++)
                    {
                        for (pointsfound = 0; pointsfound < npoints_currcell[searchrow][searchcolumn]; pointsfound ++)
                        {
                                   /* fill list with first nsearch points */
                            if (i < nsearch)
                            {
                                dy = points[searchrow][searchcolumn][pointsfound].north - north;
                                dx = points[searchrow][searchcolumn][pointsfound].east  - east;
                                list[i].dist = dy*dy + dx*dx;
                                list[i].z = points[searchrow][searchcolumn][pointsfound].z;
                                i++;

                                /* find the maximum distance */
                                if (i == nsearch)
                                {
                                    maxdist = list[max=0].dist;
                                    for (n = 1; n < nsearch; n++)
                                    {
                                        if (maxdist < list[n].dist)
                                            maxdist = list[max=n].dist;
                                    }
                                }
                            }
                            else
                            {
                           
                                /* go thru rest of the points now */
                                dy = points[searchrow][searchcolumn][pointsfound].north - north;
                                dx = points[searchrow][searchcolumn][pointsfound].east  - east;
                                dist = dy*dy + dx*dx;

                                if (dist < maxdist)
                                {
                                    /* replace the largest dist */
                                    list[max].z = points[searchrow][searchcolumn][pointsfound].z;
                                    list[max].dist = dist;
                                    maxdist = list[max=0].dist;
                                    for (n = 1; n < nsearch; n++)
                                    {
                                        if (maxdist < list[n].dist)
                                            maxdist = list[max=n].dist;
                                    }
                                }
                            
                            }
                        }
                    }
                }
               
                /* interpolate */
                sum1 = 0.0;
                sum2 = 0.0;
                for (n = 0; n < nsearch; n++)
                {
                    if(dist = list[n].dist)
                    {
                        sum1 += list[n].z / dist;
                        sum2 += 1.0/dist;
                    }
                    else
                    {
                        /* If one site is dead on the centre of the cell, ignore
                         * all the other sites and just use this value. */
                        sum1 = list[n].z;
                        sum2 = 1.0;
                        break;
                    }
                }
                interp_value = sum1/sum2;
            }
            dcell[col] = (DCELL) interp_value;
        }
        G_put_d_raster_row(fd,dcell);
    }
    G_close_cell(fd);
    fprintf (stderr, "done          \n");
    exit(0);
}

void newpoint ( double z,double east,double north)
{
    int row, column;

    row   = (int)((window.north - north) / window.ns_res);
    column = (int)((east - window.west) / window.ew_res);
 
    if (row<0 || row>=window.rows || column<0 || column>=window.cols)
        ;
    else /* For now ignore sites outside current region */
    {
        points[row][column] = (struct Point *) G_realloc (points[row][column],
                  (1 + npoints_currcell[row][column]) * sizeof (struct Point));
        points[row][column][npoints_currcell[row][column]].north = north;
        points[row][column][npoints_currcell[row][column]].east  = east;
        points[row][column][npoints_currcell[row][column]].z     = z;
        npoints_currcell[row][column]++;
        npoints++;
    }
}
