/**
****************************************************************************
*
* MODULE:       s.mask
* 
* AUTHOR(S):    Eric G. Miller <egm2@jps.net>
* 
* PURPOSE:      Filter a sites lists via a raster mask.  That is, the site
*               must fall within the region, and it must fall within a
*               non-NULL cell.  The region is the current region by default,
*               but may use the raster's region via a flag.
*               
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gis.h"
#include "site.h"

int
main (int argc, char *argv[])
{
    struct GModule *module;
    struct Flag    *rflag;
    struct Option  *input, *output, *raster;
    char *mapset_in, *mapset_cur, *mapset_rast;
    char *errmsg;
    char buff[512];
    struct Cell_head window;
    FILE *ifpsites, *ofpsites;
    int  rastfd;
    int row, col;
    RASTER_MAP_TYPE maptype, sitetype;
    void *rastbuf;
    void *vptr;
    size_t cellsize;
    Site *theSite;
    int ndims, ndbls, nstrs;
    Site_head theHead;
    struct TimeStamp ts;
    char   timestr[512] = "";
    time_t ticks;
    struct tm *theTime;
    int stotal = 0, skept = 0; 
    int error;
    
    G_gisinit(argv[0]);

    module                  = G_define_module();
    module->description     = 
        "Filter sites using a raster mask";

    rflag                   = G_define_flag();
    rflag->key              = 'r';
    rflag->description      = "Use raster's region vs. current region";

    input                   = G_define_option();
    input->key              = "input";
    input->description      = "Name of sites file to filter";
    input->required         = YES;
    input->type             = TYPE_STRING;
    input->gisprompt        = "old,site_lists,sites";

    output                  = G_define_option();
    output->key             = "output";
    output->description     = "Name of resulting sites file";
    output->required        = YES;
    output->type            = TYPE_STRING;
    output->gisprompt       = "new,site_lists,sites";

    raster                  = G_define_option();
    raster->key             = "raster";
    raster->description     = "Name of raster for mask";
    raster->required        = YES;
    raster->type            = TYPE_STRING;
    raster->gisprompt       = "old,cell,raster";

    if (G_parser(argc,argv))
        exit (EXIT_FAILURE);
    
    mapset_in   = G_find_file ("site_lists", input->answer, "");
    if (!mapset_in)
        G_fatal_error ("sites %s not found", input->answer);
    
    if (G_legal_filename (output->answer) != 1)
        G_fatal_error ("bad output name %s", output->answer);
    
    mapset_cur  = G_mapset();
    if (G_find_file ("site_lists", output->answer, mapset_cur))
        G_fatal_error ("output sites %s exists", output->answer);
    
    mapset_rast = G_find_cell (raster->answer, "");
    if (!mapset_rast)
        G_fatal_error ("raster %s not found", raster->answer);

    ifpsites = G_sites_open_old (input->answer, mapset_in);
    if (!ifpsites)
        G_fatal_error ("opening %s", input->answer);

    ofpsites = G_sites_open_new (output->answer);
    if (!ofpsites)
        G_fatal_error ("opening %s", output->answer);

    if (rflag->answer)
    {
        if (G_get_cellhd (raster->answer, mapset_rast, &window) != 0)
            G_fatal_error ("getting raster region");
        errmsg = G_adjust_Cell_head (&window, YES, YES);
        if (errmsg)
            G_fatal_error ("%s", errmsg);
    }
    else
    {
        G_get_window (&window);
    }

    G_set_window (&window);

    if ((rastfd = G_open_cell_old (raster->answer, mapset_rast)) < 0)
        G_fatal_error ("opening raster %s", raster->answer);

    maptype = G_raster_map_type (raster->answer, mapset_rast);
    rastbuf = G_allocate_raster_buf (maptype);
    cellsize = G_raster_size (maptype);
    
    switch (G_site_describe(ifpsites, &ndims, &sitetype, &nstrs, &ndbls))
    {
        case 0:
            break;
        case 1:
            G_fatal_error ("premature EOF on input sites");
            break;
        case 2:
            G_fatal_error ("couldn't get input sites format");
            break;
        default:
            G_fatal_error ("G_site_describe() returned unknown error code");
            break;
    }

    theSite = G_site_new_struct (sitetype, ndims, nstrs, ndbls);
    if (!theSite)
        G_fatal_error ("allocating sites structure");

    G_site_get_head(ifpsites, &theHead);
    theHead.name = output->answer;
    sprintf (buff, "s.mask %s input=%s output=%s raster=%s",
            (rflag->answer) ? "-r" : "",
            input->answer,
            output->answer,
            raster->answer);
    theHead.desc = buff;
    if (!theHead.time)
    {
        G_init_timestamp (&ts);
        ticks = time (NULL);
        theTime = localtime(&ticks);
        if(!strftime (timestr, 512, "%d %b %Y %H:%M:%S %z", theTime))
            timestr[0] = '\0';
        fflush(stderr);
        error = datetime_scan (&ts.dt[0], timestr);
        ts.count = 1;
        theHead.time = &ts;
    }
    G_site_put_head (ofpsites, &theHead);
    
    /* process loop */
    while ((error = G_site_get (ifpsites, theSite)) == 0)
    {
        stotal++;
        
        if (G_site_in_region (theSite, &window))
        {
            row = (int) G_northing_to_row (theSite->north, &window);
            col = (int) G_easting_to_col (theSite->east, &window);
            G_get_raster_row (rastfd, rastbuf, row, maptype);
            vptr = G_incr_void_ptr (rastbuf, col * cellsize);
            if (!G_is_null_value (vptr, maptype))
            {
                skept++;
                G_site_put (ofpsites, theSite);
            }
        }
    }
    
    if (error != -1)
    {
        if (error == 1)
            G_warning ("format mismatch found in input sites, "\
                       "output may be incomplete");
        else
            G_warning ("severe error reading input sites, "\
                       "output may be incomplete");
    }
    
    fprintf (stdout, "Total: %d, Output: %d\n", stotal, skept);

    return 0;
}
