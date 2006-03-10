/*
**  Original Algorithm:    H. Mitasova, L. Mitas, J. Hofierka, M. Zlocha 
**  GRASS Implementation:  J. Caplan, M. Ruesink  1995
**
**  US Army Construction Engineering Research Lab, University of Illinois 
**
**  Copyright  J. Caplan, H. Mitasova, L. Mitas, J. Hofierka, 
**      M. Zlocha
**
**This program is free software; you can redistribute it and/or
**modify it under the terms of the GNU General Public License
**as published by the Free Software Foundation; either version 2
**of the License, or (at your option) any later version.
**
**This program is distributed in the hope that it will be useful,
**but WITHOUT ANY WARRANTY; without even the implied warranty of
**MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**GNU General Public License for more details.
**
**You should have received a copy of the GNU General Public License
**along with this program; if not, write to the Free Software
**Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <grass/glocale.h>
#include "r.flow.h"
#include "mem.h"

#define OLD 0			/* magic	*/
#define NEW 1			/*		*/
#define TEMP 2			/*	numbers	*/

/****************************** Annoyances ******************************/

char *
tmp_name (char *fullname)

{
    char *mapset = G_mapset();
    char *location = G_location_path();
    char element[1024];
    char *el = element;

    G__temp_element(element);
    while (*fullname++ == *location++);
    while (*fullname++ == *mapset++);
    while (*fullname++ == *el++);
    return fullname;
}

/********************************* I/O **********************************/

struct Option *
parameter (char *key, int type, int required, char *options, char *gisprompt, char *description, char *answer)

{
    struct Option *opt = G_define_option();

    opt->key = key;
    if (type == TYPE_STRING)
        opt->key_desc = "name";
    opt->type = type;
    opt->required = required;
    opt->options = options;
    opt->gisprompt = gisprompt;
    opt->description = description;
    opt->answer = answer;

    return opt;
}

struct Flag *
flag (char key, char *description)

{
    struct Flag *theFlag = G_define_flag();

    theFlag->key = key;
    theFlag->description = description;

    return theFlag;
}

void
parse_command_line (int argc, char *argv[])

{
    struct GModule *module;
    struct Option *pelevin, *paspin, *pbarin, *pskip, *pbound, 
              *pflout, *plgout, *pdsout;

/* Helena: fseg commented due to problems: */
/*    struct Flag *fup, *flg, *fmem, *fseg, *fquiet, *fcprght;*/
    struct Flag *fup, *flg, *fmem, *fquiet, *fcprght;
    int default_skip, larger, default_bound;
    double default_offset;
    char *default_offset_ans, *offset_opt; 
    char *default_skip_ans, *default_bound_ans, *skip_opt;
    
	module = G_define_module();
	module->description =
		_("Construction of slope curves (flowlines), flowpath "
		"lengths, and flowline densities (upslope areas) from "
		"a raster digital elevation model (DEM)");

    larger = ((region.cols < region.rows) ? region.rows : region.cols);
    if (larger < 50)
	default_skip = 1;
    else
	default_skip = (int) (larger / 50);
    default_skip_ans = (char *) G_calloc((int) log10((double) default_skip)+2,
					 sizeof (char));
    skip_opt = (char *) G_calloc((int) log10((double)larger)+4, sizeof (char));

    sprintf(default_skip_ans, "%d", default_skip);
    sprintf(skip_opt, "1-%d", larger);

    default_bound = (int) (4. * hypot ((double) region.rows,
				       (double) region.cols));
    default_bound_ans = (char *) G_calloc((int)log10((double) default_bound)+4,
					  sizeof (char));
    sprintf (default_bound_ans, "0-%d", default_bound);

/* below fix changed from 0.0 to 1.0 and its effect disabled in calc.c, Helena June 2005 */
    default_offset = 1.0; /* fixed 20. May 2001 Helena */
    default_offset_ans = (char *) G_calloc((int) log10( default_offset) + 2,
					   sizeof (char));
    sprintf (default_offset_ans, "%f", default_offset);
    offset_opt = (char *) G_calloc((int) log10( default_offset) + 4,
				   sizeof (char)); 
    sprintf (offset_opt, "0.0-500.0");
    
    pelevin = parameter("elevin", TYPE_STRING, YES, NULL, "old,cell,raster",
			_("Input elevation file"), NULL);
    paspin  = parameter("aspin", TYPE_STRING, NO, NULL, "old,cell,raster",
			_("Input aspect file"), NULL);
    pbarin  = parameter("barin", TYPE_STRING, NO, NULL, "old,cell,raster",
			_("Input barrier file"), NULL);
    pskip   = parameter("skip", TYPE_INTEGER, NO, skip_opt, NULL,
			_("Number of cells between flowlines"), default_skip_ans);
    pbound  = parameter("bound", TYPE_INTEGER, NO, default_bound_ans, NULL,
			_("Maximum number of segments per flowline"),
			default_bound_ans + 2);
/* removed by helena June 2005
    poffset = parameter("offset", TYPE_DOUBLE, NO, offset_opt, NULL,
			"Maximum magnitude of random grid point offset", 
			default_offset_ans); 
*/
    pflout  = parameter("flout", TYPE_STRING, NO, NULL, "any,dig,vector",
			_("Output flowline vector file"), NULL);
    plgout  = parameter("lgout", TYPE_STRING, NO, NULL, "any,cell,raster",
			_("Output flowpath length raster file"), NULL);
    pdsout  = parameter("dsout", TYPE_STRING, NO, NULL, "any,cell,raster",
			_("Output flowline density raster file"), NULL);

    fup	  = flag('u', _("Compute upslope flowlines instead of default downhill flowlines"));
    flg	  = flag('3', _("3-D lengths instead of 2-D"));
    fmem  = flag('m', _("Use less memory, at a performance penalty"));
/*    fseg  = flag('M', "Use much less memory, at a severe performance penalty");*/
    fquiet= flag('q', _("Quiet operation"));
    fcprght = flag('h', _("Display Reference Information"));

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    parm.elevin	= pelevin->answer;
    parm.aspin	= paspin->answer;
    parm.barin	= pbarin->answer;
    parm.skip	= atoi(pskip->answer);
    parm.bound	= atoi(pbound->answer);
/*    parm.offset = atof(poffset->answer); removed by helena June 2005 */
    parm.flout	= pflout->answer;
    parm.lgout	= plgout->answer;
    parm.dsout	= pdsout->answer;
    parm.up	= fup->answer;
    parm.l3d	= flg->answer;
    parm.mem	= fmem->answer;
/*    parm.seg	= fseg->answer;*/
    parm.quiet	= fquiet->answer;

    if(!pflout->answer && !plgout->answer && !pdsout->answer)
	G_fatal_error(_("You must select one or more output maps (flout, lgout, dsout)")); 

    if (fcprght->answer) { 
      fprintf(stderr, "\n");
      fprintf(stderr, "Version: GRASS5.0, update: October 1999\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Authors: original program: J. Hofierka, M. Zlocha, H. Mitasova, L. Mitas\n");
      fprintf(stderr, "    new GRASS implementation: J. Caplan, M. Ruesink\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Methods used in this program are described in the following papers:\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Mitasova, H., and Hofierka, L., 1993\n");
      fprintf(stderr, "Interpolation by Regularized Spline with Tension:\n");
      fprintf(stderr, "II. Application to terrain modeling and surface geometry analysis.\n");
      fprintf(stderr, "Mathematical Geology 25, 657-669.\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Mitasova, H., Mitas, L., Brown, W.M., Gerdes, D.P., Kosinovsky, I.,\n");
      fprintf(stderr, "Baker, T., 1995, Modeling spatially and temporally\n");
      fprintf(stderr, "distributed phenomena: New methods and tools for GRASS GIS.\n");
      fprintf(stderr, "Int. Journal of Geographic Information Systems 9(4), 433-446.\n");
      fprintf(stderr, "(special issue on Integration of GIS and Environmental Modeling)\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "H. Mitasova, J. Hofierka, M. Zlocha, L.R. Iverson, 1996,\n");
      fprintf(stderr, "Modeling topographic potential for erosion and deposition using GIS.\n");
      fprintf(stderr, "Int. Journal of GIS, 629-641.\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "The postscript versions of these papers are available via Internet at\n");
      fprintf(stderr, "http://www2.gis.uiuc.edu:2280/modviz/papers/listsj.html\n");
      fprintf(stderr, "\n");
      fprintf(stderr, "Please cite these references in publications where the results of this\n");
      fprintf(stderr, "program are used.\n");
      fprintf(stderr, "\n");
     } 

    if (parm.seg)
	parm.mem = '\0';
    else if (parm.mem)
	parm.aspin = NULL;
}

void
diag (char *msg)

{
    if (!parm.quiet)
    {
	fprintf(stderr, msg);
	fflush(stderr);
    }
}

int
open_existing_cell_file (char *fname, struct Cell_head *chd)

{
    char   *mapset = G_find_cell(fname, "");

    if (mapset == NULL)
        G_fatal_error(_("Cannot find file %s"), fname);

    if (chd && (G_get_cellhd(fname, mapset, chd) < 0))
        G_fatal_error(_("Cannot get header for %s"), fname);

    return G_open_cell_old(fname, mapset);
}

void
read_input_files()
{
    DCELL   *barc;
    int     fd, row, col;
    struct  Cell_head hd;

    diag("Reading input files: elevation...\n");

    fd = open_existing_cell_file(parm.elevin, &hd);
    if (!((region.ew_res == hd.ew_res)
	  && (region.ns_res == hd.ns_res)))
	G_fatal_error(_("Elevation file's resolution differs from \
                       current region resolution"));
    for (row = 0; row < region.rows; row++)
    {
	G_get_d_raster_row(fd, el.buf[row], row);
	if (parm.seg)
	    put_row_seg(el, row);
    }
    if (parm.seg)
	segment_flush(el.seg);
    G_close_cell(fd);

    if (parm.aspin)
    {
	diag(", aspect");
	fd = open_existing_cell_file(parm.aspin, &hd);
	if (!((region.ew_res == hd.ew_res)
	      && (region.ns_res == hd.ns_res)))
	G_fatal_error(_("Aspect file's resolution differs from \
                       current region resolution"));
	for (row = 0; row < region.rows; row++)
	{
	    G_get_d_raster_row(fd, as.buf[row], row);
	    if (parm.seg)
		put_row_seg(as, row);
	}
	if (parm.seg)
	    segment_flush(as.seg);
	G_close_cell(fd);
    }

    if (parm.barin)
    {
	diag(", barrier");
	barc = G_allocate_d_raster_buf();
	fd = open_existing_cell_file(parm.barin, &hd);
	for (row = 0; row < region.rows; row++)
	{
	    G_get_d_raster_row(fd, barc, row);
	    for (col = 0; col < region.cols; col++)
	    {
		BM_set(bitbar, col, row, (barc[col] != 0));
		if (barc[col] != 0)
		    put(ds, row, col, -1);
	    }
	}
	G_close_cell(fd);
    }

    diag(".\n");
}

int
open_segment_file (char *name, layer l, int new)

{
    int fd;
    char *mapset;

    if (new == TEMP)
	G__temp_element(string);
    else
	sprintf(string, "cell_misc/%s", parm.elevin);

    if (new || !(mapset = G_find_file(string, name, "")))
    {
	if ((fd = G_open_new(string, name)) < 0)
	    G_fatal_error(_("Cannot create segment file %s"),
		    name);
	if (segment_format(fd, region.rows + l.row_offset * 2, 
		       region.cols + l.col_offset * 2, SEGROWS, SEGCOLS,
		       sizeof(DCELL)) < 1)
	    G_fatal_error(_("Cannot format segment file %s"),
		    name);
	close(fd);
	mapset = G_mapset();
    }
/*    if ((fd = G_open_update(string, name, mapset)) < 0)*/ /* update 10/99*/
    if ((fd = G_open_update(string, name)) < 0)
	G_fatal_error(_("Cannot open segment file %s"), name);
    return fd;
}

void
open_output_files()
{
    diag("Opening output files...");

    if (parm.seg)
    {
	el.sfd = open_segment_file("elevation.seg", el, OLD);
	as.sfd = open_segment_file("aspect.seg", as, OLD);
	if (parm.dsout)
	    ds.sfd = open_segment_file(tmp_name(G_tempfile()), ds, TEMP);
    }

    if (parm.lgout && ((lgfd = G_open_raster_new(parm.lgout, FCELL_TYPE)) < 0))
	G_fatal_error(_("Cannot create raster map %s"),
		parm.lgout);

    if (parm.flout && (Vect_open_new(&fl, parm.flout, 0) < 0))
	G_fatal_error(_("Cannot create vector map %s"),
		parm.flout);

    diag("done.\n");
}

void
close_files()
{
    diag("Closing files...");

    if (parm.seg)
    {
	close(el.sfd);
	close(as.sfd);
	if (parm.dsout)
	    close(ds.sfd);
    }
 /*   if (parm.lgout)
	G_close_cell(lgfd);*/
    if (parm.flout){
	Vect_build (&fl, stderr);
	Vect_close(&fl);
    }
    diag("done.\n");
}

void
write_density_file()
{
    char   *mapset;
    int     dsfd, row, col;
    double  dsmax = 0.0;
    struct  Colors colors;

    if (G_set_window(&region) < 0)
	G_fatal_error(_("Cannot reset current region"));

    diag("Writing density file...");
/*    dsfd = G_open_cell_new(parm.dsout); */
    dsfd = G_open_raster_new(parm.dsout, DCELL_TYPE);
    if (dsfd < 0)
	G_fatal_error(_("Cannot create raster map %s"),
		parm.dsout);
    for (row = 0; row < region.rows; row++)
    {
	G_put_raster_row(dsfd, get_row(ds, row), DCELL_TYPE);
	for (col = 0; col < region.cols; col++)
	    if (ds.buf[row][col] > dsmax)
		dsmax = ds.buf[row][col];	
    }
    G_close_cell(dsfd);
    
    G_init_colors(&colors);
    
    G_add_color_rule(-1,   0,0,0,       -1,           0,0,0,	 &colors);
    G_add_color_rule(0,    255,255,255, 5,            255,255,0, &colors);
    G_add_color_rule(5,    255,255,0,   30,           0,255,255, &colors);
    G_add_color_rule(30,   0,255,255,   100,          0,127,255, &colors);
    G_add_color_rule(100,  0,127,255,   1000,         0,0,255,	 &colors);
    G_add_color_rule(1000, 0,0,255,     (CELL) dsmax, 0,0,0,	 &colors);
    
    if ((mapset = G_find_file("cell", parm.dsout, "")) == NULL)
	G_fatal_error(_("Cannot find file %s"), parm.dsout);
    G_write_colors(parm.dsout, mapset, &colors);
    G_free_colors(&colors);
    diag("done.\n");
}
