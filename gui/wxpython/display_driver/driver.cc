/**
   \file driver.cc
   
   \brief Experimental C++ wxWidgets display driver

   This driver is designed for wxPython GRASS GUI.
   Displays vector map layers when Digitization tool is
   activated.

   \author Martin Landa <landa.martin gmail.com>

   (C) 2007 by the GRASS Development Team
 
   This program is free software under the GNU General Public
   License (>=v2). Read the file COPYING that comes with GRASS
   for details.
*/

#include "driver.h"

/**
   \brief Initialize driver

   Allocate given structures.
   
   \param
   
   \return
*/
DisplayDriver::DisplayDriver()
{
    G_gisinit(""); /* need by other GRASS functions */

    mapInfo = NULL;

    points       = Vect_new_line_struct();
    pointsScreen = new wxList();
    cats         = Vect_new_cats_struct();

}

/**
   \brief Destroy driver

   Close the map, deallocate given structures.

   \param

   \return
*/
DisplayDriver::~DisplayDriver()
{
    if (mapInfo)
	CloseMap();

    Vect_destroy_line_struct(points);
    delete pointsScreen;
    Vect_destroy_cats_struct(cats);
}

/**
   \brief Display content of the map in device
   
   \param[in,out] device wxDC object where to draw vector features
   
   \return number of displayed features
   \return -1 on error
 */
int DisplayDriver::DrawMap(void *device)
{
    if (!mapInfo)
	return -1;

    dc = (wxDC *) device;

    int nlines;

    nlines = Vect_get_num_lines(mapInfo);

    for (int line = 1; line <= nlines; line++) {
	DrawLine(line);
    }

    dc = NULL;

    return nlines;
}	

/**
   \brief Display selected vector feature
 
   \param[in] id of the vector feature
 
   \return 1 on success
   \return -1 on failure (vector feature is dead, etc.)
*/
int DisplayDriver::DrawLine(int line)
{
    if (!dc || !Vect_line_alive (mapInfo, line))
	return -1;

    int type;    // line type
    int x, y, z; // screen coordinates
    bool draw;   // draw object ?

    type = Vect_read_line (mapInfo, points, cats, line);
    pointsScreen->Clear();

    //self.ids[line] = []
    for (int i = 0; i < points->n_points; i++) {
	Cell2Pixel(points->x[i], points->y[i], points->z[i],
		   &x, &y, &z);
	pointsScreen->Append((wxObject*) new wxPoint(x, y)); /* TODO: 3D */
    }

    // draw vector feature
    if (type & GV_LINES) {
	switch (type) {
	case GV_LINE:
	    dc->SetPen(wxPen(settings.line.color, settings.lineWidth, wxSOLID));
	    draw = settings.line.enabled;
	    break;
	case GV_BOUNDARY:
	    int left, right;
	    Vect_get_line_areas(mapInfo, line,
				&left, &right);
	    if (left == 0 && right == 0) {
		dc->SetPen(wxPen(settings.boundaryNo.color, settings.lineWidth, wxSOLID));
		draw = settings.boundaryNo.enabled;
	    }
	    else if (left > 0 && right > 0) {
		dc->SetPen(wxPen(settings.boundaryTwo.color, settings.lineWidth, wxSOLID));
		draw = settings.boundaryTwo.enabled;
	    }
	    else {
		dc->SetPen(wxPen(settings.boundaryOne.color, settings.lineWidth, wxSOLID));
		draw = settings.boundaryOne.enabled;
	    }
	    break;
	default:
	    draw = FALSE;
	    break;
	}

	if (draw) {
	    dc->DrawLines(pointsScreen);
	    if (settings.vertex.enabled) {
		dc->SetPen(wxPen(settings.vertex.color, settings.lineWidth, wxSOLID));
		DrawLineVerteces();
	    }
	    DrawLineNodes(line);
	}
    }
    else if (type & GV_POINTS) {
	if (type == GV_POINT && settings.point.enabled) {
	    dc->SetPen(wxPen(settings.point.color, settings.lineWidth, wxSOLID));
	    DrawCross((const wxPoint *) pointsScreen->GetFirst()->GetData());
	}
	else if (type == GV_CENTROID) {
	    int cret = Vect_get_centroid_area(mapInfo, line);
	    if (cret > 0) { // -> area
		draw = settings.centroidIn.enabled;
		dc->SetPen(wxPen(settings.centroidIn.color, settings.lineWidth, wxSOLID));
	    }
	    else if (cret == 0) {
		draw = settings.centroidOut.enabled;
		dc->SetPen(wxPen(settings.centroidOut.color, settings.lineWidth, wxSOLID));
	    }
	    else {
		draw = settings.centroidDup.enabled;
		dc->SetPen(wxPen(settings.centroidDup.color, settings.lineWidth, wxSOLID));
	    }

	    if (draw) 
		DrawCross((const wxPoint *) pointsScreen->GetFirst()->GetData());
	}
    }

    return 1;
}

/**
   \brief Display verteces of the line
 
   Except of first and last vertex, see DisplayNodes().

   \param

   \return number of displayed verteces
*/
int DisplayDriver::DrawLineVerteces()
{
    for (int i = 1; i < pointsScreen->GetCount() - 1; i++) {
	DrawCross((const wxPoint*) pointsScreen->Item(i)->GetData());
    }

    return pointsScreen->GetCount() - 2;
}

/**
   \brief Display nodes of the line
 
   \param

   \return 1
*/
int DisplayDriver::DrawLineNodes(int line)
{
    int node;
    double east, north, depth;
    int x, y, z;
    int nodes [2];
    
    Vect_get_line_nodes(mapInfo, line, &(nodes[0]), &(nodes[1]));
        
    for (int i = 0; i < sizeof(nodes) / sizeof(int); i++) {
	node = nodes[i];
	Vect_get_node_coor(mapInfo, node,
			   &east, &north, &depth);

	Cell2Pixel(east, north, depth,
		   &x, &y, &z);

	if (Vect_get_node_n_lines(mapInfo, node) == 1) 
	    dc->SetPen(wxPen(settings.nodeOne.color, settings.lineWidth, wxSOLID));
	else
	    dc->SetPen(wxPen(settings.nodeTwo.color, settings.lineWidth, wxSOLID));

	wxPoint point(x, y);
	DrawCross(&point);

	//elf.ids[line].append(self.mapwindow.DrawCross(coords, size=5))
    }

    return 1;
}


/*
  \brief Close vector map layer
  
  \param

  \return
*/
void DisplayDriver::CloseMap()
{
    if (mapInfo) {
	Vect_close(mapInfo);
	G_free ((void *) mapInfo);
	mapInfo = NULL;
    }
    
    return;
}

/**
   \brief Open vector map layer
 
   \param[in] mapname name of vector map
   \param[in] mapset name of mapset where the vector map layer is stored
   
   \return
*/
void DisplayDriver::OpenMap(const char* mapname, const char *mapset)
{
    if (!mapInfo)
	mapInfo = (struct Map_info *) G_malloc (sizeof (struct Map_info));

// define open level (level 2: topology)
    Vect_set_open_level(2);

// open existing map
    Vect_open_old(mapInfo, (char*) mapname, (char *) mapset);

    return;
}

/*
  \brief Conversion from geographic coordinates (east, north)
  to screen (x, y)
  
  \param[in] east,north,elev geographical coordinates
  \param[out] x, y, z screen coordinates
  
  \return 
*/
void DisplayDriver::Cell2Pixel(double east, double north, double depth,
			       int *x, int *y, int *z)
{
    *x = int((east  - region.west) / region.ew_res);
    *y = int((region.north - north) / region.ns_res);
    *z = 0;

    return;
}

/**
   \brief Set geographical region
 
   Needed for Cell2Pixel().
   
   \param[in] north,south,east,west,ns_res,ew_res region settings
 
   \return
*/
void DisplayDriver::SetRegion(double north, double south, double east, double west,
			      double ns_res, double ew_res)
{
    region.north  = north;
    region.south  = south;
    region.east   = east;
    region.west   = west;
    region.ns_res = ns_res;
    region.ew_res = ew_res;

    return;
}

/**
   \brief Draw cross symbol of given size in device content
   
   Used for points, node, vertices

   \param[in] point coordinates of center
   \param[in] size size of the cross symbol
   
   \return 1 on success
   \return -1 on failure
*/
int DisplayDriver::DrawCross(const wxPoint* point, int size)
{
    if (!dc || !point)
	return -1;

    //self.lineid = wx.NewId()
    dc->DrawLine(point->x - size, point->y, point->x + size, point->y);
    dc->DrawLine(point->x, point->y - size, point->x, point->y + size);

    return 1;
}

/*
  \brief Set settings for displaying vector feature
 
  E.g. line width, color, ...
  
  \param[in] lineWidth,... settgings
  
  \return 
*/
void DisplayDriver::SetSettings(unsigned long highlight,
				bool ePoint,       unsigned long cPoint, /* enabled, color */
				bool eLine,        unsigned long cLine,
				bool eBoundaryNo,  unsigned long cBoundaryNo,
				bool eBoundaryOne, unsigned long cBoundaryOne,
				bool eBoundaryTwo, unsigned long cBoundaryTwo,
				bool eCentroidIn,  unsigned long cCentroidIn,
				bool eCentroidOut, unsigned long cCentroidOut,
				bool eCentroidDup, unsigned long cCentroidDup,
				bool eNodeOne,     unsigned long cNodeOne,
				bool eNodeTwo,     unsigned long cNodeTwo,
				bool eVertex,      unsigned long cVertex,
				int lineWidth)
{
    settings.highlight.Set(highlight);

    settings.point.enabled = ePoint;
    settings.point.color.Set(cPoint);

    settings.line.enabled = eLine;
    settings.line.color.Set(cLine);

    settings.boundaryNo.enabled = eBoundaryNo;
    settings.boundaryNo.color.Set(cBoundaryNo);
    settings.boundaryOne.enabled = eBoundaryOne;
    settings.boundaryOne.color.Set(cBoundaryOne);
    settings.boundaryTwo.enabled = eBoundaryTwo;
    settings.boundaryTwo.color.Set(cBoundaryTwo);


    settings.centroidIn.enabled = eCentroidIn;
    settings.centroidIn.color.Set(cCentroidIn);
    settings.centroidOut.enabled = eCentroidOut;
    settings.centroidOut.color.Set(cCentroidOut);
    settings.centroidDup.enabled = eCentroidDup;
    settings.centroidDup.color.Set(cCentroidDup);

    settings.nodeOne.enabled = eNodeOne;
    settings.nodeOne.color.Set(cNodeOne);
    settings.nodeTwo.enabled = eNodeTwo;
    settings.nodeTwo.color.Set(cNodeTwo);

    settings.vertex.enabled = eVertex;
    settings.vertex.color.Set(cVertex);


    settings.lineWidth = lineWidth;
    
}
