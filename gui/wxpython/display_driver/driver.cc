/**
   \file driver.cc
   
   \brief Experimental C++ wxWidgets display driver

   This driver is designed for wxPython GRASS GUI (digitization tool).
   Draw vector map layer to PseudoDC.

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
   
   \param[in,out] PseudoDC device where to draw vector objects
   
   \return
*/
DisplayDriver::DisplayDriver(void *device)
{
    G_gisinit(""); /* GRASS functions */

    mapInfo = NULL;
    dcId    = 1;

    dc = (wxPseudoDC *) device;

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
   \brief Set device for drawing
   
   \param[in,out] PseudoDC device where to draw vector objects

   \return
*/
void DisplayDriver::SetDevice(void *device)
{
    dc = (wxPseudoDC *) device;

    return;
}

/**
   \brief Draw content of the vector map to device
   
   \return number of lines which were drawn
   \return -1 on error
 */
int DisplayDriver::DrawMap()
{
    if (!mapInfo || !dc)
	return -1;

    int nlines;
    struct ilist *listLines;

    // initialize
    dcId = 1;
    ids.clear();
    listLines = Vect_new_list();

    /* nlines = Vect_get_num_lines(mapInfo); */

    // draw lines inside of current display region
    nlines = Vect_select_lines_by_box(mapInfo,&(region.box),
				      GV_POINTS | GV_LINES, // fixme
				      listLines);

    for (int i = 0; i < listLines->n_values; i++) {
	DrawLine(listLines->value[i]);
    }

#ifdef DEBUG
    PrintIds();
#endif

    Vect_destroy_list(listLines);

    return listLines->n_values;
}	

/**
   \brief Draw selected vector objects to the device
 
   \param[in] line id
 
   \return 1 on success
   \return -1 on failure (vector object is dead, etc.)
*/
int DisplayDriver::DrawLine(int line)
{
    if (!dc || !Vect_line_alive (mapInfo, line))
	return -1;

    int type;    // line type
    int x, y, z; // screen coordinates
    bool draw;   // draw object ?

    // read line
    type = Vect_read_line (mapInfo, points, cats, line);

    // clear screen points & convert EN -> xy
    pointsScreen->Clear();
    for (int i = 0; i < points->n_points; i++) {
	Cell2Pixel(points->x[i], points->y[i], points->z[i],
		   &x, &y, &z);
	pointsScreen->Append((wxObject*) new wxPoint(x, y)); /* TODO: 3D */
    }

    // add ids
    // -> node1, line1, vertex1, line2, ..., node2
    struct lineDesc desc = {points->n_points, dcId};
    ids[line] = desc;
    // update id for next line
    dcId += points->n_points * 2 - 1;

    // determine color of vector object
    if (IsSelected(line)) {
	dc->SetPen(wxPen(settings.highlight, settings.lineWidth, wxSOLID));
	draw = true;
    }
    else {
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
		draw = false;
		break;
	    }
	}
	else if (type & GV_POINTS) {
	    if (type == GV_POINT && settings.point.enabled) {
		dc->SetPen(wxPen(settings.point.color, settings.lineWidth, wxSOLID));
		draw = true;
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
	    }
	}
    }

    // draw object
    if (draw) {
	if (type & GV_POINTS) {
	    DrawCross(line, (const wxPoint *) pointsScreen->GetFirst()->GetData());
	}
	else {
	    long int startId = ids[line].startId + 1;
	    
	    for (int i = 0; i < pointsScreen->GetCount() - 1; startId += 2) {
		wxPoint *point_beg = (wxPoint *) pointsScreen->Item(i)->GetData();
		wxPoint *point_end = (wxPoint *) pointsScreen->Item(++i)->GetData();
	    
		// set bounds for line
		// wxRect rect (*point_beg, *point_end);
		// dc->SetIdBounds(startId, rect);
		
		// draw line if needed
		dc->SetId(startId);
		dc->DrawLine(point_beg->x, point_beg->y,
			     point_end->x, point_end->y);
	    }
	    DrawLineVerteces(line); // draw vertices
	    DrawLineNodes(line);    // draw nodes
	}
    }

    return 1;
}

/**
   \brief Draw line verteces to the device
 
   Except of first and last vertex, see DrawLineNodes().

   \param line id

   \return number of verteces which were drawn
   \return -1 if drawing vertices is disabled
*/
int DisplayDriver::DrawLineVerteces(int line)
{
    long int id;
    wxPoint *point;

    if (!settings.vertex.enabled)
	return -1;

    // determine color
    if (!IsSelected(line)) {
	dc->SetPen(wxPen(settings.vertex.color, settings.lineWidth, wxSOLID));
    }
    else {
	dc->SetPen(wxPen(settings.highlight, settings.lineWidth, wxSOLID));
    }

    // set id
    id = ids[line].startId + 2;
    for (int i = 1; i < pointsScreen->GetCount() - 1; i++, id += 2) {
	point = (wxPoint*) pointsScreen->Item(i)->GetData();
	//wxRect rect (*point, *point);
	//dc->SetIdBounds(id, rect);
	dc->SetId(id);
	DrawCross(line, (const wxPoint*) pointsScreen->Item(i)->GetData());
    }

    return pointsScreen->GetCount() - 2;
}

/**
   \brief Draw line nodes to the device
 
   \param line id

   \return 1
   \return -1 if no nodes were drawn
*/
int DisplayDriver::DrawLineNodes(int line)
{
    int node;
    long int id;
    double east, north, depth;
    int x, y, z;
    int nodes [2];
    bool draw;
    
    // draw nodes??
    if (!settings.nodeOne.enabled && !settings.nodeTwo.enabled)
	return -1;

    // get nodes
    Vect_get_line_nodes(mapInfo, line, &(nodes[0]), &(nodes[1]));
        
    for (int i = 0; i < sizeof(nodes) / sizeof(int); i++) {
	node = nodes[i];
	// get coordinates
	Vect_get_node_coor(mapInfo, node,
			   &east, &north, &depth);

	// convert EN->xy
	Cell2Pixel(east, north, depth,
		   &x, &y, &z);

	// determine color
	if (IsSelected(line)) {
	    dc->SetPen(wxPen(settings.highlight, settings.lineWidth, wxSOLID));
	    draw = true;
	}
	else {
	    if (Vect_get_node_n_lines(mapInfo, node) == 1) {
		dc->SetPen(wxPen(settings.nodeOne.color, settings.lineWidth, wxSOLID));
		draw = settings.nodeOne.enabled;
	    }
	    else {
		dc->SetPen(wxPen(settings.nodeTwo.color, settings.lineWidth, wxSOLID));
		draw = settings.nodeTwo.enabled;
	    }
	}
	
	// node1, line1, vertex1, line2, vertex2, ..., node2
	if (i == 0) // first node
	  id = dcId - points->n_points * 2 + 1;
	else // last node
	  id = dcId - 1;

	wxPoint point(x, y);
	// wxRect rect (point, point);
	// dc->SetIdBounds(id, rect);

	// draw node if needed
	if (draw) {
	    dc->SetId(id);
	    DrawCross(line, &point);
	}
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

/**
   \brief Reload vector map layer

   Close and open again. Needed for modification using v.edit.

   TODO: Get rid of that...

   \param
   
   \return
*/
void DisplayDriver::ReloadMap()
{
    // char* name   = G_store(Vect_get_map_name(mapInfo)); ???
    char* name   = G_store(mapInfo->name);
    char* mapset = G_store(Vect_get_mapset(mapInfo));

    Vect_close(mapInfo);
    mapInfo = NULL;

    return OpenMap(name, mapset);
}

/*
  \brief Conversion from geographic coordinates (east, north)
  to screen (x, y)
  
  TODO: 3D stuff...

  \param[in] east,north,depth geographical coordinates
  \param[out] x, y, z screen coordinates
  
  \return 
*/
void DisplayDriver::Cell2Pixel(double east, double north, double depth,
			       int *x, int *y, int *z)
{
    *x = int((east  - region.map_west) / region.map_res);
    *y = int((region.map_north - north) / region.map_res);
    *z = 0;

    return;
}

/**
   \brief Set geographical region
 
   Region must be upgraded because of Cell2Pixel().
   
   \param[in] north,south,east,west,ns_res,ew_res region settings
 
   \return
*/
void DisplayDriver::SetRegion(double north, double south, double east, double west,
			      double ns_res, double ew_res,
			      double center_easting, double center_northing,
			      double map_width, double map_height)
{
    region.box.N  = north;
    region.box.S  = south;
    region.box.E  = east;
    region.box.W  = west;
    region.ns_res = ns_res;
    region.ew_res = ew_res;

    region.center_easting = center_easting;
    region.center_northing = center_northing;

    region.map_width  = map_width;
    region.map_height = map_height;

    // calculate real region
    region.map_res = (region.ew_res > region.ns_res) ? region.ew_res : region.ns_res;

    region.map_west  = region.center_easting - (region.map_width / 2.) * region.map_res;
    region.map_north = region.center_northing + (region.map_height / 2.) * region.map_res;

    return;
}

/**
   \brief Draw cross symbol of given size to device content
   
   Used for points, nodes, vertices

   \param[in] point coordinates of center
   \param[in] size size of the cross symbol
   
   \return 1 on success
   \return -1 on failure
*/
int DisplayDriver::DrawCross(int line, const wxPoint* point, int size)
{
    if (!dc || !point)
	return -1;

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

/**
   \brief Prints gId: dcIds

   Useful for debugging purposes.

   \param

   \return
*/
void DisplayDriver::PrintIds()
{
    for (ids_map::const_iterator i = ids.begin(), e = ids.end();
	 i != e; ++i) {
	std::cout << "line=" << i->first << ": "
		  << "npoints=" << i->second.npoints
		  << " startId=" << i->second.startId
		  << std::endl;
    }

    for (std::vector<int>::const_iterator i = selected.begin(), e = selected.end();
	 i != e; ++i)
	std::cout << "selected: " << *i << " ";
    std::cout << std::endl;

    return;
}

/**
   \brief Select vector objects by given bounding box
   
   \param[in] x1,y1,x2,y2 corners coordinates of bounding box

   \return number of selected features
   \return -1 on error
*/
int DisplayDriver::SelectLinesByBox(double x1, double y1, double x2, double y2)
{
    if (!mapInfo)
	return -1;

    int type, line;
    double dx, dy;

    struct ilist *list;
    struct line_pnts *bbox;

    type = -1; // all types

    list = Vect_new_list();
    bbox = Vect_new_line_struct();

    dx = std::fabs(x2 - x1);
    dy = std::fabs(y2 - y1);
        
    Vect_append_point(bbox, x1, y1, 0.0);
    Vect_append_point(bbox, x2, y1, 0.0);
    Vect_append_point(bbox, x2, y2, 0.0);
    Vect_append_point(bbox, x1, y2, 0.0);
    Vect_append_point(bbox, x1, y1, 0.0);
        
    Vect_select_lines_by_polygon(mapInfo, bbox,
				 0, NULL,
				 type, list);
	
    for (int i = 0; i < list->n_values; i++) {
	line = list->value[i];
	selected.push_back(line);
    }

    // remove all duplicate ids
    sort(selected.begin(), selected.end());
    selected.erase(unique(selected.begin(), selected.end()), selected.end());

    Vect_destroy_line_struct(bbox);
    Vect_destroy_list(list);

    return selected.size();
}

/**
   \brief Select vector feature by given point in given
   threshold
   
   Only one vector object can be selected.

   \param[in] x,y point of searching
   \param[in] thresh threshold value where to search
   \param[in] onlyType select vector object of given type

   \return 1 vector object found
   \return 0 no vector object found
*/
int DisplayDriver::SelectLinesByPoint(double x, double y, double thresh,
				      int onlyType)
{
    int line;
    
    line = Vect_find_line(mapInfo, x, y, 0.0,
			  GV_POINTS | GV_LINES, thresh, 0, 0);

    std::cout << x << " " << y << " " << thresh << "->" << line << std::endl;

    if (line > 0) {
	selected.push_back(line);
	return 1;
    }

    return 0;
}

/**
   \brief Is vector object selected?
   
   \param[in] line id

   \return true if vector object is selected
   \return false if vector object is not selected
*/
bool DisplayDriver::IsSelected(int line)
{
    for(std::vector<int>::const_iterator i = selected.begin(), e = selected.end();
	i != e; ++i) {
	if (line == *i)
	    return true;
    }

    return false;
}

/**
   \brief Unselect selected features by user

   Clear list of ids of selected vector objects

   \param

   \return
*/
void DisplayDriver::Unselect()
{
    selected.clear();

    return;
}

/**
   \brief Get ids of selected objects

   \param[in] grassId if true return GRASS line ids
   if false return PseudoDC ids
   
   \return list of ids of selected vector objects
*/
std::vector<int> DisplayDriver::GetSelected(bool grassId)
{
    if (grassId)
	return selected;

    std::vector<int> dc_ids;

    for(std::vector<int>::const_iterator i = selected.begin(), e = selected.end();
	i != e; ++i) {
	ids_map::const_iterator ii = ids.find(*i);
	if (ii != ids.end()) { // line found
	    long int endId = ii->second.npoints * 2 - 1 + ii->second.startId;
	    for (long int id = ii->second.startId; id < endId; id++) {
		dc_ids.push_back(id);
	    }
	}
    }

    return dc_ids;
}

/**
   \brief Set selected vector objects
   
   \param[in] list of GRASS ids to be set

   \return 1
*/
int DisplayDriver::SetSelected(std::vector<int> id)
{
    selected = id;

    return 1;
}

/**
   \brief Get PseudoDC vertex id of selected line

   \param[in] x,y coordinates of click

   \return id of center, left and right vertex

   \return 0 no line found
   \return -1 on error
*/
std::vector<int> DisplayDriver::GetSelectedVertex(double x, double y)
{
    struct lineDesc *desc; // line desription

    int line, type;
    int Gid, DCid;
    int vx, vy, vz;      // vertex screen coordinates

    double dist, minDist;

    std::vector<int> returnId;

    // only one object can be selected
    if (selected.size() != 1)
	return returnId;

    line = selected[0];
    
    type = Vect_read_line (mapInfo, points, cats, line);
        
    // find the closest vertex (x, y)
    for(int idx = 0; idx < points->n_points; idx++) {
	dist = Vect_points_distance(x, y, 0.0,
				    points->x[idx], points->y[idx], points->z[idx], 0);
	
	if (idx == 0) {
	    minDist = dist;
	    Gid  = idx;
	}
	else {
	    if (minDist > dist) {
		minDist = dist;
		Gid = idx;
	    }
	}
    }	
    
    desc = &(ids[line]);

    // translate id
    DCid = Gid * 2 + desc->startId;

    // add selected vertex
    returnId.push_back(DCid);
    Cell2Pixel(points->x[Gid], points->y[Gid], points->z[Gid],
	       &vx, &vy, &vz);
    wxRect rect (vx, vy, 0, 0);
    dc->SetIdBounds(DCid, rect);

    // left vertex
    if (DCid == desc->startId) {
	returnId.push_back(-1);
    }
    else {
	returnId.push_back(DCid - 2);
	Cell2Pixel(points->x[Gid-2], points->y[Gid-2], points->z[Gid-2],
		   &vx, &vy, &vz);
	wxRect rect (x, y, 0, 0);
	dc->SetIdBounds(DCid-2, rect);
    }

    // right vertex
    if (DCid == (desc->npoints - 1) * 2 + desc->startId) {
	returnId.push_back(-1);
    }
    else {
	returnId.push_back(DCid + 2);
	Cell2Pixel(points->x[Gid+2], points->y[Gid+2], points->z[Gid+2],
		   &vx, &vy, &vz);
	wxRect rect (x, y, 0, 0);
	dc->SetIdBounds(DCid + 2, rect);
    }

    return returnId;
}
