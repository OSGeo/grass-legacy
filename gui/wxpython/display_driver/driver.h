#include <iostream> // debug
#include <vector>
#include <map>
#include <cmath>

// For compilers that support precompilation, includes "wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/wx.h>
#endif

#include <wx/dc.h>
#include <wx/list.h>

#include <Python.h>
//#include <wx/wxPython/pseudodc.h>
#include "pseudodc.h"

extern "C" {
#include <grass/gis.h>
#include <grass/Vect.h>
}

//#define DEBUG

class DisplayDriver
{
 private:
    wxPseudoDC *dc;  // device content
    long int   dcId; // wxDC id starting

    struct lineDesc {
	int      npoints;
	long int startId;
    };

    typedef std::map<int, lineDesc> ids_map;

    ids_map ids; // gId : {dcIds, ...}

    std::vector<int> selected; // list of selected features (gId)

    struct Map_info  *mapInfo;
    struct line_pnts *points;       // east, north, depth
    wxList           *pointsScreen; // x, y, z
    struct line_cats *cats;
    
    struct _region {
	double north; // map units
	double south;
	double east;
	double west;
	double ns_res;
	double ew_res;
	double center_easting;
	double center_northing;

	double map_width;  // px
	double map_height;

	// real region
	double west_real;
	double north_real;
	double res;
    } region;

    struct symbol {
	bool enabled;
	wxColor color;
    };

    struct _settings {
	wxColor highlight;
	
	symbol point;
	symbol line;
	
	symbol boundaryNo;
	symbol boundaryOne;
	symbol boundaryTwo;

	symbol centroidIn;
	symbol centroidOut;
	symbol centroidDup;
	
	symbol nodeOne;
	symbol nodeTwo;

	symbol vertex;

	int lineWidth; // screen units 

    } settings;

    void Cell2Pixel (double east, double north, double depth,
		     int *x, int *y, int *z);
    
    int DrawCross(int line, const wxPoint *point, int size=5);

    int DrawLine(int line);
    int DrawLineVerteces(int line);
    int DrawLineNodes(int line);

    /* debug */
    void PrintIds();

    /* select feature */
    bool IsSelected(int line);

 public:
    /* constructor */
    DisplayDriver();
    /* destructor */
    ~DisplayDriver();

    /* display */
    int DrawMap(void *device);

    /* select */
    int SelectLinesByBox(double x1, double y1, double x2, double y2);
    int SelectLinesByPoint(double x, double y, double thresh,
			   int onlyType);

    void Unselect();
    std::vector<int> GetSelected(bool grassId);
    int SetSelected(std::vector<int> id);
    std::vector<int> GetSelectedVertex(double x, double y);

    /* general */
    void CloseMap();
    void OpenMap(const char *mapname, const char *mapset);
    void ReloadMap();

    /* set */
    void SetRegion(double north, double south, double east, double west,
		   double ns_res, double ew_res,
		   double center_easting, double center_northing,
		   double map_width, double map_height);

    void SetSettings(unsigned long highlight,
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
		     int lineWidth);
};
