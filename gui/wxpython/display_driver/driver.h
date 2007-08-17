#include <iostream>

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

extern "C" {
#include <grass/gis.h>
#include <grass/Vect.h>
}

class DisplayDriver
{
 private:
    wxDC             *dc; /* device content */

    struct Map_info  *mapInfo;
    struct line_pnts *points;       // east, north, depth
    wxList           *pointsScreen; // x, y, z
    struct line_cats *cats;
    
    struct _region {
	double north;
	double south;
	double east;
	double west;
	double ns_res;
	double ew_res;
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
    
    int DrawCross(const wxPoint *point, int size=5);

    int DrawLine(int line);
    int DrawLineVerteces();
    int DrawLineNodes(int line);

 public:
    /* constructor */
    DisplayDriver();
    /* destructor */
    ~DisplayDriver();

    /* display */
    int DrawMap(void *device);

    /* general */
    void CloseMap();
    void OpenMap(const char *mapname, const char *mapset);

    /* set */
    void SetRegion(double north, double south, double east, double west,
		   double ns_res, double ew_res);
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
