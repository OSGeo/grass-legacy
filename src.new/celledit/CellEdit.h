#include <X11/copyright.h>

#ifndef _CellEdit_h
#define _CellEdit_h

#include <X11/Xaw/Simple.h>
#include <X11/Xmu/Converters.h>

/****************************************************************
 *
 * CellEdit widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cellFile	     CellFile		String		NULL
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	400
 location	     Location		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 mapset		     Mapset		String		NULL
 mouseCallback	     Callback		Pointer		NULL
 resize		     Resize		Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	400
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */


#define XtNcellfile "cellfile"
#define XtCCellfile "Cellfile"
#define XtNlocation "location"
#define XtCLocation "Location"
#define XtNmapset "mapset"
#define XtCMapset "Mapset"
#define XtNmouseCallback "mouseCallback"
#define XtCMouseCallback "MouseCallback"
#define XtNresize "resize"
#define XtCResize "Resize"

/* declare specific CellEditWidget class and instance datatypes */

typedef struct _CellEditClassRec*	CellEditWidgetClass;
typedef struct _CellEditRec*		CellEditWidget;

/* declare public functions here */
extern void LoadCellFile();
extern Status SetSaveFile();
extern char *GetCellPath();
extern void SaveCellFile();
extern void GetImageExtents();
extern void QueryCellFile();

/* declare the class constant */

extern WidgetClass cellEditWidgetClass;

#endif /* _CellEdit_h */
