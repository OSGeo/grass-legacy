#include "xgsupport.h"

static int done = 0;
static int retval = 0;

void
XgCellhdVerifyEventLoop(appContext, widget)
XtAppContext    appContext;
Widget widget;
{
    Display *display;

    done = 0;
    retval = 0;
    while (done == 0 || XtPending()) {
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }
    XtUnmanageChild(widget);
    XSync(XtDisplay(widget), False);
    while (XtPending()) {        
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }
}


DoCellhdVerify(shell, cellhd, data)
Widget shell;
struct Cell_head *cellhd;
EditCellhdData *data;
{
    struct Cell_head def_wind;
    struct Cell_head save;
    char *north;
    char *south;
    char *east;
    char *west;
    double nval, sval, eval, wval, nsval, ewval;
    char buffer[256];
    char bigbuffer[8096];
    char buf[64], buf2[30];
    char *err;

    if ( G_get_default_window(&def_wind) != 1) {
        XgWarningDialog(shell, "Can't get default window.");
	return;
    }

    save.format = cellhd->format;
    save.compressed = cellhd->compressed;
    save.rows = cellhd->rows;
    save.cols = cellhd->cols;
    save.proj = cellhd->proj;
    save.zone = cellhd->zone;
    save.ew_res = cellhd->ew_res;
    save.ns_res = cellhd->ns_res;
    save.north = cellhd->north;
    save.south = cellhd->south;
    save.east = cellhd->east;
    save.west = cellhd->west;

    north = XmTextFieldGetString(data->north);
    south = XmTextFieldGetString(data->south);
    east = XmTextFieldGetString(data->east);
    west = XmTextFieldGetString(data->west);
    G_squeeze(north);
    G_squeeze(south);
    G_squeeze(east);
    G_squeeze(west);

    if ( !G_scan_northing(north, &cellhd->north, cellhd->proj)) {
	sprintf(buffer, "Illegal value for north edge: %s", north);
	XgWarningDialog(shell, buffer);
        G_format_northing(cellhd->north, buffer, cellhd->proj);
	XmTextFieldSetString(data->north, buffer);
	return;
    }

    if ( !G_scan_northing(south, &cellhd->south, cellhd->proj)) {
	sprintf(buffer, "Illegal value for south edge: %s", south);
	XgWarningDialog(shell, buffer);
        G_format_northing(cellhd->south, buffer, cellhd->proj);
	XmTextFieldSetString(data->south, buffer);
	return;
    }

    if ( !G_scan_easting(east, &cellhd->east, cellhd->proj)) {
	sprintf(buffer, "Illegal value for east edge: %s", east);
	XgWarningDialog(shell, buffer);
        G_format_easting(cellhd->east, buffer, cellhd->proj);
	XmTextFieldSetString(data->east, buffer);
	return;
    }

    if ( !G_scan_easting(west, &cellhd->west, cellhd->proj)) {
	sprintf(buffer, "Illegal value for west edge: %s", west);
	XgWarningDialog(shell, buffer);
        G_format_easting(cellhd->west, buffer, cellhd->proj);
	XmTextFieldSetString(data->west, buffer);
	return;
    }
    nval = cellhd->north;
    sval = cellhd->south;
    eval = cellhd->east;
    wval = cellhd->west;
    nsval = cellhd->ns_res;
    ewval = cellhd->ew_res;

    if ( err = (char *)G_adjust_Cell_head(cellhd,1,1)) {
	XgWarningDialog(shell,err);
    }

    bigbuffer[0] = 0;
    sprintf(buffer,"projection: %d (%s)\n", cellhd->proj, 
	(char *)G__projection_name (cellhd->proj));
    strcat(bigbuffer, buffer);
    sprintf(buffer,"zone: %d\n\n", cellhd->zone);
    strcat(bigbuffer, buffer);

    G_format_northing (cellhd->north, buf,  cellhd->proj);
    G_format_northing (nval,         buf2, cellhd->proj);
    sprintf(buffer,"north: %s  ", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to match resolution)");
    }
    strcat(bigbuffer,"\n");

    G_format_northing (cellhd->south, buf,  cellhd->proj);
    G_format_northing (sval,         buf2, cellhd->proj);
    sprintf(buffer,"south: %s  ", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to match resolution)");
    }
    strcat(bigbuffer,"\n");

    G_format_easting (cellhd->east, buf,  cellhd->proj);
    G_format_easting (eval,         buf2, cellhd->proj);
    sprintf(buffer,"east: %s  ", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to match resolution)");
    }
    strcat(bigbuffer,"\n");

    G_format_easting (cellhd->west, buf,  cellhd->proj);
    G_format_easting (wval,         buf2, cellhd->proj);
    sprintf(buffer,"west: %s  ", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to match resolution)");
    }
    strcat(bigbuffer,"\n\n");

    G_format_resolution (cellhd->ns_res, buf,  cellhd->proj);
    G_format_easting (nsval,         buf2, cellhd->proj);
    sprintf(buffer,"ns_res: %s\n", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to conform to grid)");
    }
    strcat(bigbuffer,"\n");

    G_format_resolution (cellhd->ew_res, buf,  cellhd->proj);
    G_format_easting (ewval,         buf2, cellhd->proj);
    sprintf(buffer,"ew_res: %s\n", buf); 
    strcat(bigbuffer, buffer);
    if ( strcmp (buf, buf2) != 0) {
	strcat(bigbuffer,"  (Changed to conform to grid)");
    }
    strcat(bigbuffer,"\n");

    sprintf(buffer,"total rows: %15d\n", cellhd->rows); 
    strcat(bigbuffer, buffer);
    sprintf(buffer,"total cols: %15d\n", cellhd->cols); 
    strcat(bigbuffer, buffer);
    sprintf(buf,"%ld", (long) cellhd->rows * cellhd->cols);
    G_insert_commas(buf);
    sprintf(buffer,"total cells: %15s\n\n", buf); 
    strcat(bigbuffer, buffer);

    if (cellhd->north > def_wind.north) {
	strcat(bigbuffer, "warning - north falls outside the default region\n");
    }
    if (cellhd->south < def_wind.south) {
	strcat(bigbuffer, "warning - south falls outside the default region\n");
    }
    if (cellhd->proj != PROJECTION_LL) {
	if (cellhd->east > def_wind.east) {
	    strcat(bigbuffer, 
		"warning - east falls outside the default region\n");
	}
	if (cellhd->west < def_wind.west) {
	    strcat(bigbuffer, 
		"warning - west falls outside the default region\n\n");
	}
    }
    strcat(bigbuffer, 
	"Is this information acceptable? ");
    if ( !XgYesNo(shell,bigbuffer)) {
	cellhd->format = save.format;
	cellhd->compressed = save.compressed;
	cellhd->rows = save.rows;
	cellhd->cols = save.cols;
	cellhd->proj = save.proj;
	cellhd->zone = save.zone;
	cellhd->ew_res = save.ew_res;
	cellhd->ns_res = save.ns_res;
	cellhd->north = save.north;
	cellhd->south = save.south;
	cellhd->east = save.east;
	cellhd->west = save.west;
/*
        G_format_northing(cellhd->north, buffer, cellhd->proj);
	XmTextFieldSetString(data->north, buffer);
        G_format_northing(cellhd->south, buffer, cellhd->proj);
	XmTextFieldSetString(data->south, buffer);
        G_format_easting(cellhd->east, buffer, cellhd->proj);
	XmTextFieldSetString(data->east, buffer);
        G_format_easting(cellhd->west, buffer, cellhd->proj);
	XmTextFieldSetString(data->west, buffer);
*/
	return;
    }
}

XgCellhdVerify(shell, cellhd, data)
Widget shell;
struct Cell_head *cellhd;
EditCellhdData *data;
{
    XtAppContext    appContext;
    Widget widget;

    widget = (Widget)DoCellhdVerify(shell, cellhd, data);
    if ( widget == NULL ) return 0;
    appContext = XtWidgetToApplicationContext(shell);
    XgCellhdVerifyEventLoop(appContext, widget);
    return retval;

}

