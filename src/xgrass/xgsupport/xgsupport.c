#include "xgsupport.h"

struct {
    char                            progName[64];
    char                            map[256];
    char                            mapset[256];
    Boolean                         doHeader;
    Boolean                         doCats;
    Boolean                         doStats;
    Boolean                         doColor;
    Boolean                         doHistory;
}                               Global;

Display                        *display;
Widget                          mainshell;
Widget                          text;

XtAppContext                    appContext;


static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Support"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Usage(s)
    char                           *s;
{
    fprintf(stderr, "%s -help\n", s);
    fprintf(stderr, "%s [options] [raster file]\n", s);
    fprintf(stderr, "\t-header\n");
    fprintf(stderr, "\t-cats\n");
    fprintf(stderr, "\t-stats\n");
    fprintf(stderr, "\t-color\n");
    fprintf(stderr, "\t-history\n");
    fprintf(stderr, "Example:\n");
    fprintf(stderr, "\t%s map@%s\n", Global.progName, G_mapset());
}

char                           *
ParseCommand(argc, argv)
/* ARGSUSED */
    unsigned int                    argc;
    char                          **argv;
{
    strcpy(Global.progName, argv[0]);
    strcpy(Global.map, "");
    strcpy(Global.mapset, "");
    Global.doHeader = True;
    Global.doCats = True;
    Global.doStats = True;
    Global.doColor = True;
    Global.doHistory = True;

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if (!strncmp(*argv, "-", 1) || !strncmp(*argv, "+", 1)) {
            Boolean                         match = False;

            if (!strncmp(*argv, "+grid", 5)) {
                Global.doHeader = False;
                match = True;
            }
            if (!strncmp(*argv, "-help", 5)) {
                Usage(Global.progName);
                XFlush(display);
                exit(0);
            }
            if (!match) {
                fprintf(stderr, "unknown option \"%s\"\n", *argv);
                Usage(Global.progName);
                XFlush(display);
                exit(1);
            }
        } else {
            char                           *mapset;

            if ((mapset = G_find_file("cell", *argv, "")) == NULL) {
                char                            buf[80];

                /* should not EVER happen...but */
                fprintf(stderr, "no such map \"%s\"\n", *argv);
                Usage(Global.progName);
                exit(1);
            }
            switch (G__mapset_permissions(mapset)) {
            case 1:
                strcpy(Global.map, *argv);
                strcpy(Global.mapset, mapset);
                break;
            case 0:
                fprintf(stderr,
                  "You don't have write permission on mapset [%s]", mapset);
                Usage(Global.progName);
                exit(1);
            }
        }
        argv++;
    }
}

void
DoneCallBack(w)
    Widget                          w;
{
    XFlush(display);
    exit(0);
}

void
SupportTextCallBack(w, cld, cad)
    Widget                          w;
    XtPointer                       cld, cad;
{
    DoSetMapAndMapset();
}

DoSetMapAndMapset()
{
    char                           *string;
    char                           *mapset;
    char                            buf[80];

    XtVaGetValues(text, XmNvalue, &string, NULL);
    if ((mapset = G_find_file("cell", string, "")) == NULL) {
        XmTextFieldSetString(text, "[No Map Selected]");
        sprintf(buf, "No such map [%s]", string);
        XgWarningDialog(mainshell, buf);
        return 0;
    }
    switch (G__mapset_permissions(mapset)) {
    case 1:
        strcpy(Global.map, string);
        strcpy(Global.mapset, mapset);
        return 1;
    case 0:
        XmTextFieldSetString(text, "[No Map Selected]");
        sprintf(buf,
                "You don't have write permission on mapset [%s]", mapset);
        XgWarningDialog(mainshell, buf);
        return 0;
    }
}

void
SupportBrowserOkCallBack(w, cld, cad)
    Widget                          w;
    XtPointer                       cld, cad;
{
    Widget                          xgb = (Widget) cld;
    XmString                        res;
    char                           *string;
    char                           *mapset;

    XtVaGetValues(xgb, XmNresultString, &res, NULL);
    XmStringGetLtoR(res, XmSTRING_DEFAULT_CHARSET, &string);
    XtVaSetValues(text, XmNvalue, string, NULL);
    if (!DoSetMapAndMapset())
        return;
    XtUnmanageChild(xgb);
}

void 
DoHeader()
{
    char                           *mapset;
    char                           *name;
    unsigned char                   buffer[1024];
    struct Cell_head                cellhd;
    int                             cellhd_ok;
    int                             compressed_old;
    int                             compressed_new;
    int                             fd;
    long                            lseek(), filesize;
    long                            offset, prev_offset;
    int                             rows_old, rows_new;
    int                             nbytes;
    int                             quiet;
    char                            rname[40], rmapset[40];
    EditCellhdData                 *data;

    if (!DoSetMapAndMapset())
        return;
    name = Global.map;
    mapset = Global.mapset;
    if (G_is_reclass(name, mapset, rname, rmapset) > 0) {
        if (!strcmp(mapset, rmapset)) {
            sprintf(buffer,
                    "[%s] is a reclass of [%s]:\n\tcan't edit the header\n",
                    name, rname);
        } else {
            sprintf(buffer,
              "[%s] is a reclass of [%s in %s]:\n\tcan't edit the header\n",
                    name, rname, rmapset);
        }
        XgWarningDialog(mainshell, buffer);
        return;
    }
    /* open the cell file */
    fd = G_open_old("cell", name, mapset);
    if (fd < 0) {
        sprintf(buffer, "%s - can't open raster file\n", name);
        XgWarningDialog(mainshell, buffer);
        return;
    }
    /* determine file size */
    filesize = lseek(fd, 0L, 2);
    if (filesize == 0) {
        sprintf(buffer, "%s - empty raster file\n", name);
        XgWarningDialog(mainshell, buffer);
        return;
    }
    if (filesize < 0) {
        sprintf(buffer, "%s - error reading raster file\n", name);
        XgWarningDialog(mainshell, buffer);
        return;
    }
    cellhd_ok = G_get_cellhd(name, mapset, &cellhd) >= 0;
    if (!cellhd_ok) {
        G_zero(&cellhd, sizeof(cellhd));
        cellhd.proj = G_projection();
        cellhd.zone = G_zone();
    } else
        cellhd.format++;        /* set to number of bytes per cell (for now) */

    /*
     * Determine compression type, if any, without consulting cellhd
     * 
     * In a compressed file, there is an array of row addresses at the beginning
     * of the file.  Try to read the address array. If the file really is
     * compressed, the addresses will increase, the last one will be the same
     * as the filesize, and the number of row addresses will be one more than
     * the number of rows in the file.
     * 
     * If the file matches these conditions, it is probably compressed. The
     * probability of this being wrong is very small. So we will take a safe
     * route that doesn't annoy the user: If the cellhd wasn't valid, verify
     * the compression with the user. else if the cellhd says something
     * different, ask the user. else don't bother the user about it.
     * 
     * note: 3.0 address are in machine independent format pre 3.0 are true
     * longs
     */

    /* look for pre3.0 compression */
    compressed_old = 0;
    lseek(fd, 0L, 0);
    if (read(fd, buffer, 3) == 3 &&
            buffer[0] == 251 && buffer[1] == 255 && buffer[2] == 251) {
        rows_old = 0;
        offset = -1;
        while (next_row_addr(fd, &offset, 0)) {
            if (rows_old > 0 && offset <= prev_offset)
                break;
            if (offset >= filesize)
                break;
            prev_offset = offset;
            rows_old++;
        }
        if (offset == filesize)
            compressed_old = 1; /* it really is old format compressed */
    }
    /* look for 3.0 compression */
    compressed_new = 0;
    lseek(fd, 0L, 0);
    if (read(fd, buffer, 1) == 1 && buffer[0] > 0) {
        nbytes = buffer[0];
        rows_new = 0;
        offset = -1;
        while (next_row_addr(fd, &offset, nbytes)) {
            if (rows_new > 0 && offset <= prev_offset)
                break;
            if (offset >= filesize)
                break;
            prev_offset = offset;
            rows_new++;
        }
    }
    if (offset == filesize)
        compressed_new = 1;


    /*
     * now check these results against cellhd.compressed cellhd.compressed
     * values are -1 pre 3.0 cellhd - compression unknown (by cellhd alone) 0
     * not compressed (3.0) 1 compressed (3.0)
     */

    /*
     * printf ("cellhd compression: %d\n", cellhd.compressed); printf ("3.0
     * compression %sindicated\n", compressed_new?"":"not "); printf ("pre
     * 3.0 compression %sindicated\n", compressed_old?"":"not ");
     */

    /*
     * if we must create a brand new cell header, first find out if the file
     * is compressed?
     */
    if (!cellhd_ok) {
        sprintf(buffer, "[%s] appears to be compressed. Is it? ", name);
        cellhd.compressed = 0;
        if ((compressed_new || compressed_old) && 
	     XgPickOne(mainshell, buffer, "Yes", "No")) {
            if (compressed_new && compressed_old) {
                switch (XgPickOne(mainshell,
                                  "Please indicate the type of compression",
                                "3.0 compression", "Pre 3.0 compression")) {
                case '1':
                    compressed_new = 0;
                    break;
                case '2':
                    compressed_old = 0;
                    break;
                }
            }
            if (compressed_new) {
                cellhd.compressed = 1;
                cellhd.rows = rows_new;
            } else {
                cellhd.compressed = -1;
                cellhd.rows = rows_old;
            }
        }
    } else {
        if ((cellhd.compressed < 0) && !compressed_old)
            cellhd.compressed = 0;
        if ((cellhd.compressed == 0) && compressed_new) {
            char                            bigbuffer[1024];
            sprintf(bigbuffer, "\n***\n");
            sprintf(bigbuffer, 
		"The header for [%s] says the file is not compressed. ", name);
            sprintf(bigbuffer, "The file appears to be compressed.\n");
            sprintf(bigbuffer, 
		"Most likely the header is wrong, but I want you to decide.\n");
            sprintf(bigbuffer, "Is the file compressed? ");
            if (XgYesNo(mainshell, buffer)) {
                cellhd.compressed = 1;
            }
        } else if ((cellhd.compressed != 0) && !compressed_new) {
            char                            bigbuffer[1024];
            sprintf(bigbuffer, "\n*** WARNING ***\n\n");
            sprintf(bigbuffer,
                 "The header for [%s] says the file is compressed. ", name);
            sprintf(bigbuffer, "The file does NOT appear to be compressed.\n");
            sprintf(bigbuffer,
            "Most likely the header is wrong, but I want you to decide.\n");
            sprintf(bigbuffer, "Is the file really compressed? ");
            if (!XgYesNo(mainshell, bigbuffer)) {
                cellhd.compressed = 0;
            }
        }
    }

    if ((cellhd.compressed < 0 && rows_old != cellhd.rows)
            || (cellhd.compressed > 0 && rows_new != cellhd.rows)) {
        int                             rows;
        char                            bigbuffer[1024];

        rows = (cellhd.compressed > 0 ? rows_new : rows_old);
        sprintf(bigbuffer, "\n*** WARNING ***\n");
        sprintf(bigbuffer, "Header indicates %d row%s in the cell file, but\n",
                cellhd.rows, cellhd.rows == 1 ? "" : "s");
        sprintf(bigbuffer, "the actual file format indicates %d row%s\n",
                rows, rows == 1 ? "" : "s");
        sprintf(bigbuffer, "Should this discrepancy be corrected? ");
        if (XgYesNo(mainshell, bigbuffer))
            cellhd.rows = rows;
    }

    if (!XgDoAskFormat(mainshell, &cellhd, filesize))
        return;

    if ((data = XgEditCellhd(mainshell, &cellhd, name, mapset)) == NULL)
        return;

    if (!XgCellhdVerify(mainshell, &cellhd, data)) {
	return;
    }

    /* adjust from nbytes to nbytes-1 */
    if (cellhd.format > 0)
        cellhd.format--;

    /* Write new header out */
    if (G_put_cellhd(name, &cellhd) == -1) {
        sprintf(buffer, "unable to write header for %s", name);
        XgWarningDialog(mainshell, buffer);
        return;

    }
}

void 
DoCats()
{
    char                            buf[1024];

    if (!DoSetMapAndMapset())
        return;
    sprintf(buf, "xgcats %s@%s", Global.map, Global.mapset);
    XgSystem(mainshell, buf, False, NULL, 0);
}

void 
DoStats()
{
    struct Histogram                histogram;
    struct Categories               cats;
    struct Range                    range;
    char                           *name;
    char                           *mapset;
    int                             i;
    int                             cats_ok;

    if (!DoSetMapAndMapset())
        return;

    mapset = Global.mapset;
    name = Global.map;

    if (!DoHistogram()) {
        return;
    }
    if (G_read_histogram(name, mapset, &histogram) <= 0) {
        return;
    }
    G_init_range(&range);
    i = G_get_histogram_num(&histogram);
    while (i >= 0)
        G_update_range(G_get_histogram_cat(i--, &histogram), &range);
    G_write_range(name, &range);

    cats_ok = G_read_cats(name, mapset, &cats) >= 0;
    if (!cats_ok)
        G_init_cats(range.pmax, "", &cats);
    else if (cats.num != range.pmax) {
        cats.num = range.pmax;
        cats_ok = 0;
    }
    if (!cats_ok) {
        G_write_cats(name, &cats);
    }
    G_free_histogram(&histogram);
    G_free_cats(&cats);
    return;
}

DoHistogram()
{
    char                           *name = Global.map;
    char                           *mapset = Global.mapset;
    CELL                           *cell;
    struct Cell_head                cellhd;
    struct Cell_stats               statf;
    int                             nrows, ncols, row;
    int                             fd;

    if (G_get_cellhd(name, mapset, &cellhd) < 0)
        return 0;
    G_set_window(&cellhd);
    fd = G_open_cell_old(name, mapset);
    if (fd < 0)
        return 0;
    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();

    G_init_cell_stats(&statf);
    for (row = 0; row < nrows; row++) {
        if (G_get_map_row_nomask(fd, cell, row) < 0)
            break;
        G_update_cell_stats(cell, ncols, &statf);
    }
    G_close_cell(fd);
    free(cell);
    if (row == nrows)
        G_write_histogram_cs(name, &statf);
    G_free_cell_stats(&statf);
    return row == nrows;
}

void 
DoColor()
{
    if (!DoSetMapAndMapset())
        return;
    printf("DoColor\n");
}

void 
DoHistory()
{
    char                            buf[1024];

    if (!DoSetMapAndMapset())
        return;
    sprintf(buf, "xghistory %s@%s", Global.map, Global.mapset);
    XgSystem(mainshell, buf, False, NULL, 0);
}

void 
DoBrowser()
{
    Arg                             al[10];
    int                             ac = 0;
    XmString                        xms, xms2;
    Widget                          xgb;

    xms = XmStringCreateSimple("Please select a raster map");
    xms2 = XmStringCreateSimple(G_mapset());

    XtSetArg(al[ac], XmNinitialMapset1, xms2);
    ac++;
    XtSetArg(al[ac], XmNnumLists, 1);
    ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_RASTER);
    ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNpromptString, xms);
    ac++;
    xgb = XgCreateBrowserDialog(mainshell, "Raster Browser", al, ac);
    XtManageChild(xgb);
    XtAddCallback(xgb, XmNokCallback, SupportBrowserOkCallBack, 
		  (XtPointer) xgb);
    XmStringFree(xms);
}

int
main(argc, argv)
    unsigned int                    argc;
    char                          **argv;
{
    Widget                          shell;

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    G_gisinit(argv[0]);
    mainshell = shell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    ParseCommand(argc, argv);

    DoMainMenu();

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}

DoMainMenu()
{
    Widget                          xgi;
    Widget                          form;
    Widget                          rc;
    Widget                          bform;
    Widget                          last;
    Widget                          sep;
    Widget                          button;
    Screen                         *scrptr;
    Pixmap                          pixmap;
    Pixel                           fg, bg;
    char                            buf[512];
    Arg al[16];
    int ac = 0;

    scrptr = XtScreen(mainshell);

    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Done")); ac++;

    xgi = XgCreateInteractor(mainshell, "Raster Map Support", al, ac);
    XtUnmanageChild(XgInteractorGetChild(xgi,XmINTERACT_CANCEL_BUTTON));
    XtManageChild(xgi);
    XtAddCallback(xgi, XmNokCallback, DoneCallBack, xgi);

    form = XtVaCreateManagedWidget("form",
                                 xmFormWidgetClass, xgi,
                                 NULL);

    bform = XtVaCreateManagedWidget("form",
                                 xmFormWidgetClass, form,
                                 NULL);

    if (Global.map[0] == NULL) {
        sprintf(buf, "[No Map Selected]");
    } else {
        sprintf(buf, "%s@%s", Global.map, Global.mapset);
    }

    XtVaGetValues(bform, XmNforeground, &fg, XmNbackground, &bg, NULL);

    pixmap = XCreatePixmapFromBitmapData(display,
                                 RootWindowOfScreen(scrptr), raster_bm_bits,
                                         raster_bm_width, raster_bm_height,
                                         fg, bg,
                                         DefaultDepthOfScreen(scrptr));

    button = XtVaCreateManagedWidget("Select Raster Map",
                                     xmPushButtonWidgetClass, bform,
                                     XmNlabelType, XmPIXMAP,
                                     XmNlabelPixmap, pixmap, 
                                     XmNtopAttachment, XmATTACH_FORM,
                                     XmNleftAttachment, XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     NULL);

    XtAddCallback(button, XmNactivateCallback, DoBrowser, NULL);

    text = XtVaCreateManagedWidget("text",
                           xmTextFieldWidgetClass, bform, XmNvalue, buf, 
                                     XmNtopAttachment, XmATTACH_FORM,
                                     XmNleftAttachment, XmATTACH_WIDGET,
                                     XmNleftWidget, button,
                                     XmNrightAttachment, XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     NULL);

    XtAddCallback(text, XmNactivateCallback, SupportTextCallBack, NULL);

    sep = XtVaCreateManagedWidget("sep",
                                     xmSeparatorWidgetClass, form,
                                     XmNtopAttachment, XmATTACH_WIDGET,
                                     XmNtopWidget, bform,
                                     XmNleftAttachment, XmATTACH_FORM,
                                     XmNrightAttachment, XmATTACH_FORM,
				     NULL);

    rc = XtVaCreateManagedWidget("row_col", xmRowColumnWidgetClass,
                                     form, 
                                     XmNentryAlignment, XmALIGNMENT_CENTER,
                                     XmNisAligned, True,
                                     XmNorientation, XmHORIZONTAL,
                                     XmNpacking, XmPACK_COLUMN,
                                     XmNnumColumns, 3,
                                     XmNtopAttachment, XmATTACH_WIDGET,
                                     XmNtopWidget, sep,
                                     XmNrightAttachment, XmATTACH_FORM,
                                     XmNleftAttachment, XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_FORM,
				     NULL);
                                     

    if (Global.doHeader) {
        button = XtVaCreateManagedWidget("Edit Header...",
                                        xmPushButtonWidgetClass, rc,
					NULL);
        XtAddCallback(button, XmNactivateCallback, DoHeader, NULL);
    }
    if (Global.doCats) {
	    button = XtVaCreateManagedWidget("Edit Categories...",
					     xmPushButtonWidgetClass, rc,
					NULL);
        XtAddCallback(button, XmNactivateCallback, DoCats, NULL);
    }
    if (Global.doHistory) {
        button = XtVaCreateManagedWidget("Edit History...",
                                         xmPushButtonWidgetClass, rc,
					NULL);
        XtAddCallback(button, XmNactivateCallback, DoHistory, NULL);
    }
    if (Global.doColor) {
        button = XtVaCreateManagedWidget("Edit/Assign Color Table...",
                                         xmPushButtonWidgetClass, rc,
					NULL);
        XtSetSensitive(button, False);
        XtAddCallback(button, XmNactivateCallback, DoColor, NULL);
    }
    if (Global.doStats) {
        button = XtVaCreateManagedWidget("Update Histogram/Range",
                                         xmPushButtonWidgetClass, rc,
					NULL);
        XtAddCallback(button, XmNactivateCallback, DoStats, NULL);
    }
}

next_row_addr(fd, offset, nbytes)
    long                           *offset;
{
    unsigned char                   buf[256];
    int                             i;

    /* nbytes <=0 means pre 3.0 compression */
    if (nbytes <= 0)
        return (read(fd, offset, sizeof(*offset)) == sizeof(*offset));

    /* 3.0 compression */
    if (read(fd, buf, nbytes) != nbytes)
        return 0;
    *offset = 0;
    for (i = 0; i < nbytes; i++)
        *offset = *offset * 256 + buf[i];
    return 1;
}
