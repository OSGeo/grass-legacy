#include "xgrass.h"
#include "Caption.h"
#include "sgi.h"

extern char    *version;

Boolean         fullExit;
char            DatabaseFile[256];
int             DatabaseWritable;
char          **databaseList;
int             databaseCount;
Boolean         databaseNewer;
Boolean         databaseSystem;

char            theLocation[256];
char            theMapset[256];
char            theDatabase[256];
char            theSession[256];

XtAppContext    appContext;

Widget          xgi;
Widget          theShell;
Widget          listContainer;
Widget          listContainer2;
Widget          mainForm;
Widget          theFrame;
Widget          sessionFrame, dbFrame, locationFrame, mapsetFrame;
Widget          sessionCaption, dbCaption, locationCaption, mapsetCaption;
Widget          sessionList, dbList, locationList, mapsetList;

/* this stuff for loc dialog */

Widget          locDialog;
Widget          locForm;
Widget          locRadios;
Widget          LocSetupToggles[16];
int             CountLocSetupToggles;
char           *G__projection_name();
Widget          locZoneText;
Widget          locZoneLabel;

/* this stuff for the Dave Gerdes dialog */

Widget          gerdesDialog;
Widget          gerdesContainer;
Widget          gerdesSessionLabel;
Widget          gerdesDBLabel;
Widget          gerdesLocationLabel;
Widget          gerdesMapsetLabel;
Widget          gerdesSessionText;
Widget          gerdesDBText;
Widget          gerdesLocationText;
Widget          gerdesMapsetText;
Widget          gerdesSessionButton;
Widget          gerdesDBButton;
Widget          gerdesLocationButton;
Widget          gerdesMapsetButton;
Widget          gerdesSessionBox;
Widget          gerdesDBBox;
Widget          gerdesLocationBox;
Widget          gerdesMapsetBox;

static void     ReadCurrentValues();
#ifdef _NO_PROTO
static void WriteDatabaseList();
#else
static void WriteDatabaseList(void);
#endif

Boolean         make_location();
static	void            UpdateAllLists();
static	Boolean         DoCheckMapset();

static void
ChangeDatabase(s)
    char           *s;
{
    if (strcmp(theDatabase, s)) {
        InitValues();
        strcpy(theDatabase, s);
        G__setenv("GISDBASE", theDatabase);
        UpdateAllLists();
    }
}

static void
ChangeLocation(s)
    char           *s;
{
    if (strcmp(theLocation, s)) {
        *theSession = 0;
        *theMapset = 0;
        strcpy(theLocation, s);
        G__setenv("LOCATION_NAME", theLocation);
        UpdateAllLists();
    }
}

static void
ChangeMapset(s)
    char           *s;
{
    if (strcmp(theMapset, s)) {
        strcpy(theMapset, s);
        G__setenv("MAPSET", theMapset);
        *theSession = 0;
        UpdateAllLists();
    }
}

/* Loc dialog stuff */

static void
DoCreateSession(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;
    char            file[256];
    char            buf[256];
    char           *home = (char *) getenv("HOME");
    FILE           *fp;
    s = XmTextGetString(gerdesSessionText);
    if (!*s) {
        XgWarningDialog(w, "You must enter a session name");
        return;
    }
    if ( *theDatabase == '\0' ) {
        XgWarningDialog(w, "Database not set! Please set it and try again.");
        return;
    }
    if ( *theLocation == '\0' ) {
        XgWarningDialog(w, "Location not set! Please set it and try again.");
        return;
    }
    if ( *theMapset == '\0' ) {
        XgWarningDialog(w, "Mapset not set! Please set it and try again.");
        return;
    }
    if (!DoCheckMapset(w, data, cbs))
        return;
    strcpy(theSession, s);
    sprintf(file, "%s/.xgrass/session/%s", (char *) getenv("HOME"), theSession);
    if ((fp = fopen(file, "w")) != NULL) {
        fprintf(fp, "GISDBASE: %s\n", theDatabase);
        fprintf(fp, "LOCATION_NAME: %s\n", theLocation);
        fprintf(fp, "MAPSET: %s\n", theMapset);
        fclose(fp);
        if (s != NULL && *s != '\0') {
            if (access(file, F_OK) == -1) {
                fprintf(stderr, "No such session:%s", s);
                exit(1);
            } else {
#if defined(mips) || defined(BSD)
                sprintf(buf, "%s/.xgrass/session/%s", home, s);
                setenv("GISRC", _XgStrDup(buf), 1);
#else
                sprintf(buf, "GISRC=%s/.xgrass/session/%s", home, s);
                putenv(_XgStrDup(buf));
#endif
                sprintf(buf, "%s/.xgrass/session/%s", home, s);
                G__setenv("GISRC", _XgStrDup(buf));
            }
        } else {
            return;
        }
        sprintf(file, "%s/.xgrass/histories/%s", (char *) getenv("HOME"), s);
        __XgHistorySaveToFile(file);

        ReadCurrentValues();
        UpdateAllLists();
    } else {

        sprintf(buf, "Couldn't save to session \"%s\"", theSession);
        XgWarningDialog(w, buf);
        return;
    }
}

static          Boolean
DoCheckDatabase(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;

    if ( gerdesDBText && XtIsManaged(gerdesDBText) ) {
	s = XmTextGetString(gerdesDBText);
	if (*s != '/') {
	    XgWarningDialog(xgi, "A database must be an absolute path");
	    XmTextSetString(gerdesDBText, theDatabase);
	    return False;
	}
	if (access(s, 0) != 0) {
	    XgWarningDialog(xgi, "That database does not exist.");
	    XmTextSetString(gerdesDBText, theDatabase);
	    return False;
	}
	if (_XgStringArraySearch(databaseList, databaseCount, s) != -1) {
	    ChangeDatabase(s);
	    return True;
	} else {
	    XgWarningDialog(xgi, "That database is not in the list.");
	    XmTextSetString(gerdesDBText, theDatabase);
	    return False;
	}
    } else
        return True;
}

static void
DoAddDatabase(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;
    s = XmTextGetString(gerdesDBText);
    if (*s != '/') {
        XgWarningDialog(xgi, "A database must be an absolute path");
        XmTextSetString(gerdesDBText, theDatabase);
        return;
    }
    if (access(s, 0) != 0) {
        XgWarningDialog(xgi, "That database does not exist.");
        XmTextSetString(gerdesDBText, theDatabase);
        return;
    }
    if (_XgStringArraySearch(databaseList, databaseCount, s) != -1) {
        if (data)
            XgWarningDialog(xgi, "That database is already in the list.");
        else {
            ChangeDatabase(s);
        }
        return;
    } else {
        if (DatabaseWritable) {
            extern char   **_XgStringArrayAdd();
            if (_XgStringArraySearch(databaseList, databaseCount, s) == -1) {
                databaseList = _XgStringArrayAdd(databaseList, databaseCount, s);
                databaseCount++;
                ChangeDatabase(s);
                WriteDatabaseList();
            }
            return;
        } else {
            XgWarningDialog(xgi, "That database exists, but is not in the database list.Unfortunately, the database list is not writable by you.See your system administrator.");
            XmTextSetString(gerdesDBText, theDatabase);
            return;
        }
    }
}

static          Boolean
DoCheckLocation(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;
    char          **dir;
    int             dircount;
    char            path[256];
    Boolean         result;
    if (!DoCheckDatabase(w, data, cbs))
        return False;
    if ( gerdesLocationText && XtIsManaged(gerdesLocationText) ) {
	s = XmTextGetString(gerdesLocationText);
	if (G_legal_filename(s) < 0) {
	    XgWarningDialog(xgi, "Illegal location name");
	    XmTextSetString(gerdesLocationText, theLocation);
	    return;
	}
	sprintf(path, "%s", theDatabase);
	dir = _XgDirectoryListing(path, &dircount, False, 0, 0);
	result = (_XgStringArraySearch(dir, dircount, s) != -1);
	if (!result) {
	    XgWarningDialog(w, "That location does not exist");
	    XmTextSetString(gerdesLocationText, theLocation);
	} else {
	    ChangeLocation(s);
	}
    } else
        return True;
}

static void
DoCreateLocation(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;
    char          **dir;
    int             dircount;
    char            path[256];
    Boolean         result;
    if (!DoCheckDatabase(w, data, cbs))
        return;
    s = XmTextGetString(gerdesLocationText);
    if (G_legal_filename(s) < 0) {
        XgWarningDialog(xgi, "Illegal location name");
        XmTextSetString(gerdesLocationText, theLocation);
        return;
    }
    sprintf(path, "%s", theDatabase);
    dir = _XgDirectoryListing(path, &dircount, False, 0, 0);
    result = (_XgStringArraySearch(dir, dircount, s) != -1);
    G__setenv("GISDBASE", theDatabase);
    G__setenv("LOCATION_NAME", theLocation);
    if (!result) {
        make_location(s);
    } else {
        if (data)
            XgWarningDialog(w, "That location already exists.");
        else {
            ChangeLocation(s);
        }
    }
}

static          Boolean
DoCheckMapset(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char          **dir;
    int             dircount;
    char            path[256];
    Boolean         result;
    char           *s;
    if (!DoCheckDatabase(w, data, cbs))
        return False;
    if (!DoCheckLocation(w, data, cbs))
        return False;
    s = XmTextGetString(gerdesMapsetText);
    if (G_legal_filename(s) < 0) {
        XgWarningDialog(xgi, "Illegal mapset name");
        XmTextSetString(gerdesMapsetText, theMapset);
        return;
    }
    sprintf(path, "%s/%s", theDatabase, theLocation);
    dir = _XgDirectoryListing(path, &dircount, False, 0, 0);
    result = (_XgStringArraySearch(dir, dircount, s) != -1);
    if (!result) {
        XgWarningDialog(w, "That mapset does not exist");
        XmTextSetString(gerdesMapsetText, theMapset);
    } else {
        ChangeMapset(s);
    }
}

static void
MakeFirstMapset()
{
    char           *me = G_whoami();
    if (!DoCheckDatabase(xgi, NULL, NULL))
        return;
    if (!DoCheckLocation(xgi, NULL, NULL))
        return;
    make_mapset(theDatabase, theLocation, me);
}

static void
DoCreateMapset(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char          **dir;
    int             dircount;
    char            path[256];
    Boolean         result;
    char           *s;
    if (!DoCheckDatabase(w, data, cbs))
        return;
    if (!DoCheckLocation(w, data, cbs))
        return;
    s = XmTextGetString(gerdesMapsetText);
    if (G_legal_filename(s) < 0) {
        XgWarningDialog(xgi, "Illegal mapset name");
        XmTextSetString(gerdesMapsetText, theMapset);
        return;
    }
    sprintf(path, "%s/%s", theDatabase, theLocation);
    dir = _XgDirectoryListing(path, &dircount, False, 0, 0);
    result = (_XgStringArraySearch(dir, dircount, s) != -1);
    if (result) {
        if (data)
            XgWarningDialog(w, "That mapset already exists");
        else {
            G__setenv("GISDBASE", theDatabase);
            G__setenv("LOCATION_NAME", theLocation);
            if (G__mapset_permissions(s) != 1) {
                XgWarningDialog(xgi, "That mapset already exists, and you do not have permission for it.");
            } else {
                ChangeMapset(s);
            }
        }
    } else {
        make_mapset(theDatabase, theLocation, s);
    }
}

static void
#ifdef _NO_PROTO
ZoneTextVerifyCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
ZoneTextVerifyCallBack(
                       Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    Widget          xgr = (Widget) client_data;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;
    int             i;

    for (i = 0; i < cbs->text->length; i++) {
        /* disallow non-digits, but allow a '-' in position 0 */
        if (!isdigit(*(cbs->text->ptr + i))) {
            cbs->doit = False;
            XBell(XtDisplay(xgr), 0);
            return;
        }
    }
    cbs->doit = True;
}

static struct Cell_head locDefaultRegion;

static char    *location_name;

static void
LocDialogOk(w, data)
    Widget          w;
    XtPointer       data;
{
    /* Create Location */
    char           *mapset;
    char            buf[1024];
    char            myname[75];
    FILE           *fd;

    mapset = "PERMANENT";
    G__setenv("MAPSET", mapset);
    G__setenv("LOCATION_NAME", location_name);

    sprintf(buf, "mkdir %s/%s", theDatabase, location_name);
    if (system(buf))
        return;
    sprintf(buf, "%s/%s", theDatabase, location_name);
#ifdef mips
    chmod(buf, 0777);
#else
    chmod(buf, S_IWUSR | S_IRUSR | S_IXUSR | S_IWGRP | S_IRGRP | S_IXGRP | S_IWOTH | S_IROTH | S_IXOTH);
#endif
    sprintf(buf, "mkdir %s/%s/%s", theDatabase, location_name, mapset);
    if (system(buf))
        return;

    sprintf(buf, "%s/%s/%s/DEFAULT_WIND", theDatabase, location_name, mapset);
    fd = fopen(buf, "w");

    G__write_Cell_head(fd, &locDefaultRegion, 0);
    fclose(fd);

    sprintf(buf, "%s/%s/%s/WIND", theDatabase, location_name, mapset);
    fd = fopen(buf, "w");

    G__write_Cell_head(fd, &locDefaultRegion, 0);
    fclose(fd);

    sprintf(buf, "echo '%s' >  %s/%s/%s/MYNAME", myname, theDatabase, location_name, mapset);
    system(buf);
    /* Put something here to update the current loc */
    ChangeLocation(location_name);
    XtDestroyWidget(w);
}

static void
LocDialogCancel(w, data)
    Widget          w;
    XtPointer       data;
{

    XBell(_XG_Global.display, 73);
    XmTextSetString(gerdesLocationText, theLocation);
}

static void
LocationSetupToggle(w, data)
    Widget          w;
    XtPointer       data;
{
    int             i;
    int             chose = (int) data;
    for (i = 0; i < CountLocSetupToggles; i++) {
        if (i != chose)
            XmToggleButtonSetState(LocSetupToggles[i], False, False);
    }
    XmToggleButtonSetState(LocSetupToggles[chose], True, False);
    if ((chose == 0) || (chose == PROJECTION_LL)) {
        if (locZoneText && locZoneLabel) {
            XtSetSensitive(locZoneText, False);
            XtSetSensitive(locZoneLabel, False);
        }
    } else {
        if (locZoneText && locZoneLabel) {
            XtSetSensitive(locZoneText, True);
            XtSetSensitive(locZoneLabel, True);
        }
    }
}

static void
RegionCancelCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    XtDestroyWidget(w);
}

static void
RegionOkCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    XtDestroyWidget(w);
}

static void
SetDefaultRegion(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    Widget          shell = (Widget) data;
    Arg             al[10];
    int             ac = 0;
    Widget          xgr;
    XtAppContext    appContext;

    XtSetArg(al[ac], XmNpromptLabelString, XmStringCreateSimple("Please specify the default region for your new location"));
    ac++;
    XtSetArg(al[ac], XmNgrid, 0);
    ac++;
    XtSetArg(al[ac], XmNeditDefaultRegion, True);
    ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNcurrentRegion, &locDefaultRegion);
    ac++;
    xgr = XgCreateRegionDialog(shell, "TESTIT", al, ac);
    XtManageChild(XgInteractorGetChild(xgr, XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgr, XmNokCallback, RegionOkCallback, (XtPointer) shell);
    XtAddCallback(xgr, XmNcancelCallback, RegionCancelCallback, (XtPointer) shell);
    XtManageChild(xgr);

    appContext = XtWidgetToApplicationContext(xgr);
}

Boolean
MakeLocationDialog(w)
    Widget          w;
{
    Arg             al[16];
    int             ac = 0;
    int             i;
    char           *name;
    Widget          aToggle;
    Widget          descLabel;
    Widget          coordLabel;
    Widget          descText;
    Widget          regionButton;
    XtAppContext    appContext;

    XtSetArg(al[ac], XmNenableWorkAreaStretch, False);
    ac++;
    locDialog = XgCreateInteractorDialog(w, "New Location Information", al, ac);

    XtAddCallback(locDialog, XmNokCallback, LocDialogOk, locDialog);
    XtAddCallback(locDialog, XmNcancelCallback, LocDialogCancel, locDialog);

    XtVaSetValues(locDialog, XmNpromptLabelString, XmStringCreateSimple("Location Setup"), NULL);

    locForm = XtVaCreateManagedWidget("xgdbset_form", xmRowColumnWidgetClass, locDialog,
                                      XmNorientation, XmVERTICAL,
                                      XmNpacking, XmPACK_TIGHT,
                                      NULL);

    coordLabel = XtVaCreateManagedWidget("xgdbset_locationsetup_zonelabel", xmLabelWidgetClass, locForm,
                  XmNlabelString, XmStringCreateSimple("Coordinate System"),
                                         XmNtraversalOn, False,
                                         NULL);

    locRadios = XtVaCreateManagedWidget("xgdbset_radios", xmRowColumnWidgetClass, locForm,
                                        XmNleftAttachment, XmATTACH_FORM,
                                        XmNrightAttachment, XmATTACH_FORM,
                                        XmNtopAttachment, XmATTACH_FORM,
                                        NULL);

    for (i = 0; name = G__projection_name(i); i++) {
        aToggle = LocSetupToggles[i] = XtVaCreateManagedWidget("xgdbset_location_toggle", xmToggleButtonWidgetClass, locRadios,
                                            XmNindicatorType, XmONE_OF_MANY,
                                 XmNlabelString, XmStringCreateSimple(name),
                                                               NULL);
        XgAddHelpCallBackFromFile(aToggle, "xgdbset_locationsetup_toggle");
        XtAddCallback(aToggle, XmNvalueChangedCallback, LocationSetupToggle, i);
    }
    CountLocSetupToggles = i;

    locZoneLabel = XtVaCreateManagedWidget("xgdbset_locationsetup_zonelabel", xmLabelWidgetClass, locForm,
                                           XmNtraversalOn, False,
                        XmNlabelString, XmStringCreateSimple("Zone number"),
                                           NULL);

    locZoneText = XtVaCreateManagedWidget("xgdbset_locationsetup_zone", xmTextFieldWidgetClass, locForm,
                                          XmNleftAttachment, XmATTACH_FORM,
                                          XmNrightAttachment, XmATTACH_FORM,
                                          XmNtopAttachment, XmATTACH_WIDGET,
                                          XmNtopWidget, locRadios,
                                          NULL);
    XtAddCallback(locZoneText, XmNmodifyVerifyCallback, ZoneTextVerifyCallBack, locDialog);

    descLabel = XtVaCreateManagedWidget("xgdbset_locationsetup_labellabel", xmLabelWidgetClass, locForm,
                                        XmNtraversalOn, False,
                        XmNlabelString, XmStringCreateSimple("Description"),
                                        NULL);

    descText = XtVaCreateManagedWidget("xgdbset_locationsetup_desc", xmTextFieldWidgetClass, locForm,
                                       XmNeditable, True,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNtopAttachment, XmATTACH_WIDGET,
                                       XmNtopWidget, locZoneText,
                                       NULL);
    regionButton = XtVaCreateManagedWidget("xgdbset_locationsetup_region", xmPushButtonWidgetClass, locForm,
              XmNlabelString, XmStringCreateSimple("Set Default Region..."),
                                           XmNleftAttachment, XmATTACH_FORM,
                                           XmNrightAttachment, XmATTACH_FORM,
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, descText,
                                           NULL);
    XtAddCallback(regionButton, XmNactivateCallback, SetDefaultRegion, locDialog);

    LocationSetupToggle(locDialog, 0);
    XtManageChild(locDialog);
}

Boolean
make_location(newloc)
    char           *newloc;
{
    char            title[75];
    char            buf[1024];
    int             i, t, n, s, e, p, r;
    char            myname[75];
    char           *mapset;
    Widget          dialog;
    int             stat;
    char           *name;
    char            buf3[4096];
    char            buf2[256];

    location_name = newloc;
    sprintf(buf3, "To create a new LOCATION, you will need the following information:\n\n1. The coordinate system for the database\n");
    name = G__projection_name(0);
    sprintf(buf2, "        %s (for imagery and other unreferenced data)\n", name);
    strcat(buf3, buf2);
    for (i = 1; name = G__projection_name(i); i++) {
        sprintf(buf2, "        %s\n", name);
        strcat(buf3, buf2);
    }
    strcat(buf3, "2. The zone for the database\n");
    sprintf(buf2, "       (except for %s", G__projection_name(0));
    strcat(buf3, buf2);
    sprintf(buf2, " and %s databases)\n", G__projection_name(PROJECTION_LL));
    strcat(buf3, buf2);
    strcat(buf3, "3. The coordinates of the area to become the default region\n");
    strcat(buf3, "   and the grid resolution of this region\n");
    strcat(buf3, "4. A short, one-line description or title for the location\n");
    strcat(buf3, "\n");

    sprintf(buf2, "Do you have all this information for location <%s> ? ", location_name);
    strcat(buf3, buf2);
    if (!XgYesNo(xgi, buf3))
        return False;

    G_zero(&locDefaultRegion, sizeof(locDefaultRegion));

    locDefaultRegion.north = 10000;
    locDefaultRegion.south = 1;
    locDefaultRegion.east = 10000;
    locDefaultRegion.west = 1;
    locDefaultRegion.proj = 1;
    locDefaultRegion.zone = 1;
    locDefaultRegion.ew_res = 100;
    locDefaultRegion.ns_res = 100;

    G_adjust_Cell_head(&locDefaultRegion, 0, 0);

    MakeLocationDialog(xgi);
    return True;
}

int
make_mapset(database, location, mapset)
    char           *database;
    char           *location;
    char           *mapset;
{
    char            buffer[256];

    /* create the mapset directory */
    sprintf(buffer, "mkdir %s/%s/%s", database, location, mapset);
    system(buffer);

    /* give the mapset a default region for the entire location */
    sprintf(buffer, "cat %s/%s/PERMANENT/DEFAULT_WIND  > %s/%s/%s/WIND",
            database, location, database, location, mapset);
    system(buffer);

    ChangeMapset(mapset);
    return (0);
}

/* Gerdes dialog stuff */

static void
GerdesDialogOk(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    DoCreateMapset(gerdesDialog, NULL, NULL);
    gerdesSessionText = NULL;
    gerdesDBText = NULL;
    gerdesLocationText = NULL;
    gerdesMapsetText = NULL;

    XtDestroyWidget(w);
}

static void
DoGerdesDialog(w)
    Widget          w;
{
    Arg             al[16];
    int             ac = 0;
    int             i;

    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Done"));
    ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, False);
    ac++;
    gerdesDialog = XgCreateInteractorDialog(w, "Add Items", al, ac);

    XtAddCallback(gerdesDialog, XmNokCallback, GerdesDialogOk, gerdesDialog);
    XtUnmanageChild(XgInteractorGetChild(gerdesDialog, XmINTERACT_CANCEL_BUTTON));
    gerdesContainer = XtVaCreateManagedWidget("xgdbset_gerdes_rc", xmFormWidgetClass, gerdesDialog,
                                              NULL);

    gerdesSessionLabel = XtVaCreateManagedWidget("xgdbset_gerdes_session_caption", xbaeCaptionWidgetClass, gerdesContainer,
                                          XmNrightAttachment, XmATTACH_FORM,
                                            XmNtopAttachment, XmATTACH_FORM,
                                         XmNlabelPosition, XbaePositionLeft,
                            XmNlabelString, XmStringCreateSimple("Session"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                                 XmNtraversalOn, False,
                                                 NULL);

    gerdesSessionBox = XtVaCreateManagedWidget("xgdbset_gerdes_rc", xmRowColumnWidgetClass, gerdesSessionLabel,
                                               XmNorientation, XmHORIZONTAL,
           XmNpacking, XmPACK_COLUMN, XmNentryAlignment, XmALIGNMENT_CENTER,
                                               NULL);

    gerdesSessionText = XtVaCreateManagedWidget("xgdbset_gerdes_session_text", xmTextFieldWidgetClass, gerdesSessionBox,
                                                XmNcolumns, 15,
                                                XmNvalue, theSession,
                                                NULL);
    XgAddHelpCallBackFromFile(gerdesSessionText, "xgd_ses_text");
    XtAddCallback(gerdesSessionText, XmNactivateCallback, DoCreateSession, NULL);

    gerdesSessionButton = XtVaCreateManagedWidget("xgdbset_gerdes_session_button", xmPushButtonWidgetClass, gerdesSessionBox,
                     XmNlabelString, XmStringCreateSimple("Create Session"),
                                                  NULL);
    XgAddHelpCallBackFromFile(gerdesSessionButton, "xgd_ses_button");
    XtAddCallback(gerdesSessionButton, XmNactivateCallback, DoCreateSession, gerdesDialog);

    gerdesDBLabel = XtVaCreateManagedWidget("xgdbset_gerdes_session_caption", xbaeCaptionWidgetClass, gerdesContainer,
                                          XmNrightAttachment, XmATTACH_FORM,
                                          XmNtopAttachment, XmATTACH_WIDGET,
                                            XmNtopWidget, gerdesSessionLabel,
                                         XmNlabelPosition, XbaePositionLeft,
                           XmNlabelString, XmStringCreateSimple("Database"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                            XmNtraversalOn, False,
                                            NULL);

    gerdesDBBox = XtVaCreateManagedWidget("xgdbset_gerdes_rc", xmRowColumnWidgetClass, gerdesDBLabel,
                                          XmNorientation, XmHORIZONTAL,
           XmNpacking, XmPACK_COLUMN, XmNentryAlignment, XmALIGNMENT_CENTER,
                                          NULL);

    gerdesDBText = XtVaCreateManagedWidget("xgdbset_gerdes_session_text", xmTextFieldWidgetClass, gerdesDBBox,
                                           XmNcolumns, 15,
                                           XmNvalue, theDatabase,
                                           NULL);
    XgAddHelpCallBackFromFile(gerdesDBText, "xgd_db_text");
    XtAddCallback(gerdesDBText, XmNactivateCallback, DoAddDatabase, NULL);

    gerdesDBButton = XtVaCreateManagedWidget("xgdbset_gerdes_session_button", xmPushButtonWidgetClass, gerdesDBBox,
                       XmNlabelString, XmStringCreateSimple("Add Database"),
                                             NULL);
    XgAddHelpCallBackFromFile(gerdesDBButton, "xgd_db_button");
    XtAddCallback(gerdesDBButton, XmNactivateCallback, DoAddDatabase, gerdesDialog);
    if (!DatabaseWritable) {
        XtSetSensitive(gerdesDBButton, False);
    }
    gerdesLocationLabel = XtVaCreateManagedWidget("xgdbset_gerdes_session_caption", xbaeCaptionWidgetClass, gerdesContainer,
                                          XmNrightAttachment, XmATTACH_FORM,
                                          XmNtopAttachment, XmATTACH_WIDGET,
                                                XmNtopWidget, gerdesDBLabel,
                                         XmNlabelPosition, XbaePositionLeft,
                           XmNlabelString, XmStringCreateSimple("Location"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                                  XmNtraversalOn, False,
                                                  NULL);

    gerdesLocationBox = XtVaCreateManagedWidget("xgdbset_gerdes_rc", xmRowColumnWidgetClass, gerdesLocationLabel,
                                                XmNorientation, XmHORIZONTAL,
           XmNpacking, XmPACK_COLUMN, XmNentryAlignment, XmALIGNMENT_CENTER,
                                                NULL);

    gerdesLocationText = XtVaCreateManagedWidget("xgdbset_gerdes_session_text", xmTextFieldWidgetClass, gerdesLocationBox,
                                                 XmNcolumns, 15,
                                                 XmNvalue, theLocation,
                                                 NULL);
    XgAddHelpCallBackFromFile(gerdesLocationText, "xgd_loc_text");
    XtAddCallback(gerdesLocationText, XmNactivateCallback, DoCreateLocation, NULL);

    gerdesLocationButton = XtVaCreateManagedWidget("xgdbset_gerdes_session_button", xmPushButtonWidgetClass, gerdesLocationBox,
                    XmNlabelString, XmStringCreateSimple("Create Location"),
                                                   NULL);
    XgAddHelpCallBackFromFile(gerdesLocationButton, "xgd_loc_button");
    XtAddCallback(gerdesLocationButton, XmNactivateCallback, DoCreateLocation, gerdesDialog);

    gerdesMapsetLabel = XtVaCreateManagedWidget("xgdbset_gerdes_session_caption", xbaeCaptionWidgetClass, gerdesContainer,
                                          XmNrightAttachment, XmATTACH_FORM,
                                          XmNtopAttachment, XmATTACH_WIDGET,
                                          XmNtopWidget, gerdesLocationLabel,
                                         XmNlabelPosition, XbaePositionLeft,
                             XmNlabelString, XmStringCreateSimple("Mapset"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                                XmNtraversalOn, False,
                                                NULL);

    gerdesMapsetBox = XtVaCreateManagedWidget("xgdbset_gerdes_rc", xmRowColumnWidgetClass, gerdesMapsetLabel,
                                              XmNorientation, XmHORIZONTAL,
           XmNpacking, XmPACK_COLUMN, XmNentryAlignment, XmALIGNMENT_CENTER,
                                              NULL);

    gerdesMapsetText = XtVaCreateManagedWidget("xgdbset_gerdes_session_text", xmTextFieldWidgetClass, gerdesMapsetBox,
                                               XmNcolumns, 15,
                                               XmNvalue, theMapset,
                                               NULL);
    XgAddHelpCallBackFromFile(gerdesMapsetText, "xgdbset_mapset_text");
    XtAddCallback(gerdesMapsetText, XmNactivateCallback, DoCreateMapset, NULL);

    gerdesMapsetButton = XtVaCreateManagedWidget("xgdbset_gerdes_session_button", xmPushButtonWidgetClass, gerdesMapsetBox,
                      XmNlabelString, XmStringCreateSimple("Create Mapset"),
                                                 NULL);
    XgAddHelpCallBackFromFile(gerdesMapsetButton, "xgd_map_button");
    XtAddCallback(gerdesMapsetButton, XmNactivateCallback, DoCreateMapset, gerdesDialog);

    XtManageChild(gerdesDialog);
}

static          Boolean
MakeSelectionVisible(list)
    Widget          list;
{
    int             visible;
    int             count;
    int             top;
    int            *pos;
    int             posCount;
    int             newTop;
    if (XmListGetSelectedPos(list, &pos, &posCount)) {
        XtVaGetValues(list, XmNvisibleItemCount, &visible, XmNitemCount, &count, XmNtopItemPosition, &top, NULL);
        if (!(((*pos) >= top) && ((*pos) < (top + visible)))) {
            newTop = *pos - visible / 2;
            if (newTop < 1) {
                newTop = 1;
            }
            XmListSetPos(list, newTop);
        }
        return True;
    }
    return False;
}

static void
UpdateSessionList()
{
    XmString        s;
    char            buf[256];
    sprintf(buf, "%s/.xgrass/session", getenv("HOME"));
    _XgPutDirectoryInList(buf, sessionList, 0);

    XmListDeselectAllItems(sessionList);
    if (gerdesSessionText)
        XmTextSetString(gerdesSessionText, theSession);
    if (*theSession) {
        XmListSelectItem(sessionList, s = XmStringCreateSimple(theSession), False);
        if (!MakeSelectionVisible(sessionList)) {
            *theSession = 0;
            XmListDeselectAllItems(sessionList);
        }
    }
}

static void
UpdateDBList()
{
    XmString        s;
    _XgPutStringArrayInList(dbList, databaseList, databaseCount);
    XmListDeselectAllItems(dbList);
    if (gerdesDBText)
        XmTextSetString(gerdesDBText, theDatabase);
    if (*theDatabase) {
        XmListSelectItem(dbList, s = XmStringCreateSimple(theDatabase), False);
        if (!MakeSelectionVisible(dbList)) {
            *theDatabase = 0;
            XmListDeselectAllItems(dbList);
        }
    }
}

static void
UpdateLocList()
{
    char            path[512];
    XmString        s;
    if (*theDatabase) {
        sprintf(path, "%s", theDatabase);
        _XgPutDirectoryInList(path, locationList, 0);
        XmListDeselectAllItems(locationList);
        if (gerdesLocationText)
            XmTextSetString(gerdesLocationText, theLocation);
        if (*theLocation) {
            XmListSelectItem(locationList, s = XmStringCreateSimple(theLocation), False);
            if (!MakeSelectionVisible(locationList)) {
                *theLocation = 0;
                XmListDeselectAllItems(locationList);
            }
        }
    } else {
        XmListDeleteAllItems(locationList);
        if (gerdesLocationText)
            XmTextSetString(gerdesLocationText, theLocation);
    }
}

static void
UpdateMapsetList()
{
    char            path[512];
    XmString        s;
    int             mapsetCount;
    if (*theDatabase && *theLocation) {
        sprintf(path, "%s/%s", theDatabase, theLocation);
        G__setenv("GISDBASE", theDatabase);
        G__setenv("LOCATION_NAME", theLocation);
        mapsetCount = _XgPutDirectoryInListOWN(path, mapsetList, 0);
        if (mapsetCount) {
            XmListDeselectAllItems(mapsetList);
        } else {
            /* The user owns no mapsets in this location. */
            MakeFirstMapset();
            mapsetCount = _XgPutDirectoryInListOWN(path, mapsetList, 0);
        }
        if (gerdesMapsetText)
            XmTextSetString(gerdesMapsetText, theMapset);
        if (*theMapset) {
            XmListSelectItem(mapsetList, s = XmStringCreateSimple(theMapset), False);
            if (!MakeSelectionVisible(mapsetList)) {
                *theMapset = 0;
                XmListDeselectAllItems(mapsetList);
            }
        }
    } else {
        XmListDeleteAllItems(mapsetList);
        if (gerdesMapsetText)
            XmTextSetString(gerdesMapsetText, theMapset);
    }
}

static void
UpdateAllLists()
{
    UpdateSessionList();
    UpdateDBList();
    UpdateLocList();
    UpdateMapsetList();
}


static void
#ifdef _NO_PROTO
SessionListCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
#else
SessionListCallback(Widget w, XtPointer data, XmListCallbackStruct *cbs)
#endif
{
    char           *text;
    char            buf[1024];
    char           *home = (char *) getenv("HOME");

    int             count;

    XgDoHourGlass(_XG_Global.applShell);
    XtVaGetValues(w, XmNselectedItemCount, &count, NULL);
    if (count) {
        XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &text);
        strcpy(theSession, text);
        if (text != NULL && *text != '\0') {
            sprintf(buf, "%s/.xgrass/session/%s", home, text);
            if (access(buf, F_OK) == -1) {
                fprintf(stderr, "No such session:%s", text);
                exit(1);
            } else {
#if defined(mips) || defined(BSD)
                sprintf(buf, "%s/.xgrass/session/%s", home, text);
                setenv("GISRC", _XgStrDup(buf), 1);
#else
                sprintf(buf, "GISRC=%s/.xgrass/session/%s", home, text);
                putenv(_XgStrDup(buf));
#endif
                sprintf(buf, "%s/.xgrass/session/%s", home, text);
                G__setenv("GISRC", _XgStrDup(buf));
            }
        } else {
	    XgUndoHourGlass(_XG_Global.applShell);
            return;
        }
        ReadCurrentValues();
        if (gerdesSessionText) {
            XmTextSetString(gerdesSessionText, text);
        }
        UpdateAllLists();
    } else {
        InitValues();
        UpdateAllLists();
    }
    XgUndoHourGlass(_XG_Global.applShell);
}

static void
#ifdef _NO_PROTO
DBListCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
#else
DBListCallback(Widget w, XtPointer data, XmListCallbackStruct * cbs)
#endif
{
    char           *text;
    int             count;

    XgDoHourGlass(_XG_Global.applShell);
    XtVaGetValues(w, XmNselectedItemCount, &count, NULL);
    if (count) {
        XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &text);
        ChangeDatabase(text);
    } else {
        ChangeDatabase("");
    }
    XgUndoHourGlass(_XG_Global.applShell);
}

static void
#ifdef _NO_PROTO
LocationListCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
#else
LocationListCallback(Widget w, XtPointer data, XmListCallbackStruct * cbs)
#endif
{
    char           *text;
    int             count;
  
    XgDoHourGlass(_XG_Global.applShell);
    XtVaGetValues(w, XmNselectedItemCount, &count, NULL);
    if (count) {
        XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &text);
        ChangeLocation(text);
    } else {
        ChangeLocation("");
    }
    XgUndoHourGlass(_XG_Global.applShell);
}

static void
#ifdef _NO_PROTO
MapsetListCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
#else
MapsetListCallback(Widget w, XtPointer data, XmListCallbackStruct * cbs)
#endif
{
    char           *text;
    int             count;

    XgDoHourGlass(_XG_Global.applShell);
    XtVaGetValues(w, XmNselectedItemCount, &count, NULL);
    if (count) {
        XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &text);
        ChangeMapset(text);
    } else {
        ChangeMapset("");
    }
    XgUndoHourGlass(_XG_Global.applShell);
}

static void
#ifdef _NO_PROTO
WriteCurrentValues()
#else
WriteCurrentValues(void)
#endif
{
    char           *gisrc;
    FILE           *grassrc;

    gisrc = getenv("GISRC");
    grassrc = fopen(gisrc, "w");
    fprintf(grassrc, "GISDBASE: %s\n", theDatabase);
    fprintf(grassrc, "LOCATION_NAME: %s\n", theLocation);
    fprintf(grassrc, "MAPSET: %s\n", theMapset);
    fclose(grassrc);
    G__setenv("GISDBASE", theDatabase);
    G__setenv("LOCATION_NAME", theLocation);
    G__setenv("MAPSET", theMapset);
}

static void
#ifdef _NO_PROTO
ReadCurrentValues()
#else
ReadCurrentValues(void)
#endif
{
    char           *gisrc;
    char          **words;
    char            line[256];
    char           *stat;
    char           *home;
    FILE           *grassrc;
    char *sdb = NULL;
    char *sloc = NULL;
    char *smap = NULL;

    if ( *theDatabase ) 
	sdb = XtNewString(theDatabase);

    if ( *theLocation ) 
	sloc = XtNewString(theLocation);

    if ( *theMapset ) 
	smap = XtNewString(theMapset);

    gisrc = getenv("GISRC");
    if (gisrc) {
        /* does it exist ? */
        if (access(gisrc, F_OK) != -1) {
            grassrc = fopen(gisrc, "r");
        } else
            return;
    } else {
        return;
    }
    stat = fgets(line, 255, grassrc);
    if ( line == NULL || *line == '\0' ) {
	char file[1024];
        char *text = G_rindex(gisrc,'/');
        int pos;
        XmString xms = XmStringCreateSimple(text + 1);

	sprintf(file, "%s/.xgrass/histories%s",
	    (char *) getenv("HOME"), text);
	unlink(file);
	sprintf(file, "%s/.xgrass/session%s",
	    (char *) getenv("HOME"), text);
            unlink(file);
        pos = XmListItemPos(sessionList, xms);
        XmListDeletePos(sessionList, pos);
        XgWarningDialog(_XG_Global.applShell, 
            "Error in session file. Please recreate it.");
        return;
    }
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    if ( words[1] )
	strcpy(theDatabase, words[1]);
    else {
        if ( sdb ) {
	    strcpy(theDatabase, sdb);
	    XtFree(sdb);
        } else {
            *theDatabase = '\0';
        }
        if ( sloc ) {
	    strcpy(theLocation, sloc);
	    XtFree(sloc);
        } else {
            *theLocation = '\0';
        }
        if ( smap ) {
	    strcpy(theMapset, smap);
	    XtFree(smap);
        } else {
            *theMapset = '\0';
        }
	fclose(grassrc);
        return;
    }

    stat = fgets(line, 255, grassrc);
    if ( line == NULL || *line == '\0' ) {
	char file[1024];
        char *text = G_rindex(gisrc,'/');
        int pos;
        XmString xms = XmStringCreateSimple(text + 1);

	sprintf(file, "%s/.xgrass/histories%s",
	    (char *) getenv("HOME"), text);
	unlink(file);
	sprintf(file, "%s/.xgrass/session%s",
	    (char *) getenv("HOME"), text);
            unlink(file);
        pos = XmListItemPos(sessionList, xms);
        XmListDeletePos(sessionList, pos);
        XgWarningDialog(_XG_Global.applShell, 
            "Error in session file. Please recreate it.");
        return;
    }
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    if ( words[1] )
	strcpy(theLocation, words[1]);
    else {
        if ( sdb ) {
	    strcpy(theDatabase, sdb);
	    XtFree(sdb);
        } else {
            *theDatabase = '\0';
        }
        if ( sloc ) {
	    strcpy(theLocation, sloc);
	    XtFree(sloc);
        } else {
            *theLocation = '\0';
        }
        if ( smap ) {
	    strcpy(theMapset, smap);
	    XtFree(smap);
        } else {
            *theMapset = '\0';
        }
	fclose(grassrc);
        return;
    }

    stat = fgets(line, 255, grassrc);
    if ( line == NULL || *line == '\0' ) {
	char file[1024];
        char *text = G_rindex(gisrc,'/');
        int pos;
        XmString xms = XmStringCreateSimple(text + 1);

	sprintf(file, "%s/.xgrass/histories%s",
	    (char *) getenv("HOME"), text);
	unlink(file);
	sprintf(file, "%s/.xgrass/session%s",
	    (char *) getenv("HOME"), text);
            unlink(file);
        pos = XmListItemPos(sessionList, xms);
        XmListDeletePos(sessionList, pos);
        XgWarningDialog(_XG_Global.applShell, 
            "Error in session file. Please recreate it.");
        return;
    }
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    if ( words[1] ) 
	strcpy(theMapset, words[1]);
    else {
        if ( sdb ) {
	    strcpy(theDatabase, sdb);
	    XtFree(sdb);
        } else {
            *theDatabase = '\0';
        }
        if ( sloc ) {
	    strcpy(theLocation, sloc);
	    XtFree(sloc);
        } else {
            *theLocation = '\0';
        }
        if ( smap ) {
	    strcpy(theMapset, smap);
	    XtFree(smap);
        } else {
            *theMapset = '\0';
        }
	fclose(grassrc);
        return;
    }
    fclose(grassrc);
    G__setenv("GISDBASE", theDatabase);
    G__setenv("LOCATION_NAME", theLocation);
    G__setenv("MAPSET", theMapset);
}

static void
#ifdef _NO_PROTO
FindDatabaseList()
#else
FindDatabaseList(void)
#endif
{
    char            buf[1024];
    char            buf2[1024];
    struct stat     date1, date2;
    char           *home = (char *) getenv("HOME");
    char           *val;

    databaseNewer = False;
    databaseSystem = False;
    sprintf(buf, "%s/.xgrass/databases", home);
    if (!access(buf, R_OK | F_OK)) {
        strcpy(DatabaseFile, buf);
        DatabaseWritable = 1;
        val = getenv("XGRASSLIBDIR");
        strcpy(buf2, val);
        strcat(buf2, "/databases");
        if (!access(buf2, (R_OK | F_OK))) {
            stat(buf, &date1);
            stat(buf2, &date2);
            if (date1.st_mtime < date2.st_mtime) {
                databaseNewer = True;
            }
        }
    } else {
        databaseSystem = True;
        val = getenv("XGRASSLIBDIR");
        if (val) {
            strcpy(DatabaseFile, val);
            strcat(DatabaseFile, "/databases");
            if (access(DatabaseFile, (R_OK | F_OK))) {
                perror("XGrass database list file is not available");
                exit(1);
            }
            DatabaseWritable = 0;
        } else {
            perror("XGRASSLIBDIR environment variable is not set");
            exit(1);
        }
    }
}

static void
#ifdef _NO_PROTO
ReadDatabaseList()
#else
ReadDatabaseList(void)
#endif
{
    FILE           *theFile;
    extern char   **_XgPutFileInStringArray();
    theFile = fopen(DatabaseFile, "r");
    databaseList = _XgPutFileInStringArray(theFile, &databaseCount);
    fclose(theFile);
}

static void
#ifdef _NO_PROTO
WriteDatabaseList()
#else
WriteDatabaseList(void)
#endif
{
    FILE           *theFile;
    int             i;
    theFile = fopen(DatabaseFile, "w");
    for (i = 0; i < databaseCount; i++) {
        fprintf(theFile, "%s\n", databaseList[i]);
    }
    fclose(theFile);
}

static void
#ifdef _NO_PROTO
XgdbsetApply(w, data)
    Widget          w;
    XtPointer       data;
#else
XgdbsetApply(Widget w, XtPointer data)
#endif
{
    Widget          shell = (Widget) data;
    DoGerdesDialog(shell);
}

static void
#ifdef _NO_PROTO
XgdbsetOk(w, data)
    Widget          w;
    XtPointer       data;
#else
XgdbsetOk(Widget w, XtPointer data)
#endif
{
    char            buf[256];
    Widget          shell = (Widget) data;

    if (*theDatabase == '\0') {
        XgWarningDialog(shell, "Database not set!!");
        return;
    } else if (*theLocation == '\0') {
        XgWarningDialog(shell, "Location not set!!");
        return;
    } else if (*theMapset == '\0') {
        XgWarningDialog(shell, "Mapset not set!!");
        return;
    }
    WriteCurrentValues();
    ReadCurrentValues();
    XtDestroyWidget(w);
    XtUnmapWidget(shell);
    if (*theSession) {
        sprintf(buf, "%s/.xgrass/histories/%s", getenv("HOME"), theSession);
        LoadHistory(buf, 0);
    }
    sprintf(buf, "%s [DB: %s LOCATION: %s MAPSET: %s]",
            version, theDatabase, theLocation, theMapset);
    if (shell == _XG_Global.applShell) {
        XtVaSetValues(shell, XmNtitle, buf, NULL);
    } else {
        XtVaSetValues(_XG_Global.applShell, XmNtitle, buf, NULL);
    }
    G_gisinit("xgrass");
    if (!_XG_Global.menuRunning)
        CreateMenuSystem();
    if (!_XG_Global.menuRunning)
        XtMapWidget(shell);
    _XG_Global.menuRunning = True;
}


static void
#ifdef _NO_PROTO
XgdbsetCancel(w, data)
    Widget          w;
    XtPointer       data;
#else
XgdbsetCancel(Widget w, XtPointer data)
#endif
{
    if (theShell == _XG_Global.applShell) {
        XtDestroyWidget(theShell);
    } else {
        XtDestroyWidget(theShell);
    }
    if (fullExit) {
        XFlush(_XG_Global.display);
        exit(0);
    }
}

InitValues()
{
    theSession[0] = 0;
    theDatabase[0] = 0;
    theLocation[0] = 0;
    theMapset[0] = 0;
}

/* NEW */
static void
#ifdef _NO_PROTO
DeleteSessionCallBack(w, cld, cad)
Widget w;
XtPointer cld;
XtPointer cad;
#else
DeleteSessionCallBack(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget list = (Widget)cld;
    int i;
    int *pos;
    int posCount;
    XmString *items;

    if ( XmListGetSelectedPos(list, &pos, &posCount) ) {
        char buf[1024];
	char *text;

	XtVaGetValues(list, XmNitems, &items, NULL);
	/* actually delete session files */
	XmStringGetLtoR(items[pos[0] - 1], XmSTRING_DEFAULT_CHARSET, 
	    &text);
	sprintf(buf,"Confirm deletion of session [%s] ?", text);
        if ( XgYesNo(w, buf) ) {
	    char file[1024];

	    sprintf(file, "%s/.xgrass/histories/%s", 
		(char *) getenv("HOME"), text);
	    unlink(file);
	    sprintf(file, "%s/.xgrass/session/%s", 
		(char *) getenv("HOME"), text);
	    unlink(file);
	    XmListDeletePos(list, pos[0]);
	    InitValues();
	    UpdateAllLists();
        }
        XtFree(&pos);
    } else {
       XgWarningDialog(w, "Nothing to delete.");
       return;
    }
}
/* NEW */

static int
#ifdef _NO_PROTO
CreateLayout(w)
    Widget          w;
#else
CreateLayout(Widget w)
#endif
{
    Arg             al[16];
    Widget          child;
    int             ac = 0;
    FILE           *fp, *fopen();
    int             length;
    Boolean         noIntro = False;
    XmString        xms;
    XmString        xms1;
    XmString        xms2;
    char           *string;
    char           *ptr;
    char            file[256];
    struct stat     statbuf;
/* NEW */
    Widget deleteButton, sessionRC;
/* NEW */

    FindDatabaseList();
    InitValues();
    ReadDatabaseList();

    if (databaseSystem) {
        xms = (XmString) XgCreateXmStringFromFile("intro_personal");
    } else if (databaseNewer) {
        xms = (XmString) XgCreateXmStringFromFile("intro_old_dbs");
    } else {
        xms = (XmString) XgCreateXmStringFromFile("intro_text");
    }
    if (xms == NULL) {
        fprintf(stderr, "intro_text file missing...ignoring this fact\n");
        noIntro = True;
    }
    if (!noIntro) {
        XtSetArg(al[ac], XmNpromptLabelString, xms);
        ac++;
    }
    xms1 = XmStringCreateSimple("Accept");
    if ( fullExit )
	xms2 = XmStringCreateSimple("Exit");
    else
	xms2 = XmStringCreateSimple("Cancel");
    XtSetArg(al[ac], XmNokLabelString, xms1);
    ac++;
    XtSetArg(al[ac], XmNapplyLabelString, XmStringCreateSimple("Add Items..."));
    ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms2);
    ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, False);
    ac++;
    if (fullExit) {
        xgi = XgCreateInteractor(w, "XGRASS Startup Dialog", al, ac);
    } else {
        xgi = XgCreateInteractor(w, "Change Database/Location/Mapset", al, ac);
    }
    XtManageChild(xgi);
    if (!noIntro) {
        XtManageChild(XgInteractorGetChild(xgi, XmINTERACT_PROMPT_LABEL));
        XtVaSetValues(XgInteractorGetChild(xgi, XmINTERACT_PROMPT_LABEL),
                      XmNalignment, XmALIGNMENT_BEGINNING, NULL);
        XmStringFree(xms);
    }
    XtManageChild(child = XgInteractorGetChild(xgi, XmINTERACT_APPLY_BUTTON));
    XgAddHelpCallBackFromFile(child, "xgdb_add_button");
    XtAddCallback(xgi, XmNapplyCallback, XgdbsetApply, (XtPointer) w);
    XtAddCallback(xgi, XmNokCallback, XgdbsetOk, (XtPointer) w);
    XtAddCallback(xgi, XmNcancelCallback, XgdbsetCancel, (XtPointer) w);

    mainForm = XtVaCreateManagedWidget("xgdbset_form", xmFormWidgetClass, xgi,
                                       NULL);

    listContainer = XtVaCreateManagedWidget("xgdbset_list_container", xmRowColumnWidgetClass, mainForm,
                                            XmNpacking, XmPACK_TIGHT,
                                            XmNorientation, XmHORIZONTAL,
                                            XmNtopAttachment, XmATTACH_FORM,
                                            XmNleftAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                          XmNrightAttachment, XmATTACH_FORM,
                                            NULL);

    sessionFrame = XtVaCreateManagedWidget("xgdbset_session_frame", xmFrameWidgetClass, listContainer,
                                           XmNmarginWidth, 15,
                                           XmNmarginHeight, 15,
                                           NULL);

    sessionCaption = XtVaCreateManagedWidget("xgdbset_session_caption", xbaeCaptionWidgetClass, sessionFrame,
                                          XmNlabelPosition, XbaePositionTop,
                            XmNlabelString, XmStringCreateSimple("Session"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                             XmNtraversalOn, False,
                                             NULL);
     
/* NEW */
    sessionRC =  XtVaCreateManagedWidget("xgdbset_session_rc", xmRowColumnWidgetClass, sessionCaption, XmNpacking, XmPACK_TIGHT, NULL);

/* NEW */
    ac = 0;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC);
    ac++;
    XtSetArg(al[ac], XmNwidth, 150);
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
/* NEW */
    sessionList = XmCreateScrolledList(sessionRC, "xgd_ses_list", al, ac);
/* NEW */
/* OLD
    sessionList = XmCreateScrolledList(sessionCaption, "xgd_ses_list", al, ac);
*/
    XgAddHelpCallBackFromFile(sessionList, "xgd_ses_list");

    XtAddCallback(sessionList, XmNsingleSelectionCallback, SessionListCallback, xgi);
    XtManageChild(sessionList);

/* NEW */
    deleteButton = XtVaCreateManagedWidget("xgdbset_session_delete", xmPushButtonWidgetClass, sessionRC, XmNlabelString, XmStringCreateSimple("Delete"), NULL);
    XtAddCallback(deleteButton, XmNactivateCallback, DeleteSessionCallBack, sessionList);
/* NEW */

    theFrame = XtVaCreateManagedWidget("xgdbset_session_frame", xmFrameWidgetClass, listContainer,
                                       XmNmarginWidth, 15,
                                       XmNmarginHeight, 15,
                                       NULL);

    listContainer2 = XtVaCreateManagedWidget("xgdbset_list_container", xmRowColumnWidgetClass, theFrame,
                                             XmNpacking, XmPACK_COLUMN,
                                             XmNorientation, XmHORIZONTAL,
                                             XmNtopAttachment, XmATTACH_FORM,
                                           XmNleftAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                          XmNrightAttachment, XmATTACH_FORM,
                                             NULL);

    dbCaption = XtVaCreateManagedWidget("xgdbset_session_caption", xbaeCaptionWidgetClass, listContainer2,
                                        XmNlabelPosition, XbaePositionTop,
                           XmNlabelString, XmStringCreateSimple("Database"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                        XmNtraversalOn, False,
                                        NULL);

    ac = 0;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC);
    ac++;
    XtSetArg(al[ac], XmNwidth, 150);
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
    dbList = XmCreateScrolledList(dbCaption, "xgd_db_list", al, ac);

    XtAddCallback(dbList, XmNsingleSelectionCallback, DBListCallback, xgi);
    XgAddHelpCallBackFromFile(dbList, "xgd_db_list");
    XtManageChild(dbList);

    locationCaption = XtVaCreateManagedWidget("xgdbset_session_caption", xbaeCaptionWidgetClass, listContainer2,
                                          XmNlabelPosition, XbaePositionTop,
                           XmNlabelString, XmStringCreateSimple("Location"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                              XmNtraversalOn, False,
                                              NULL);

    ac = 0;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC);
    ac++;
    XtSetArg(al[ac], XmNwidth, 150);
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
    locationList = XmCreateScrolledList(locationCaption, "xgd_loc_list", al, ac);

    XtAddCallback(locationList, XmNsingleSelectionCallback, LocationListCallback, xgi);
    XgAddHelpCallBackFromFile(locationList, "xgd_loc_list");
    XtManageChild(locationList);

    mapsetCaption = XtVaCreateManagedWidget("xgdbset_mapset_caption", xbaeCaptionWidgetClass, listContainer2,
                                          XmNlabelPosition, XbaePositionTop,
                             XmNlabelString, XmStringCreateSimple("Mapset"),
                                     XmNlabelAlignment, XbaeAlignmentCenter,
                                            XmNtraversalOn, False,
                                            NULL);

    ac = 0;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC);
    ac++;
    XtSetArg(al[ac], XmNwidth, 150);
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
    mapsetList = XmCreateScrolledList(mapsetCaption, "xgd_map_list", al, ac);

    XtAddCallback(mapsetList, XmNsingleSelectionCallback, MapsetListCallback, xgi);
    XgAddHelpCallBackFromFile(mapsetList, "xgd_map_list");
    XtManageChild(mapsetList);

    UpdateAllLists();
}

#ifdef _NO_PROTO
XgDbSet(sessions, count, useToplevel)
    char          **sessions;
    int             count;
    Boolean         useToplevel;
#else
XgDbSet(char **sessions, int count, Boolean useToplevel)
#endif
{
    Widget          shell;
    Arg             al[16];
    int             ac;
    static Boolean  beenHere = False;
    char            buf[256];
    Atom            protocol;

    fullExit = False;
    if (useToplevel) {
        shell = _XG_Global.applShell;
        fullExit = True;
    } else {
        shell = XtAppCreateShell(_XG_Global.progName, "XGrass", applicationShellWidgetClass, _XG_Global.display, NULL, 0);

        protocol = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
        XmAddWMProtocols(shell, &protocol, 1);
        XtAddEventHandler(shell, NoEventMask, True, _XgWMClientMessage, shell);

        if (XmIsMotifWMRunning(shell)) {
            unsigned int    decor_flags, func_flags;

            decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
            decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

            func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
            func_flags |= MWM_FUNC_MOVE;

            XtVaSetValues(shell,
                          XmNmwmDecorations, decor_flags,
                          XmNmwmFunctions, func_flags,
                          NULL);
        }
        beenHere = False;
    }

    theShell = shell;

    if (fullExit) {
        sprintf(buf, "XGRASS Startup Dialog", version);
    } else {
        sprintf(buf, "Change Database/Location/Mapset", version);
    }
    CreateLayout(shell);
    XtVaSetValues(shell, XmNtitle, buf, NULL);
    if (beenHere)
        XtMapWidget(shell);
    else if (!useToplevel && !beenHere) {
        XtRealizeWidget(shell);
    }
    beenHere = True;
}

void
#ifdef _NO_PROTO
StartTrio(d, l, m)
    char           *d;
    char           *l;
    char           *m;
#else
StartTrio(char *d, char *l, char *m)
#endif
{
    char            buf[1024];

    strcpy(theDatabase, d);
    strcpy(theLocation, l);
    strcpy(theMapset, m);

    WriteCurrentValues();
    sprintf(buf, "%s [DB: %s LOCATION: %s MAPSET: %s]",
            version, theDatabase, theLocation, theMapset);
    XtVaSetValues(_XG_Global.applShell, XmNtitle, buf, NULL);
    if (!_XG_Global.menuRunning)
        CreateMenuSystem();
    _XG_Global.menuRunning = True;
}

void
#ifdef _NO_PROTO
StartSession(text)
    char           *text;
#else
StartSession(char *text)
#endif
{
    char            buf[1024];
    char           *home = (char *) getenv("HOME");

    if (text != NULL && *text != '\0') {
        sprintf(buf, "%s/.xgrass/session/%s", home, text);
        if (access(buf, F_OK) == -1) {
            fprintf(stderr, "No such session:%s", text);
            exit(1);
        } else {
#if defined(mips) || defined(BSD)
            sprintf(buf, "%s/.xgrass/session/%s", home, text);
            setenv("GISRC", _XgStrDup(buf), 1);
#else
            sprintf(buf, "GISRC=%s/.xgrass/session/%s", home, text);
            putenv(_XgStrDup(buf));
#endif
            sprintf(buf, "%s/.xgrass/session/%s", home, text);
            G__setenv("GISRC", _XgStrDup(buf));
        }
    } else {
        return;
    }
    ReadCurrentValues();
    sprintf(buf, "%s/.xgrass/histories/%s", home, text);
    LoadHistory(buf);
    sprintf(buf, "%s [DATABASE: %s    LOCATION: %s    MAPSET: %s]",
            version, theDatabase, theLocation, theMapset);
    XtVaSetValues(_XG_Global.applShell, XmNtitle, buf, NULL);
    if (!_XG_Global.menuRunning)
        CreateMenuSystem();
    _XG_Global.menuRunning = True;
}
