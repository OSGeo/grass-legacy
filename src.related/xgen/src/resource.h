/**********************************************************************
   resource.h   - resource header file
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
static struct _resourceTable {
    int                             index;
    char                           *name;
    int                             type;
    unsigned int                    valid;
}                               resourceTable[] = {

    InitialShells, "initialshells", STRING, ENV,
    X, "x", INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
    DX, "dx", REAL, SHL|MU|CO|OBJ|ALLOBJ,
    Y, "y", INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
    DY, "dy", REAL, SHL|MU|CO|OBJ|ALLOBJ,
    Width, "width", INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
    Height, "height", INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
    ForceSize, "forcesize", BOOLEAN, SHL|CO,
    MaxWidth, "maxwidth", INTEGER, SHL|MU,
    MaxHeight, "maxheight", INTEGER, SHL|MU,
    Help, "help", STRING, SHL|MU|CO|OBJ|ALLOBJ,
    Columns, "columns", INTEGER, SHL|CO|OBJ|TA|ME|ML|TO,
    Font, "font", STRING, OBJ|ALLOBJ,
    EditorFont, "editorfont", STRING, OBJ|ALLOBJ,
    EntryFont, "entryfont", STRING, OBJ|TA,
    Background, "background", STRING, SHL|MU|CO|OBJ|ALLOBJ,
    Foreground, "foreground", STRING, SHL|MU|CO|OBJ|ALLOBJ,
    TitleString, "titlestring", STRING, SHL|MU|CO|OBJ|ALLOBJ,
    Override, "override", BOOLEAN, SHL|MU|CO,
    Functions, "functions", STRING, SHL|MU|CO,
    Decorations, "decorations", STRING, SHL|MU|CO,
    Popup, "popup", STRING, OBJ|PB|TO|TE,
    Popdown, "popdown", STRING, OBJ|PB|TO|TE,
    Destroy, "destroy", STRING, OBJ|PB|TO|TE,
    Exit, "exit", INTEGER, OBJ|PB|TO|TE,
    UpdateObject, "updateobject", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    PostNotice, "postnotice", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    RunForeground, "runforeground", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    RunBackground, "runbackground", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    CommandShell, "commandshell", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    InteractiveShell, "interactive", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    InputFrom, "inputfrom", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    CaptureOutput, "captureoutput", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    NotifyComplete, "notifycomplete", BOOLEAN, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    UpdateFrom, "updatefrom", STRING, SHL|MU|OBJ|LI,
    Pane, "pane", BOOLEAN, SHL|CO,
    PaneType, "panetype", STRING, SHL|CO,
    Store, "store", STRING, OBJ|PB|TO|TE,
    GetEnv, "getenv", STRING, OBJ|PB|TO|TE,
    Clear, "clear", STRING, OBJ|PB|TO|TE,
    CommandArg, "commandarg", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    Set, "set", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    Alignment, "alignment", STRING, OBJ|ME|LA|PB,
    ListElement, "listelement", STRING, OBJ|LI|TO,
    ListSeparator, "listseparator", STRING, OBJ|LI|TO,
    ListType, "listtype", STRING, OBJ|LI,
    VisibleItems, "visibleitems", INTEGER, SHL|MU|OBJ|LI,
    ValueString, "valuestring", STRING, OBJ|TE|TO|LI|ML,
    Scrolled, "scrolled", BOOLEAN, OBJ|ML,
    LabelPixmap, "labelpixmap", STRING, OBJ|LA|PB|TO,
    Minimum, "minimum", INTEGER, OBJ|SL,
    Maximum, "maximum", INTEGER, OBJ|SL,
    StartValue, "startvalue", INTEGER, OBJ|SL,
    SliderWidth, "sliderwidth", INTEGER, OBJ|SL,
    SliderHeight, "sliderheight", INTEGER, OBJ|SL,
    Orientation, "orientation", STRING, SHL|CO|OBJ|SL|SE|TO,
    DecimalPoints, "decimalpoints", INTEGER, OBJ|SL,
    Rows, "rows", INTEGER, OBJ|TA|ML,
    RowsDisplayed, "rowsdisplayed", INTEGER, OBJ|TA,
    FixedRows, "fixedrows", INTEGER, OBJ|TA,
    FixedColumns, "fixedcolumns", INTEGER, OBJ|TA,
    ColumnsDisplayed, "columnsdisplayed", INTEGER, OBJ|TA,
    ColumnHeadings, "columnheadings", STRING, OBJ|TA,
    RowHeadings, "rowheadings", STRING, OBJ|TA,
    RowValue, "rowvalue", STRING, OBJ|TA,
    TableValue, "tablevalue", STRING, OBJ|TA,
    RowHeight, "rowheight", INTEGER, OBJ|TA,
    ColumnWidth, "columnwidth", INTEGER, OBJ|TA,
    Separator, "separator", STRING, OBJ|TA,
    Newline, "newline", BOOLEAN, OBJ|TA,
    ToggleType, "toggletype", STRING, OBJ|TO,
    ToggleState, "togglestate", STRING, OBJ|TO,
    SeparatorType, "separatortype", STRING, OBJ|SE,
    Sensitive, "sensitive", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
    Insensitive, "insensitive", STRING, ENV|SHL|MU|CO|OBJ|PB|PU|TO|TE,
};

static struct _shellTable {
    char                           *name;
    int                             index;
    unsigned int                    valid;
}                               shellTable[] = {

    "menu", MU, LA|PB|SE,
    "commandboard", CO, PU|LA|LI|ME|PB|TE|TA|SE|SL|TO|ML,
};

static struct _objectTable {
    char                           *name;
    int                             index;
}                               objectTable[] = {

    "label", LA,
    "message", ME,
    "list", LI,
    "pushbutton", PB,
    "textentry", TE,
    "table", TA,
    "separator", SE,
    "slider", SL,
    "toggle", TO,
    "pulldown", PU,
    "multiline", ML,
};
