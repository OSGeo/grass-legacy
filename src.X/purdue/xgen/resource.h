static struct _resourceTable {
	int index;
	char *name;
	int type;
	unsigned int valid;
} resourceTable[] = {
	InitialShells,    "initialshells",     STRING,  ENV,
	X,                "x",                 INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
	DX,               "dx",                REAL,    SHL|MU|CO|OBJ|ALLOBJ,
	Y,                "y",                 INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
	DY,               "dy",                REAL,    SHL|MU|CO|OBJ|ALLOBJ,
	Width,            "width",             INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
	Height,           "height",            INTEGER, SHL|MU|CO|OBJ|ALLOBJ,
	MaxWidth,         "maxwidth",          INTEGER, SHL|MU,
	MaxHeight,        "maxheight",         INTEGER, SHL|MU,
	Help,             "help",              STRING,  SHL|MU|CO|OBJ|ALLOBJ,
	Columns,          "columns",           INTEGER, OBJ|TA|ME,
	Font,             "font",              STRING,  OBJ|ALLOBJ,
	EntryFont,        "entryfont",         STRING,  OBJ|TA,
	Background,       "background",        STRING,  SHL|MU|CO|OBJ|ALLOBJ,
	Foreground,       "foreground",        STRING,  SHL|MU|CO|OBJ|ALLOBJ,
	TitleString,      "titlestring",       STRING,  SHL|MU|CO|OBJ|ALLOBJ,
	Override,         "override",          BOOLEAN, SHL|MU|CO,
	Popup,            "popup",             STRING,  OBJ|PB,
	Popdown,          "popdown",           STRING,  OBJ|PB,
	Destroy,          "destroy",           STRING,  OBJ|PB,
	Exit,             "exit",              INTEGER, OBJ|PB,
	RunForeground,    "runforeground",     STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	RunBackground,    "runbackground",     STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	InputFrom,        "inputfrom",         STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	CaptureOutput,    "captureoutput",     STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	UpdateFrom,       "updatefrom",        STRING,  SHL|MU|OBJ|LI,
	Pane,             "pane",              BOOLEAN, SHL|CO,
	Store,            "store",             STRING,  OBJ|PB,
	GetEnv,           "getenv",            STRING,  OBJ|PB,
	Clear,            "clear",             STRING,  OBJ|PB,
	CommandArg,       "commandarg",        STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	Set,              "set",               STRING,  ENV|SHL|MU|CO|OBJ|PB|PU,
	Alignment,        "alignment",         STRING,  OBJ|ME|LA|PB,
	ListElement,      "listelement",       STRING,  OBJ|LI|TO,
	ListType,         "listtype",          STRING,  OBJ|LI,
	VisibleItems,     "visibleitems",      INTEGER, SHL|MU|OBJ|LI,
	ValueString,      "valuestring",       STRING,  OBJ|TE|TO|LI,
	LabelPixmap,      "labelpixmap",       STRING,  OBJ|LA|PB|TO,
	Minimum,          "minimum",           INTEGER, OBJ|SL,
	Maximum,          "maximum",           INTEGER, OBJ|SL,
	StartValue,       "startvalue",        INTEGER, OBJ|SL,
	SliderWidth,      "sliderwidth",       INTEGER, OBJ|SL,
	SliderHeight,     "sliderheight",      INTEGER, OBJ|SL,
	Orientation,      "orientation",       STRING,  OBJ|SL|SE,
	DecimalPoints,    "decimalpoints",     INTEGER, OBJ|SL,
	Rows,             "rows",              INTEGER, OBJ|TA,
	RowsDisplayed,    "rowsdisplayed",     INTEGER, OBJ|TA,
	ColumnsDisplayed, "columnsdisplayed",  INTEGER, OBJ|TA,
	ColumnHeadings,   "columnheadings",    STRING,  OBJ|TA,
	RowHeadings,      "rowheadings",       STRING,  OBJ|TA,
	RowValue,         "rowvalue",          STRING,  OBJ|TA,
	RowHeight,        "rowheight",         INTEGER, OBJ|TA,
	ColumnWidth,      "columnwidth",       INTEGER, OBJ|TA,
	Separator,        "separator",         STRING,  OBJ|TA,
	ToggleType,       "toggletype",        STRING,  OBJ|TO,
	SeparatorType,    "separatortype",     STRING,  OBJ|SE,
};

static struct _shellTable {
	char *name;
	int index;
	unsigned int valid;
} shellTable[] = {
	"menu",   MU, LA|PB|SE,
	"commandboard", CO, PU|LA|LI|ME|PB|TE|TA|SE|SL|TO,
};

static struct _objectTable {
	char *name;
	int index;
} objectTable[] = {
	"label",      LA,
	"message",    ME,
	"list",       LI,
	"pushbutton", PB,
	"textentry",  TE,
	"table",      TA,
	"separator",  SE,
	"slider",     SL,
	"toggle",     TO,
	"pulldown",   PU,
};
