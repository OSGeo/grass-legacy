static struct _resourceTable {
	int index;
	char *name;
	int type;
	unsigned int valid;
} resourceTable[] = {
	InitialShells,         "initialshells",          STRING,  ENV,
	MakeShells,            "makeshells",             STRING,  ENV,
	X,                     "x",                      INTEGER, SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	DX,                    "dx",                     REAL,    SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	Y,                     "y",                      INTEGER, SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	DY,                    "dy",                     REAL,    SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	Width,                 "width",                  INTEGER, SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	Height,                "height",                 INTEGER, SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	MaxWidth,              "maxwidth",               INTEGER, SHL|SM|DM|MB|CB,
	MaxHeight,             "maxheight",              INTEGER, SHL|SM|DM|MB|CB,
	Help,                  "help",                   STRING,  SHL|SM|DM|MB|CB,
	Columns,               "columns",                INTEGER, OBJ|TA|ME,
	Font,                  "font",                   STRING,  OBJ|ALLOBJ,
	EntryFont,             "entryfont",              STRING,  OBJ|TA,
	Background,            "background",             STRING,  SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	Foreground,            "foreground",             STRING,  SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	TitleString,           "titlestring",            STRING,  SHL|SM|DM|MB|CB|OBJ|ALLOBJ,
	Override,              "override",               BOOLEAN, SHL|SM|DM|MB|CB,
	Popup,                 "popup",                  STRING,  OBJ|PB,
	Popdown,               "popdown",                STRING,  OBJ|PB,
	Destroy,               "destroy",                STRING,  OBJ|PB,
	Exit,                  "exit",                   INTEGER, OBJ|PB,
	RunForeground,         "runforeground",          STRING,  ENV|SHL|SM|DM|MB|CB|OBJ|PB,
	RunBackground,         "runbackground",          STRING,  ENV|SHL|SM|DM|MB|CB|OBJ|PB,
	CaptureOutput,         "captureoutput",          STRING,  ENV|SHL|SM|DM|MB|CB|OBJ|PB,
	UpdateFrom,            "updatefrom",             STRING,  SHL|DM,
	Store,                 "store",                  STRING,  OBJ|PB,
	GetEnv,                "getenv",                 STRING,  OBJ|PB,
	Clear,                 "clear",                  STRING,  OBJ|PB,
	CommandArg,            "commandarg",             STRING,  ENV|SHL|SM|DM|MB|CB|OBJ|PB,
	Set,                   "set",                    STRING,  ENV|SHL|SM|DM|MB|CB|OBJ|PB,
	Alignment,             "alignment",              STRING,  OBJ|ME|LA|PB,
	ListElement,           "listelement",            STRING,  OBJ|LI|TO,
	ListType,              "listtype",               STRING,  OBJ|LI,
	VisibleItems,          "visibleitems",           INTEGER, OBJ|LI,
	ValueString,           "valuestring",            STRING,  OBJ|TE|TO|LI,
	LabelPixmap,           "labelpixmap",            STRING,  OBJ|LA|PB|TO,
	Minimum,               "minimum",                INTEGER, OBJ|SL,
	Maximum,               "maximum",                INTEGER, OBJ|SL,
	StartValue,            "startvalue",             INTEGER, OBJ|SL,
	SliderWidth,           "sliderwidth",            INTEGER, OBJ|SL,
	SliderHeight,          "sliderheight",           INTEGER, OBJ|SL,
	Orientation,           "orientation",            STRING,  OBJ|SL|SE,
	DecimalPoints,         "decimalpoints",          INTEGER, OBJ|SL,
	Rows,                  "rows",                   INTEGER, OBJ|TA,
	RowsDisplayed,         "rowsdisplayed",          INTEGER, OBJ|TA,
	ColumnsDisplayed,      "columnsdisplayed",       INTEGER, OBJ|TA,
	ColumnHeadings,        "columnheadings",         STRING,  OBJ|TA,
	RowHeadings,           "rowheadings",            STRING,  OBJ|TA,
	RowValue,              "rowvalue",               STRING,  OBJ|TA,
	RowHeight,             "rowheight",              INTEGER, OBJ|TA,
	ColumnWidth,           "columnwidth",            INTEGER, OBJ|TA,
	ToggleType,            "toggletype",             STRING,  OBJ|TO,
	SeparatorType,         "separatortype",          STRING,  OBJ|SE,
};

static struct _shellTable {
	char *name;
	int index;
	unsigned int valid;
} shellTable[] = {
	"staticmenu",   SM, LA|PB|SE,
	"dynamicmenu",  DM, LA|PB|SE,
	"messageboard", MB, LA|ME|PB|SE,
	"commandboard", CB, LA|LI|ME|PB|TE|TA|FS|SE|SL|TO,
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
	"fileselect", FS,
	"separator",  SE,
	"slider",     SL,
	"toggle",     TO,
};
