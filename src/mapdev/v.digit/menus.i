int Help_main ();
int Help_global ();
int Help_digitize ();
int Help_edit ();
int Help_label ();
int Help_window ();
int Help_custom ();
int Help_tool ();
int Help_debug ();
int Help_display ();
int Help_color ();

struct Menu_item Main_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_main = {
    Main_items,
    "Main",
    0,
    Help_main
};

struct Menu_item Glob_items[] = {
	{ "Digitize  ",  'D', 1 },
	{ "Edit  ",      'E', 1 },
	{ "Label  ",     'L', 1 },
	{ "Customize  ", 'C', 1 },
	{ "Toolbox  "  , 'T', 1 },
	{ "Window  ",    'W', 1 },
	{ "Help  ",      'H', 1 },
	{ "Zoom  ",      'Z', 1 },
	{ "Quit  ",      'Q', 1 },
	{ "*  ",         '*', 1 },
	{ "!  ",    	 '!', 1 },
/* This next one is currently only needed for SUNVIEW driver that needs to be
** bumped when the video is overwritten or the screen saver goes off
*/
	{ "^  ",    	 '^', 1 },  /* bump the video driver */
	{ "",            '-', 1 },  /* debug menu */
	{ NULL,      0, 0 }
};
struct Menu_head M_global = {
    Glob_items,
    "Global",
    1,
    Help_global
};

struct Menu_item Mask_items[] = {
	{ "Digitize  ",  'D', 1 },
	{ "Edit  ",      'E', 1 },
	{ "Label  ",     'L', 1 },
	{ "Customize  ", 'C', 1 },
	{ "Toolbox  "  , 'T', 1 },
	{ "Window  ",    'W', 1 },
	{ "Redraw  ",    'R', 1 },
	{ "Help  ",      'H', 1 },
	{ "Quit  ",      'Q', 1 },
	{ "",            '*', 1 },
	{ "",            '-', 1 },
	{ NULL,      0, 0 }
};
struct Menu_head Global_mask = {
    Mask_items,
    "",
    1,
    Help_global
};

struct Menu_item Dig_items[] = {
    { "",            		 0,  0 },
    { "<space> Digitize",       ' ', 1 },
    { "m - Toggle MODE",        'm', 1 },
    { "t - Toggle TYPE",        't', 1 },
    { "l - Auto Label",		'l', 1 },
    { "q - Quit to main menu",  'q', 1 },
    { "",                       '*', 1 },
    { NULL,      		 0,  0 }
};
struct Menu_head M_digit = {
    Dig_items,
    "Digitize",
    1,
    Help_digitize
};

struct Menu_item Edit_items[] = {
    {  "r - Remove line",  		  'r', 1 },
    {  "i - Remove site", 		  'i', 1 },
    {  "s - Snap line to node",  	  's', 1 },
#ifdef SCS_MODS
    {  "S - Snap line to line", 	  'S', 1 },
#endif /* SCS_MODS */
    {  "b - Break a line", 		  'b', 1 },
    {  "m - Move a point", 		  'm', 1 },
    {  "M - Move a line or site", 	  'M', 1 },
    {  "t - Re-type a line  (AREA/LINE)", 't', 1 },
    {  "d - Display nodes in threshold",  'd', 1 },
    {  "R - Remove BLOCK of lines",  	  'R', 1 },
    {  "v - Smooth poly-line",            'v', 1 },
    { "",				   0,  0 },
    {  "q - Quit to main menu", 	  'q', 1 },
    { NULL,      0, 0 }
};
struct Menu_head M_edit = {
    Edit_items,
    "Edit",
    1,
    Help_edit
};

struct Menu_item Custom_items[] = {
    { "d - Set digitizing threshold",       	'd', 1 },
    { "s - Set snapping threshold",         	's', 1 },
    { "F - Toggle flexible grey scale", 	'F', 1 },  /*OHLER*/
    { "b - Toggle BEEP",   		   	'b', 1 },
    { "a - Toggle Auto Window",   		'a', 1 },
    { "w - Toggle Windowing device",		'w', 1 },
    { "p - Toggle Point device",		'p', 1 },
    { "z - Toggle Digitizing device",		'z', 1 },
    { "O - Select An Overlay vector Map",	'O', 1 },
    { "B - Select A Backdrop CELL Map",		'B', 1 },
/*  { "L - Select A Label Map",			'L', 0 }, */
/*  { "",					 0,  0 }, */
/*    { "r - Toggle Remove Block display",	'r', 1 }, */
    { "D - Enter Display Options Menu",   	'D', 1 },
    { "C - Enter Color Options Menu",   	'C', 1 },
    { "m - Toggle Auto-smoothing",		'm', 1 },
    { "q - Return from whence we came",     	'q', 1 },
    { "t - Toggle TERSE mode",   	   	't',-1 },
/*  { "l - Toggle labeling devices",   		'l', 0 }, */
/*  { "c - Compress file on write",   		'c', 0 }, */
    { NULL,      				 0,  0 }
};
struct Menu_head M_custom = {
    Custom_items,
    "Customize",
    1,
    Help_custom
};

struct Menu_item Tool_items[] = {
/*  { "c - Create a Cell file",			'c', -1 }, */
/*  { "d - Display the Cell file",		'd', -1 }, */
    { "",				 	 0,  0 },
    { "w - Write out session",			'w', 1 },
    { "R - Register map",			'R', 1 },
    { "N - Build Neat line",			'N', 1 },
    { "",				 	 0,  0 },
    { "u - Display Unlabeled Areas",		'u', 1 },
    { "o - Display Open area lines",		'o', 1 },
    { "d - Display Duplicate lines",		'd', 1 },
    { "",				 	 0,  0 },
    { "n - Display Node lines",			'n', 1 },
    { "i - Display Islands",		 	'i', 1 },
    { "",				 	 0,  0 },
    { "q - return from whence we came",		'q', 1 },
    { "g - Garbage collection (Free up memory)",'g', -1 },
    { "e - Escape to shell",			'e', -1 },
    { NULL,      0, 0 }
};
struct Menu_head M_tool = {
    Tool_items,
    "Toolbox",
    1,
    Help_tool
};

struct Menu_item Label_items[] = {
    { "a - Label Areas", 		'a', 1 },
    { "l - Label Lines", 		'l', 1 },
    { "s - Label Sites", 		's', 1 },
    { "",				 0 , 0 },
    { "A - Un-Label Areas", 		'A', 1 },
    { "L - Un-Label Lines", 		'L', 1 },
    { "S - Un-Label Sites", 		'S', 1 },
    { "",				 0 , 0 },
    { "B - Bulk Label Remaining Lines",	'B', 1 },
    { "",				 0,  0 },
    { "h - Highlight Lines of category #",'h', 1 },
    { "d - Display Areas of category #",'d', 1 },
    { "",				 0 , 0 },
    { "q - Return to main menu", 	'q', 1 },
    { "m - Label Multiple Lines",	'm', 1 },
    { "M - Un-Label Multiple Lines",	'M', 1 },
    { "",				 0 , 0 },
    { "c - Label Contours",		'c', 1 },
    { "i - Contour interval:",		'i', 1 },
    { NULL,      0, 0 }
};
struct Menu_head M_label = {
    Label_items,
    "Label",
    1,
    Help_label
};

struct Menu_item Window_items[] = {
    { "a - Show area markers",	 	'a', 1 },
    { "A - Show area labels", 		'A', 1 },
    { "",				 0,  0 },
    { "i - Show lines", 		'i', 1 },
    { "l - Show labeled lines", 	'l', 1 },
    { "L - Show line labels", 		'L', 1 },
    { "U - Show unlabeled lines", 	'U', 1 },
    { "",				 0,  0 },
    { "s - Show sites", 		's', 1 },
    { "S - Show site labels", 		'S', 1 },
    { "n - Show nodes", 		'n', 1 },
    { "",				 0,  0 },
    { "q - Return from whence we came",	'q', 1 },
    { "",				 0,  0 },
    { "",				 0,  0 },   /* line 14 */
    { "W - Define new window", 		'W', 1 },
    { "C - Clear window", 		'C', 1 },
    { "",				 0,  0 },
    { "c - Display scale", 		'c', 1 },	/* toolbox ? */
    { "w - Where am I", 		'w', 1 },
    { "O - Display Overlay Map", 	'O', 0 },  /* starts w/ overlay off */
    { "B - Display Backdrop CELL Map", 	'B', 0 },
    { "",				 0,  0 },
    { "t - Thresholds", 		't', -1 },
    { "I - Show Islands", 		'I', -1 },
    { "u - Unlabeled Areas", 		'u', -1 },
/*
    { "P - Previous window", 		'P', 0 },
*/
    { NULL,      0, 0 }
};
struct Menu_head M_window = {
    Window_items,
    "Window",
    1,
    Help_window
};

struct Menu_item Debug_items[] = {
    { "l - Display line info", 		'l', 1 },
    { "n - Display node info", 		'n', 1 },
    { "a - Display area info", 		'a', 1 },
/*	no longer needed
    { "s - Show each area", 		's', 0 },
*/
    { "",				 0,  0 },
    { "L - Find line",	 		'L', 1 },
    { "N - Find node", 			'N', 1 },
    { "A - Find area", 			'A', 1 },
    { "I - Find isle", 			'I', 1 },
    { "",				 0,  0 },
    { "d - Display Node-lines",		'd', 1 },
    { "",				 0,  0 },
    { "q - Return from whence we came",	'q', 1 },
    { NULL,      0, 0 }
};
struct Menu_head M_debug = {
    Debug_items,
    "Debug",
    1,
    Help_debug
};

struct Menu_item Display_items[] = {
    { "a - Area Labels",		'a', 1 },
    { "l - Line Labels",		'l', 1 },
    { "s - Site Labels",		's', 1 },
    { "m - Area Markers",		'm', 1 },
    { "A - Area Border lines",		'A', 1 },
    { "L - Labeled lines",		'L', 1 },
    { "i - Lines",			'i', 1 },
    { "S - Sites",			'S', 1 },
    { "n - Nodes",			'n', 1 },
    { "p - Points in lines",		'p', 1 },
    /*
    { "O - Overlay map",		'O', 1 },
    { "B - CELL Backdrop",		'B', 1 },
    */
    { "",				 0,  0 },
    { "r - Reset Defaults", 		'r', 1 },
    { "",				 0,  0 },
    { "q - Return from whence we came",	'q', 1 },  /* line 14 */
    { "U - Update the monitor", 	'U', -1 }, /* no code for this yet */
    { "x - No Changes", 		'x', -1 },
    { NULL,      0, 0 }
};
struct Menu_head M_display = {
    Display_items,
    "Display",
    1,
    Help_display
};

struct Menu_item Color_items[] = {
    { "a - Areas",			'a', 1 },
    { "l - Lines",			'l', 1 },
    { "s - Sites",			's', 1 },
    { "A - Labeled areas",		'A', 1 },
    { "L - Labeled lines",		'L', 1 },
    { "S - Labeled Sites",		'S', 1 },
    { "1 - Nodes w/ 1 line",		'1', 1 },
    { "2 - Nodes w/ 2 or more lines",	'2', 1 },
    { "h - Highlight",			'h', 1 },
    { "B - Background",			'B', 1 },
    { "O - Overlay map",		'O', 1 },
    { "r - Reset Defaults", 		'r', 1 },
    { "", 				 0 , 0 },
    { "q - Return from whence we came",	'q', 1 },   /* line 14 */
    { "U - Update the monitor", 	'U', -1 },
    { "x - No Changes",			'x', -1 },
    { NULL,      0, 0 }
};
struct Menu_head M_color = {
    Color_items,
    "Color",
    1,
    Help_color
};

