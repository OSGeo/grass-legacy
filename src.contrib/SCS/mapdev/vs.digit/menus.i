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
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_global = {
    Glob_items,
    "Global",
    1,
    Help_global
};

struct Menu_item Mask_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head Global_mask = {
    Mask_items,
    "",
    1,
    Help_global
};

struct Menu_item Dig_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_digit = {
    Dig_items,
    "Digitize",
    1,
    Help_digitize
};

struct Menu_item Edit_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_edit = {
    Edit_items,
    "Edit",
    1,
    Help_edit
};

struct Menu_item Custom_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_custom = {
    Custom_items,
    "Customize",
    1,
    Help_custom
};

struct Menu_item Tool_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_tool = {
    Tool_items,
    "Toolbox",
    1,
    Help_tool
};

struct Menu_item Label_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_label = {
    Label_items,
    "Label",
    1,
    Help_label
};

struct Menu_item Window_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_window = {
    Window_items,
    "Window",
    1,
    Help_window
};

struct Menu_item Debug_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_debug = {
    Debug_items,
    "Debug",
    1,
    Help_debug
};

struct Menu_item Display_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_display = {
    Display_items,
    "Display",
    1,
    Help_display
};

struct Menu_item Color_items[] = {
	{ "",		0, 0 },
	{ NULL, 0, 0 }
};
struct Menu_head M_color = {
    Color_items,
    "Color",
    1,
    Help_color
};
