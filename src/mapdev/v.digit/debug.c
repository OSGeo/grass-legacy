/*
**  Written by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "curses.h"
#include "digit.h"
#include "dig_curses.h"
#include "Map_proto.h"
#include "gis.h"
#include "debug.h"
#include "raster.h"
#include "display_area.h"
#include "display_line.h"
#include "display_node.h"
#include "local_proto.h"

static int d_line_info(struct Map_info *);
static int m_line_info(struct Map_info *, int);
static int d_area_info(struct Map_info *);
static int d_node_info(struct Map_info *);
static int d_find_line(struct Map_info *);
static int d_find_area(struct Map_info *);
static int d_find_node(struct Map_info *);
static int d_show_isles(struct Map_info *);
static int d_show_areas(struct Map_info *);
static int d_show_lines(struct Map_info *);
static int d_show_nodes(struct Map_info *);
static int m_node_info(struct Map_info *, int);

/*
**  Debug menu 
**
**  driver routine for Debug menu
**  these routines are intended for programmer debugging, 
**  but could be usefull to users
**
**  Contains a number of routines that match the menu options, as
**  well as some related routines for displaying specific information
**
**  Written by Dave Gerdes 4 1988
*/

int Debug (void)
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;

    Pass = 0;
    Set_Global (MG_DIGIT, OFF);
    Set_Global (MG_EDIT, OFF);
    Set_Global (MG_DEBUG, OFF);
    Set_Global (MG_LABEL, OFF);
    Set_Global (MG_CUSTOM, OFF);
    Set_Global (MG_TOOL, OFF);
    Set_Global (MG_QUIT, OFF);
    Set_Global (MG_WINDOW, OFF);

    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	Write_generic_win(&M_debug);

	if ((command = get_menu_command (&M_debug, &chr)) > 0)
	{
	    switch(command) {
		case MBC_LINE:
		    d_line_info (CMap);
		    break;
		case MBC_AREA:
		    d_area_info (CMap);
		    break;
		case MBC_NODE:
		    d_node_info (CMap);
		    break;
		case MBC_SHOWAREAS:
		    d_show_areas (CMap);
		    break;
		case MBC_FINDLINE:
		    d_show_lines (CMap);
		    break;
		case MBC_FINDAREA:
		    d_show_areas (CMap);
		    break;
		case MBC_FINDISLE:
		    d_show_isles (CMap);
		    break;
		case MBC_FINDNODE:
		    d_show_nodes (CMap);
		    break;
		case MBC_NODELINES:
		    node_lines (CMap);
		    break;
		case MBC_QUIT:
		    Pass = 0;
		    goto DBUG_END;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_tool)) > 0)
	    {
		Pass = ret;	/* should never get here for window () */
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
DBUG_END:
    Set_Global (MG_DIGIT, ON);
    Set_Global (MG_EDIT, ON);
    Set_Global (MG_DEBUG, ON);
    Set_Global (MG_LABEL, ON);
    Set_Global (MG_CUSTOM, ON);
    Set_Global (MG_TOOL, ON);
    Set_Global (MG_QUIT, ON);
    Set_Global (MG_WINDOW, ON);
    return (Pass);
}

static int m_line_prev;

static int d_line_info (struct Map_info *map)
{
    int line;

    line = 0;
    m_line_prev = 0;	/* for m_line_info -  display_llabel() */
    Clear_info ();
    while ((line = find_line_with_mouse (-1, "Choose Line:", m_line_info) > 0))
    {
	Clear_info ();
	if (Disp_llabels) display_llabel (map, line);
	display_line (map->Line[line].type, &Gpoints, line, map);
    }
    return 0;
}

static int m_line_info (struct Map_info *map, int line)
{
    char buf[500];
    P_LINE *Line;

    if (m_line_prev)
	if (Disp_llabels)
	    display_llabel (map, m_line_prev);
    if (line > 0)
    {
	Line = &(map->Line[line]);
	sprintf (buf, "#%4d:  N1 %3d N2 %3d  Left %3d Right %3d",
	    line, Line->N1, Line->N2, Line->left, Line->right);
	Write_info (2, buf);
	if (Line->att)
	{
	    P_ATT *Att;

	    if (Disp_llabels)
	    {
		highlight_llabel (map, line);
		m_line_prev = line;
	    }
	    Att = &(map->Att[Line->att]);
	    sprintf (buf, "  Att %3d (index %3d)   Category %3d  X %10.3f Y %10.3f",
		Line->att, Att->index, Att->cat, Att->x, Att->y);
	}
	else
	    sprintf (buf, " Line is NOT labeled");
	Write_info (3, buf);
    }

    return 0;
}

static int d_area_info (struct Map_info *map)
{
    int area;
    double x, y;
    char buf[100];

    area = 0;
    Clear_info ();
    while (1)
    {
	new_point_with_mouse (&x, &y, "Choose a point in Area:"); 
	if (area)
	{
	    
	    if (Disp_labels)
		display_area_label (area, map);
	    else
		undisplay_area_label (area, map);
	    if (Disp_outline)
		display_area (area, map);
	    else
		reset_area (area, map);
	}
	_Clear_base ();
	if (x == 0.0 && y == 0.0)
	{
	    return (0);
	}
	_Clear_info ();
	if ((area = dig_point_to_area (map, x, y)))
	{
	    if (Disp_labels)
		highlight_area_label (area, map);
	    highlight_area (area, map);
	    sprintf (buf, "Area# %d", area);
	    Write_info (1, buf);
	}
	else
	{
	    Write_info (1, "No Area Found");
	}
    }
}

static int d_node_info (struct Map_info *map)
{
/*
    while (1)
    {
	Clear_info ();
    }
*/

    return 0;
}

static int d_find_line (struct Map_info *map)
{
    int num;
    char buf[1024];

    num = 0;
    while (1)
    {
	if (num)
	{
	    sprintf (buf, "Line# %d", num);
	    Write_info (1, buf);
	}
	Write_info (2, "Enter Line number to find (0 to end)[Default=Next]: ");
	Get_curses_text (buf);
	if (num && LINE_ALIVE (&(map->Line[num])))
	    display_line (map->Line[num].type, &Gpoints, num, map);

	G_squeeze (buf); dig_rmcr (buf);

	if (!strlen (buf))
	    num++;
	else
	{
	    if (!(num = atoi (buf)))
		break;
	    if (num < 0) num = abs (num);
	}

	if (num > map->n_lines)
	{
	    Write_info (3 ,"Line does not exist");
	    num = 0;
	    sleep (2);
	    continue;
	}
	if (!LINE_ALIVE (&(map->Line[num])))
	{
	    Write_info (3, "Line has been deleted");
	    sleep (2);
	    continue;
	}
	if(0 > V1_read_line(map, &Gpoints, map->Line[num].offset))
	{
	    Write_info (3, "Error reading line information");
	    sleep (2);
	    return (0);
	}
	highlight_line (map->Line[num].type, &Gpoints, num, map);
	V_flush ();
    }

    return 0;
}

static int d_find_area (struct Map_info *map)
{
    int num;
    char buf[1024];

    num = 0;
    while (1)
    {
	Clear_info ();
	if (num)
	{
	    sprintf (buf, "Area# %d", num);
	    Write_info (1, buf);
	}
	Write_info (2, "Enter Area number to find (0 to end): ");
	Get_curses_text (buf);
	if (num)
	    reset_area (num, map);
	if (!(num = atoi (buf)))
	    break;
	if (num < 0) num = abs (num);

	if (num > map->n_areas)
	{
	    Write_info (3, "Area does not exist");
	    sleep (2);
	    continue;
	}
	if (!AREA_ALIVE (&(map->Area[num])))
	{
	    Write_info (3, "Area has been deleted");
	    sleep (2);
	    continue;
	}

	highlight_area (num, map);
	V_flush ();
    }
    return 0;
}

static int d_find_node (struct Map_info *map)
{
    P_NODE *Node;
    int num;
    char buf[1024];

    num = 0;
    while (1)
    {
	Clear_info ();
	if (num)
	{
	    sprintf (buf, "Node# %d   X %f (%8p)  Y %f (%8p)", num,
                     Node->x, &Node->x, Node->y, &Node->y);
	    Write_info (1, buf);
	}
	Write_info (2, "Enter Node number to find (0 to end): ");
	Get_curses_text (buf);
	if (num)
	{
	    R_standard_color (dcolors[dig_node_color (Node->n_lines)]);
	    Blot (&(Node->x), &(Node->y));
	}
	if (!(num = atoi (buf)))
	    break;
	if (num < 0) num = abs (num);

	if (num > map->n_nodes)
	{
	    Write_info (3 ,"Node does not exist");
	    sleep (2);
	    continue;
	}

	Node = &(map->Node[num]);
	if (!NODE_ALIVE (Node))
	{
	    Write_info (3, "Node has been deleted");
	    sleep (2);
	    continue;
	}
	R_standard_color (dcolors[CLR_HIGHLIGHT]);	/* hightlight it */
	Blot (&(Node->x), &(Node->y));
    }
    return 0;
}

static int d_show_isles ( /*ISLE*/
    struct Map_info *map
)
{
    int i;
    register int prev, x;
    char buf[1024];
    char text[2048];

    _Clear_info ();
    sprintf (text, "Total isles: %d", map->n_isles);
    Write_info (4, text);
    prev = 0;
    for (i = 1 ; i <= map->n_isles ; i++)
    {
	if (prev)
	{
	    reset_isle (prev, map);
	}
	if (ISLE_ALIVE (&(map->Isle[i])))
	{
	    prev = i;
	    highlight_isle (i, map);

	    sprintf (text, "Isle %d  Area: %d  N_lines: %d", i, map->Isle[i].area, map->Isle[i].n_lines);
	    Write_info (1, text);

	    strcpy (text, "  ");
	    for (x = 0 ; x < map->Isle[i].n_lines ; x++)
	    {
		sprintf (buf, "%d ", map->Isle[i].lines[x]);
		strcat (text, buf);
	    }
	    Write_info (2, text);
	}
	else
	    prev = 0;

	Get_curses_text (buf);
	if (*buf && *buf == 'q')	/* 'q' quits */
	    break;
	if (strlen(buf) && *buf >= '1' && *buf <= '9')
	{
	    i = atoi (buf) - 1;
	}

	if (i < 0) i = 0;
	if (i > map->n_isles) i = map->n_isles;
    }
    if (prev)
	reset_isle (prev, map);
    return (0);
}

static int d_show_areas (struct Map_info *map)
{
    int i;
    register int prev, x;
    int unlabeled;
    char buf[1024];
    char text[2048];

    unlabeled = 0;
    _Clear_info ();
    sprintf (text, "Total areas: %d", map->n_areas);
    Write_info (4, text);
    prev = 0;
    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (unlabeled && AREA_LABELED (&(map->Area[i])))
	    continue;
	if (prev)
	{
	    reset_area (prev, map);
	}
	if (AREA_ALIVE (&(map->Area[i])))
	{
	    prev = i;

	    if (Auto_Window && area_outside_window (&(map->Area[i])))
	    {
		P_AREA *Area;

		Area = &(map->Area[i]);
		move_window (Area->N, Area->S, Area->E, Area->W, 1);
	    }

	    highlight_area (i, map);

	    sprintf (text, "Area %d  Att: %d  N_lines: %d", i, map->Area[i].att ? map->Att[map->Area[i].att].cat : 1, map->Area[i].n_lines);
	    Write_info (1, text);

	    strcpy (text, "  ");
	    for (x = 0 ; x < map->Area[i].n_lines ; x++)
	    {
		sprintf (buf, "%d ", map->Area[i].lines[x]);
		strcat (text, buf);
	    }
	    Write_info (2, text);
	}
	else
	    prev = 0;

	Get_curses_text (buf);
	/* if user types 'u' display next unlabeled area */
	if (*buf && *buf == 'u')  
	    unlabeled = 1;
	if (*buf && *buf == 'q')	/* 'q' quits */
	    break;
	if (strlen(buf) && *buf >= '1' && *buf <= '9')
	{
	    i = atoi (buf) - 1;
	    unlabeled = 0;	/* reset looking for unlabeled's */
	}

	if (i < 0) i = 0;
	if (i > map->n_areas) i = map->n_areas;
    }
    if (prev)
	reset_area (prev, map);
    return (0);
}

static int d_show_lines (struct Map_info *map)
{
    register int i;
    register int prev;
    char buf[1024];
    char text[2048];

    _Clear_info ();
    sprintf (text, "Total lines: %d", map->n_lines);
    Write_info (4, text);
    prev = 0;
    m_line_prev = 0;
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (prev)	/* reset the previously highlit */
	{
	    display_line (map->Line[prev].type, &Gpoints, prev, map);
	    if (Disp_llabels)
		display_llabel (map, prev);
	}

	m_line_info (map, i);	/* show line info on lines 2,3 */
	if (LINE_ALIVE (&(map->Line[i])))
	{
	    prev = i;
	    if(0 > V1_read_line (map, &Gpoints, map->Line[i].offset))
	    {
		Write_info (3, "Error reading line information");
		sleep (2);
		return (0);
	    }

	    if (Auto_Window && line_outside_window (&(map->Line[i])))
	    {
		P_LINE *Line;

		Line = &(map->Line[i]);
		move_window (Line->N, Line->S, Line->E, Line->W, 1);
	    }

	    highlight_line (map->Line[i].type, &Gpoints,i, map);
	}
	else
	    prev = 0;

	sprintf(text,"Enter <CR> for next line, # of desired line, 'q' quit: ");
	Write_info (1, text);


	Get_curses_text (buf);
	if (*buf && *buf == 'q')		
	    break;
	if (strlen(buf) && *buf >= '1' && *buf <= '9')
	    i = atoi (buf) - 1;

	if (i < 0) i = 0;
	if (i > map->n_lines) i = map->n_lines;
    }
    if (prev)
    {
	display_line (map->Line[prev].type, &Gpoints, prev, map);
	if (Disp_llabels)
	    display_llabel (map, prev);
    }
    return (0);
}

static int d_show_nodes (struct Map_info *map)
{
    int i;
    register int prev, x;
    char buf[1024];
    char text[2048];

    sprintf (text, "Total nodes: %d", map->n_nodes);
    Write_info (4, text);
    prev = 0;
    for (i = 1 ; i <= map->n_nodes ; i++)
    {
	if (prev)	/* reset the previously highlit */
	{
	    display_node (prev, map);
	}

	m_node_info (map, i);	/* show node info on nodes 2,3 */
	if (NODE_ALIVE (&(map->Node[i])))
	{
	    prev = i;
	    highlight_node (i, map);
	}
	else
	    prev = 0;

	sprintf(text,"Enter <CR> for next node, # of desired node, 'q' quit: ");
	Write_info (1, text);

	Get_curses_text (buf);
	if (*buf && *buf == 'q')		
	    break;
	if (strlen(buf) && *buf >= '1' && *buf <= '9')
	    i = atoi (buf) - 1;

	if (i < 0) i = 0;
	if (i > map->n_nodes) i = map->n_nodes;
    }
    _Clear_info ();
    if (prev)
    {
	display_node (prev, map);
    }
    return (0);
}

static int m_node_info (struct Map_info *map, int node)
{
    char buf[500];
    char text[500];
    P_NODE *Node;
    register int x;

    if (node > 0)
    {
	Node = &(map->Node[node]);
	sprintf (buf, "#%4d:  Num lines: %d  X: %12.2f  Y: %12.2f",
	    node, Node->n_lines, Node->x, Node->y);
	Write_info (2, buf);

	strcpy (text, "  ");
	sprintf (text, "HEX: %8lx %8lx  Lines:  ", (long int)Node->x, (long int)Node->y);
	for (x = 0 ; x < map->Node[node].n_lines ; x++)
	{
	    sprintf (buf, "%d ", map->Node[node].lines[x]);
	    strcat (text, buf);
	}
	Write_info (3, text);
    }
    return (0);
}
