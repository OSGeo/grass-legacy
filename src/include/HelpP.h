/* $Log: HelpP.h,v $
 * Revision 0.0  1992/05/05  14:56:43  sink
 * auto checkin: Tue May  5 09:56:43 CDT 1992
 *
 * Revision 0.0  1992/03/08  17:56:25  kurt
 * auto checkin: Sun Mar  8 11:56:25 CST 1992
 *
 * Revision 1.3  92/02/21  14:38:24  jinyuan
 * shink and wrap -- 2nd iteration
 * 
 * Revision 1.2  92/02/21  10:43:56  jinyuan
 * shink and warp .... 1st iteration
 * 
 * Revision 1.1  92/02/14  11:35:29  jinyuan
 * Initial revision
 * 
 * Revision 1.7  92/02/14  09:58:48  jinyuan
 * *** empty log message ***
 * 
 * Revision 1.6  92/02/13  15:42:08  jinyuan
 *  */

#ifndef _HelpP_h
#define _HelpP_h

#include "Help.h"
#include <InteractP.h>

#define XmHELP_BIT (52)

#define REFERENCE 1
#define GLOSSARY 2
#define MANUAL 3

#define REGULAR 4
#define HOTWORD 5

/*  New fields for the Help widget class record  */

typedef struct
{
    caddr_t extension;      /* Pointer to extension record */
} HelpClassPart;


/* Full class record declaration */

typedef struct _HelpClassRec
{
   CoreClassPart               core_class;
   CompositeClassPart          composite_class;
   ConstraintClassPart         constraint_class;
   XmManagerClassPart          manager_class;
   XmBulletinBoardClassPart    bulletin_board_class;
   InteractorClassPart         interactor_class;
   HelpClassPart               help_class;
} HelpClassRec;

externalref HelpClassRec helpClassRec;

/* New fields for the Help widget record */

typedef struct _Text {
   Widget		text;		/* the widget id with this hot word */
   Dimension	x;			/* where should this label be */
   Dimension	y;
   int			type;		/* a regular word or a hot word */
} Text ;

typedef struct _HelpPart
{
   Widget			scr_win;			/* scrolled window */
   Widget			draw_area;			/* drawing area */
   String		    help_file;			/* where is the help file */
   Pixel			regular_fg;
   Pixel			hotword_fg;
   XmFontList 		regular_fontlist; 	/* fontlist for regular text */
   XmFontList 		hotword_fontlist;	/* fontlist for hot words */
   Text		 		*words;				/* text words */
   int				fontheight;			/* height for each line */
   int				num_words;			/* number of text words */
   Dimension		width;				/* window width */
   Dimension		height;				/* window height */
   Boolean		dismiss_only;			/* dismiss only mode */
} HelpPart, HelpPartPtr;

typedef struct _Hot {
   HelpWidget parent;
   Boolean dismissOnly;
   char *buf;
   char *where;
   int type;
} Hot;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _HelpRec
{
    CorePart	            core;
    CompositePart           composite;
    ConstraintPart          constraint;
    XmManagerPart           manager;
    XmBulletinBoardPart     bulletin_board;
    InteractorPart          interactor;
    HelpPart                help;
} HelpRec;


/* Access macros */

#define H_HelpFile(w) 			(((HelpWidget) (w))->help.help_file)
#define H_RegularFontList(w)	(((HelpWidget) (w))->help.regular_fontlist)
#define H_HotwordFontList(w)	(((HelpWidget) (w))->help.hotword_fontlist)
#define H_RegularForeground(w)	(((HelpWidget) (w))->help.regular_foreground)
#define H_HotwordForeground(w)	(((HelpWidget) (w))->help.hotword_foreground)
#define H_Width(w)				(((HelpWidget) (w))->help.width)
#define H_Height(w)				(((HelpWidget) (w))->help.height)
#define H_ScrolledWin(w) 		(((HelpWidget) (w))->help.scr_win)
#define H_DrawArea(w)  			(((HelpWidget) (w))->help.draw_area)

#endif /* _HelpP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
