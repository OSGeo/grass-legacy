/*  
**
**  $Header: /water/sponsor/cerl/xgen/src/RCS/types.h,v 1.8 90/07/03 12:43:05 buehlerk Exp Locker: buehlerk $
**  $Log:	types.h,v $
 * Revision 1.8  90/07/03  12:43:05  buehlerk
 * automatic check in: Tue Jul  3 12:42:23 MDT 1990
 * 
 * Revision 1.7  90/07/03  09:39:49  buehlerk
 * automatic check in: Tue Jul  3 09:38:49 MDT 1990
 * 
 * Revision 1.6  90/06/04  15:51:25  buehlerk
 * automatic check in: Mon Jun  4 15:49:08 MDT 1990
 * 
 * Revision 1.5  90/05/21  16:01:07  buehlerk
 * automatic check in: Mon May 21 16:00:23 MDT 1990
 * 
 * Revision 1.4  90/05/18  16:00:13  buehlerk
 * automatic check in: Fri May 18 15:59:35 MDT 1990
 * 
 * Revision 1.3  90/05/15  17:09:09  buehlerk
 * automatic check in: Tue May 15 17:08:46 MDT 1990
 * 
 * Revision 1.2  90/05/14  16:05:56  buehlerk
 * automatic check in: Mon May 14 16:04:47 MDT 1990
 * 
 * Revision 1.1  90/03/08  16:53:00  buehlerk
 * automatic check in: Thu Mar  8 16:52:03 MST 1990
 * 
 * Revision 1.1  90/03/06  16:45:51  buehlerk
 * Initial revision
 * 
**
**/

/* 
**  Definition of the Value union. 
**/

typedef union _value {
    int            ival;
    double         dval;
    char *         cval;
    Boolean        bval;
} Value;

/* 
**  Definition of the Resource record.
**  The type field in a Resource structure will indicate which 
**  type is stored in the union. These types are described 
**  using the defines following the definition. A resource value
**  is always UNKNOWN at creation time, and is set once the value
**  is assigned. If at parse time a resource that should be an 
**  int, double, or Boolean, is found to be a variable ($XYZ)
**  then the variable flag is set. When the resource is accessed
**  it is expanded into the proper data type.
*/

typedef struct _resource {
    char *                name;     /* the name of the resource       */
    unsigned int          type;     /* the type of the resource value */
    Value                 val;      /* the actual resource value      */
	Boolean               variable; /* this is a variable             */
	char *                varValue; /* this is the string rep.        */
    struct _resource *    next;     /* the next resource              */
} Resource;

#define UNKNOWN    0

#define INTEGER    1
#define REAL       2
#define STRING     3
#define BOOLEAN    4

#define IsVariable(s) ((*(s) == '$')?(True):(False))

/* 
**  Definition of the Object record.
**  The type field in an Object structure will indicate which 
**  type of object is described. These types are ALWAYS described 
**  using the defines following the definition. 
*/

typedef struct _object {
    char *                name;      /* the name of the object */
    unsigned int          type;      /* the type of object     */
	Widget				  widget;    /* the object's widget id */
    Resource *            resources; /* the resource list      */
    struct _object *      next;      /* the next object        */
} InterfaceObject;

#define LABEL      1
#define MESSAGE    2
#define LIST       3
#define PUSHBUTTON 4
#define TEXTENTRY  5
#define TABLE      6
#define FILESELECT 7
#define SEPARATOR  8
#define SLIDER     9
#define TOGGLE     10

/* 
**  Definition of the Shell record.
**  The type field in a Shell structure will indicate which 
**  type of shell is described. These types are ALWAYS described 
**  using the defines following the definition. 
*/

typedef struct _shell {
    char *                name;      /* the name of the shell   */
	Widget	              widget;    /* the widget of the shell */
	Boolean               initial;   /* it's an initialshell    */
	Boolean               make;      /* it's a makeshell        */
	Boolean               popup;     /* it's popped up          */
    unsigned int          type;      /* the type of shell       */
    InterfaceObject *     objects;   /* the list of objects     */
    Resource *            resources; /* the resource list       */
    struct _shell *       next;      /* the next shell          */
} Shell;


#define STATICMENU   1
#define DYNAMICMENU  2
#define MESSAGEBOARD  3
#define COMMANDBOARD 4

/* Definition of the Environ record */

typedef struct _environment {
    char *                name;      /* the name of the environment */
    Shell *               shells;    /* the list of shells          */
    Resource *            resources; /* the resource list           */
    struct _environment * next;      /* the next environment        */
} Environ;

#define ENVIRONMENT 0
#define SHELL       1
#define OBJECT      2

/* Definition of the Command record. A KillBox will have this info.  */

typedef struct _command {
	char *			  path;    /* the main program to call before commandarg */
	char *            arglist; /* the commandarg argument list */
	char *            tablearg;/* the tablearg argument list */
	/* if !strcmp(sink,"null") the output goes to a editable text widget */
	Boolean           capture; /* do we capture the output ?       */
	char *            sink;    /* where stdout is to go            */
	char *	          tmpfile; /* the tmpfile created by tmpnam() */
	char *	          errfile; /* the errfile created by tmpnam() */
	Widget            widget;  /* widget id of the control box push button. */
	Boolean           dowait;  /* do we wait for a return ?        */
	int               pid;     /* the process id of the child      */
	struct _command * next;    /* the next command in the list */
} Command;

/* Definition of the MessageInfo record. */

typedef struct _message_info {
	int startpos;
	int endpos;
	struct _message_info *next;
} MessageInfo;

/* Definition of the FileParts record. */

typedef struct _fileparts {
	char *d; /* the directory part, including the last '/' */
	char *n; /* everything following the above, full file/directory name */
	char *b; /* the base name, = to name if no extension. */
	char *e; /* the extension including the '.' */
} FileParts;

/* Definition of the ToggleData record. */

typedef struct _toggledata {
	char *name;
	Widget widget;
	Boolean set;
	Boolean radioIgnoreOff;
	struct _toggledata *next;
} ToggleData;

/* Definition of the ToggleInfo record. */

typedef struct _toggleinfo {
	char *objectName;
	ToggleData *toggleData;
	struct _toggleinfo *next;
} ToggleInfo;

/* Definition of the ListData record. */

typedef struct _listdata {
	XmString item;
	char *valueString;
	Boolean selected;
	struct _listdata *next;
} ListData;

/* Definition of the ListInfo record. */

typedef struct _listinfo {
	char *objectName;
	ListData *listData;
	struct _listinfo *next;
} ListInfo;

/* Definition of ListType enumeration type */
typedef enum { 
	single, multiple, extended, browse
} ListType;

/* Definition of the Popup record */

typedef struct _popup {
	Widget  widget;         /* the widget */
	struct _popup * next;   /* the next popup data structure */
} PopUp;

/* Definition of StringType enumeration type */

typedef enum { 
	UnknownString, CommandString, DirectoryString, FileString 
} StringType;

/* defines for resource validity checking */ 

#define ENV (1L<<1)
#define SHL (1L<<2)
#define OBJ (1L<<3)

#define SM  (1L<<4)
#define DM  (1L<<5)
#define MB  (1L<<6)
#define CB  (1L<<7)

#define LA  (1L<<8)
#define ME  (1L<<9)
#define LI  (1L<<10)
#define PB  (1L<<11)
#define TE  (1L<<12)
#define TA  (1L<<13)
#define FS  (1L<<14)
#define SE  (1L<<15)
#define SL  (1L<<16)
#define TO  (1L<<17)

#define ALLOBJ (LA|ME|LI|PB|TE|TA|FS|SE|SL|TO)

