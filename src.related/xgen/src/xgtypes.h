/**********************************************************************
   types.h      - type definition header file
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

/*
 * *  Definition of the Value union.
 */

typedef union _value {
    int                             ival;
    double                          dval;
    char                           *cval;
    Boolean                         bval;
}                               Value;

/*
 * Definition of the WidgetListElement record. *
 * 
 * This is used to keep track of widgets in dynamic menus * so that
 * sensitivity can be changed... *
 * 
 */

typedef struct _widgetListElement {
    Widget                          widget;
    struct _widgetListElement      *next;
}                               WidgetListElement;

/*
 * *  Definition of the Resource record. *  The type field in a Resource
 * structure will indicate which *  type is stored in the union. These types
 * are described *  using the defines following the definition. A resource
 * value *  is always UNKNOWN at creation time, and is set once the value *
 * is assigned. If at parse time a resource that should be an *  int, double,
 * or Boolean, is found to be a variable ($XYZ) *  then the variable flag is
 * set. When the resource is accessed *  it is expanded into the proper data
 * type.
 */

typedef struct _resource {
    char                           *name;       /* the name of the resource       */
    unsigned int                    type;       /* the type of the resource
                                                 * value */
    Value                           val;        /* the actual resource value      */
    Boolean                         variable;   /* this is a variable             */
    char                           *varValue;   /* this is the string rep.        */
    struct _resource               *next;       /* the next resource              */
}                               Resource;

#define UNKNOWN    0

#define INTEGER    1
#define REAL       2
#define STRING     3
#define BOOLEAN    4

#define IsVariable(s) ((*(s) == '$')?(True):(False))

/*
 * *  Definition of the Object record. *  The type field in an Object
 * structure will indicate which *  type of object is described. These types
 * are ALWAYS described *  using the defines following the definition.
 */

typedef struct _object {
    char                           *name;       /* the name of the object */
    unsigned int                    type;       /* the type of object     */
    Widget                          widget;     /* the object's widget id */
    struct _object                 *objects;    /* the object list for a
                                                 * pulldown object */
    Resource                       *resources;  /* the resource list      */
    struct _object                 *next;       /* the next object        */
}                               InterfaceObject;

#define LABEL      1
#define MESSAGE    2
#define LIST       3
#define PUSHBUTTON 4
#define TEXTENTRY  5
#define TABLE      6
#define SEPARATOR  7
#define SLIDER     8
#define TOGGLE     9
#define PULLDOWN   10
#define MULTILINE  11

/*
 * *  Definition of the Shell record. *  The type field in a Shell structure
 * will indicate which *  type of shell is described. These types are ALWAYS
 * described *  using the defines following the definition.
 */

typedef struct _shell {
    char                           *name;       /* the name of the shell   */
    Widget                          widget;     /* the widget of the shell */
    Boolean                         initial;    /* it's an initialshell    */
    Boolean                         make;       /* it's a makeshell        */
    Boolean                         popup;      /* it's popped up          */
    unsigned int                    type;       /* the type of shell       */
    InterfaceObject                *objects;    /* the list of objects     */
    WidgetListElement              *dynObjects; /* a list of dynamic button
                                                 * widgets */
    Resource                       *resources;  /* the resource list       */
    struct _shell                  *next;       /* the next shell          */
}                               Shell;


#define MENU   1
#define COMMANDBOARD 2

/* Definition of the Environ record */

typedef struct _environment {
    char                           *name;       /* the name of the
                                                 * environment */
    Shell                          *shells;     /* the list of shells          */
    Resource                       *resources;  /* the resource list           */
    struct _environment            *next;       /* the next environment        */
}                               Environ;

#define ENVIRONMENT 0
#define SHELL       1
#define OBJECT      2

/* Definition of the Command record. A KillBox will have this info.  */

typedef struct _command {
    char                           *path;       /* the main program to call
                                                 * before commandarg */
    char                           *arglist;    /* the commandarg argument
                                                 * list             */
    char                           *tablearg;   /* the tablearg argument list               */
    char                           *buf;        /* the completely constructed
                                                 * command            */
    /* if !strcmp(sink,"null") the output goes to a editable text idget   */
    char                           *cinterp;    /* command interp with
                                                 * arguments              */
    Boolean                         interp;     /* Are we using something
                                                 * other than /bin/sh */
    char                           *activeshell;        /* name and command line
                                                         * options interactive
                                                         * program                     */
    Boolean                         capture;    /* do we capture the output ?               */
    Boolean                         notify;    /* do we notify the user when a 
                                                  background job is done  ?  */
    Boolean                         input;      /* do we have special input ?               */
    char                           *source;     /* where stdin is to come
                                                 * from              */
    char                           *sink;       /* where stdout is to go                    */
    char                           *tmpfile;    /* the tmpfile created by
                                                 * tmpnam()        */
    char                           *errfile;    /* the errfile created by
                                                 * tmpnam()        */
    Widget                          shell;      /* widget id of the parent
                                                 * shell.            */
    Widget                          widget;     /* widget id of the control
                                                 * box pushb.      */
    Boolean                         dowait;     /* do we wait for a return ?                */
    PIDTYPE                         pid;        /* the process id of the
                                                 * child              */
    int                            *err;        /* acceptable exit codes                    */
    int                             nerr;       /* number of acceptable exit
                                                 * codes          */
    struct _command                *next;       /* the next command in the
                                                 * list             */
}                               Command;

/* Definition of the MessageInfo record. */

typedef struct _message_info {
    int                             startpos;
    int                             endpos;
    struct _message_info           *next;
}                               MessageInfo;

/* Definition of the ToggleData record. */

typedef struct _toggledata {
    char                           *name;
    Widget                          widget;
    Boolean                         set;
    Boolean                         radioIgnoreOff;
    struct _toggledata             *next;
}                               ToggleData;

/* Definition of the ToggleInfo record. */

typedef struct _toggleinfo {
    char                           *objectName;
    ToggleData                     *toggleData;
    struct _toggleinfo             *next;
}                               ToggleInfo;

/* Definition of the ListData record. */

typedef struct _listdata {
    XmString                        item;
    char                           *valueString;
    Boolean                         selected;
    struct _listdata               *next;
}                               ListData;

/* Definition of ListType enumeration type */
typedef enum {
    singleSelect, multipleSelect, extendedSelect, browseSelect
}                               ListType;

/* Definition of the ListInfo record. */

typedef struct _listinfo {
    char                           *objectName;
    ListType                        listType;
    ListData                       *listData;
    struct _listinfo               *next;
}                               ListInfo;

/* Definition of the Popup record */

typedef struct _popup {
    Widget                          widget;     /* the widget */
    struct _popup                  *next;       /* the next popup data
                                                 * structure */
}                               PopUp;

/* Definition of StringType enumeration type */

typedef enum {
    UnknownString, CommandString, DirectoryString, FileString
}                               StringType;

/* defines for resource validity checking */

#define ENV (1L<<1)
#define SHL (1L<<2)
#define OBJ (1L<<3)

#define MU  (1L<<4)
#define CO  (1L<<5)

#define PU  (1L<<6)
#define LA  (1L<<7)
#define ME  (1L<<8)
#define LI  (1L<<9)
#define PB  (1L<<10)
#define TE  (1L<<11)
#define TA  (1L<<12)
#define SE  (1L<<13)
#define SL  (1L<<14)
#define TO  (1L<<15)
#define ML  (1L<<16)

#define ALLOBJ (PU|LA|ME|LI|PB|TE|TA|SE|SL|TO|ML)

/* defines for value validity checking */

#define IsPercent(x) ((fabs((x)) >= 0.0000001) && (((x) <= 100.0000001 )))

#define IsPositive(x) (fabs((x)) == (x))

/* data type checking enumeration typedef */
typedef enum _dataType {
    OnOff, Real, Int
}                               DataType;
