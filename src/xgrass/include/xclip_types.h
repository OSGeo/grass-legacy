/**********************************************************************
   types.h      - type definition header file
 *********************************************************************/
#ifndef _XCLIP_TYPES_H

#define _XCLIP_TYPES_H

/*
 * *  Definition of the Value union.
 */

#define XC_UNSPECIFIED 0

#define XC_FLAG   1
#define XC_PARM   2
#define XC_DIALOG 3

#define XC_DB_TYPE_UNKNOWN           0  /* unknown... */
#define XC_DB_TYPE_RASTER            1
#define XC_DB_TYPE_ASCII_DLG         2
#define XC_DB_TYPE_DLG               3
#define XC_DB_TYPE_ASCII_VECTOR      4
#define XC_DB_TYPE_VECTOR            5
#define XC_DB_TYPE_SITES             6
#define XC_DB_TYPE_REGION            7
#define XC_DB_TYPE_ICON              8
#define XC_DB_TYPE_LABEL             9
#define XC_DB_TYPE_IMAGE_GROUP       10
#define XC_DB_TYPE_IMAGE_SUBGROUP    11
#define XC_DB_TYPE_USER_DEFINED      12

#define XC_DB_STATUS_OLD             0
#define XC_DB_STATUS_NEW             1

typedef struct _database_element {
    int                             type;       /* one of the above... */
    char                           *desc;       /* e.g., name@mapset, ... */
    char                            status;     /* 0 = old, 1 = new */
}                               XCDBElement;

#define XC_TYPE_UNKNOWN              0  /* unknown... */
#define XC_TYPE_CHARACTER            1  /* -> value.cval */
#define XC_TYPE_INTEGER              2  /* -> value.ival */
#define XC_TYPE_FLOAT                3  /* -> value.dval */
#define XC_TYPE_DOUBLE               4  /* -> value.dval */
#define XC_TYPE_LOGICAL              5  /* -> value.bval */
#define XC_TYPE_FILENAME             6  /* -> value.cval */
#define XC_TYPE_ENUMERATE            7  /* -> value.cval */
#define XC_TYPE_DB_ELEMENT           8  /* -> value.dbval */

typedef union _value {
    int                             ival;
    double                          dval;
    char                           *cval;
    Boolean                         bval;
    XCDBElement                     dbval;
}                               XCValue;

typedef struct _require_data {
    Boolean                         isFlag;
    char                           *name;
    struct _require_data           *next;
}                               XCRequireData;

typedef struct _file_data {
    FILE                           *fp;
    char                           *filename;
    char                           *basename;
}                               XCFileData;

typedef struct _flag_data {
    Widget                          toggle;       /* Widget id of toggle used
                                                   * to get data */
    Boolean                         flagSet;      /* True = yes, False = no */
    Boolean                         answer;       /* True = on, False = off */
    char                           *desc;         /* handle used in script */
    char                           *key;          /* key used for argString */
    char                           *help;         /* help string */
    Boolean                         def;          /* default value */
    XCFileData                      helpFile;     /* help found in this file */
    Boolean                         precludes;    /* precludes other flags or
                                                   * parms */
    XCRequireData                   preclude;     /* what is the nature of the
                                                   * dependency? */
    Boolean                         requirements; /* depends on other
                                                   * parms or flags */
    XCRequireData                   requires;     /* what is the nature of the
                                                   * dependency? */
}                               XCFlagData;

typedef struct _type_modifier {
    int                             type;
    char                           *value;

    /* modifier data is parsed by XcModifierParse into data elements */

    /* modifier for enumerated data */
    int                             enumNItems;
    char                          **enumStrings;
    XmStringTable                   enumTable;

    /* modifier for integer data */
    /* if it exists we use a scale, has form "min:max:start" */

    long                            min, max, start;

    /* modifier for double or float data */
    /* if it exists we use a scale, has form "min:max:start:decimalpoints" */

    long                            decimalPoints;

    /* modifier for logical data */
    /* must be "onString:offString" */

    char                           *onString;
    char                           *offString;

}                               XCTypeModifier;

#define XC_SELECT_FALSE         0
#define XC_SELECT_UNDETERMINED  1
#define XC_SELECT_TYPE          2
#define XC_SELECT_TYPE_OPT      3

typedef struct _parm_data {
    Widget                          widget;       /* Widget id of widget used
                                                   * to get data */
    Widget                          scale_toggle; /* Widget id of widget used
                                                   * to toggle scale */
    Boolean                         togSet;       /* True = yes, False = no */
    Boolean                         parmSet;      /* True = yes, False = no */
    Boolean                         isInput;      /* this is an input parm */
    Boolean                         multiple;     /* for db elements and files,
                                                   * more than one? */
    char                           *desc;         /* handle used in script, and
                                                   * argString */
    char                           *help;         /* help string */
    XCFileData                      helpFile;     /* help found in this file */
    Boolean                         precludes;    /* precludes other flags or
                                                   * parms */
    XCRequireData                   preclude;     /* what is the nature of the
                                                   * dependency? */
    Boolean                         requirements; /* depends on other
                                                   * parms or flags */
    XCRequireData                   requires;     /* what is the nature of the
                                                   * dependency? */
    Boolean                         optional;     /* is it optional?  */
    char                            selecttype;   /* 0 = not selectable, 1 =
                                                   * undertermined, 2 = type, 3
                                                   * = type_opt */
    Boolean                         hasModifier;  /* does the type have a
                                                   * modifier? */
    XCTypeModifier                  modifier;     /* contains modifier
                                                   * information */
    int                             type;         /* see above XCValue typedef
                                                   * decl. */
    XCValue                         value;        /* value union */
    int                             type_opt;     /* see above XCValue typedef
                                                   * decl. */
    XCValue                         value_opt;    /* value union */
    XCValue                         def;          /* default value */
}                               XCParmData;


typedef struct _interface_components {
    int                             type;
    Widget                          shell; /* will point to NULL or
					      a dialog shell if this is 
					      a dialog component */
    Widget                          xgi;  /* will point to an interactor */
    char                           *name;
    char                           *desc;
    char                           *help;
    XCFileData                      helpFile;  /* help found in this file */
    XCFlagData                     *flag;
    XCParmData                     *parm;
    struct _interface_components   *right;
    struct _interface_components   *left;
}                               XCInterfaceData;

typedef struct _help_widget_data {
    char *label;
    char *path;
    struct _help_widget_data *next;
} XCHelpWidgetData;

typedef struct _xclip_data {
    char                           *prog;           /* program name */
    char                           *title;          /* short desc. (used in
                                                     * titlebar) */
    char                           *description;    /* long description */
    char                           *help;           /* help string */
    XCFileData                      helpFile;       /* help found in file */
    XCHelpWidgetData               *helpData;       /* help widget data */
    Boolean                         capture;        /* what to do with stdout */
    int                            *errorcodes;     /* acceptable exit codes */
    int                             nerrcodes;      /* # of exit codes */
    char                           *argString;      /* formatting string for
                                                     * flags and parameters */
    char                           *currentCommand; /* the command as it
                                                     * currently stands */
    Widget                          descText;       /* widget id of the
                                                     * current desc text */
    Widget                          commandText;    /* widget id of the
                                                     * current command text */
    XCInterfaceData                *data;
}                               _XC_data;

/* declaration of command line default arguments */
typedef struct _xc_command_defaults {
    Boolean                         isFlag;     /* True -> flag, False ->
                                                 * parm */
    char                           *name;
    char                           *value;
    struct _xc_command_defaults    *next;
}                               XCCommandDefaults;

#ifdef MAIN
_XC_data                        _xc_data;
XCCommandDefaults               _xc_com_defs;
#else
extern _XC_data                 _xc_data;
extern XCCommandDefaults        _xc_com_defs;
#endif                          /* MAIN */


#define ISTRUESTR(s) \
    (((!strcmp((s),"true")) || (!strcmp((s),"True")) || \
      (!strcmp((s),"TRUE")))  ? True:False )
#define ISONSTR(s) \
    (((!strcmp((s),"on")) || (!strcmp((s),"On")) || \
      (!strcmp((s),"ON")))  ? True:False )
#define ISFLAGSTR(s) \
    (((!strcmp((s),"flag")) || (!strcmp((s),"Flag")) || \
      (!strcmp((s),"FLAG")))  ? True:False )
#define ISPARMSTR(s) \
    (((!strcmp((s),"parm")) || (!strcmp((s),"Parm")) || \
      (!strcmp((s),"PARM")))  ? True:False )


#endif                          /* _XCLIP_TYPES_H */
