/**********************************************************************
grammar.y    - the xclip parser grammar (yacc)
*********************************************************************/
%{

#include "xc.xclip.h"

int dialogLevel = 0;
XCInterfaceData *interface = NULL;
XCInterfaceData *curPtr = NULL;
XCInterfaceData *tempPtr = NULL;
extern XclipGlobalData *zzGlobal;

#define CURFLAG (curPtr->flag)
#define CURPARM (curPtr->parm)

XCInterfaceData *_xc_stack[50];
int _xc_size = 0;
#define PUSHDIALOGPTR(p) ((void)(_xc_stack[_xc_size++] = (p)))
#define POPDIALOGPTR     (_xc_stack[_xc_size] = NULL , _xc_stack[--_xc_size])

%}

%start Interface

%union {
    char *  cval;
    int ival;
    double dval;
    Boolean bval;
};

%token <cval> String
%token <ival> Integer
%token <dval> Real
%token <bval> Logical

/* Header Section Keywords */
%token  Program
%token  Title
/* This keyword is used in all sections */
%token  Description
/* This keyword is used in all sections */
%token  Help
/* This keyword is used in all sections */
%token  HelpFile
%token  HelpWidgetRef
%token  Capture
%token  ErrorCodes
%token  CommandString

%token  Dialog

%token  Flag

%token  Parameter

/* This keyword is used in the flags and the parameters sections */
%token  Key
%token  Requires
%token  Precludes

%token  Optional
%token  Input
%token  Multiple
%token  Type
%token  TypeEnumerate
%token  TypeFileName
%token  TypeDatabaseElement
%token  TypeCharacter
%token  TypeInteger
%token  TypeFloat
%token  TypeDouble
%token  TypeLogical

/* Modifiers for database_element */
%token  DatabaseElementRaster
%token  DatabaseElementAsciiDlg
%token  DatabaseElementDlg
%token  DatabaseElementAsciiVector
%token  DatabaseElementVector
%token  DatabaseElementSites
%token  DatabaseElementRegion
%token  DatabaseElementIcon
%token  DatabaseElementLabel
%token  DatabaseElementGroup
%token  DatabaseElementSubGroup
%token  StatusOld
%token  StatusNew

%token  SelectType
%token  Default

%type <cval>    String FlagOrParmKey 
%type <ival>    Integer
%type <bval>    Logical
%type <dval>    Real

%%

Interface

    :
    
    {
        bzero((char *)&zzGlobal->_xc_Data, sizeof(_XC_data));
    }
    ProgramHeader 
    { 
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Header Section\n"); 
        }
    }
    MainProgram 
    { 
        zzGlobal->_xc_Data.data = interface;
        /* check precludes and requirements for valid names */
        _XcValidDependencies(zzGlobal->_xc_Data.data,zzGlobal);
        if ( XCverbose) {
            DoFprintf(dialogLevel,"COMPLETED -> Program Section\n"); 
        }
    }
    ;

ProgramHeader

    :
    Program ':' String
    { 
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Description of Program: %s\n",$3);
        }
        zzGlobal->_xc_Data.prog = _XgStrDup($3);
    }
    Title ':' String 
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tTitle: %s\n",$7);
        }
        zzGlobal->_xc_Data.title = _XgStrDup($7);
    }
    CommandString ':' String 
    {
        zzGlobal->_xc_Data.argString = _XgStrDup($11);
    }
    OptionalHeaderElementList
    ;

OptionalHeaderElementList

    :
    /* None, they're optional... */
    
    | HeaderElementList
    ;

HeaderElementList

    :
    HeaderElementList HeaderElement
    
    | HeaderElement
    ;
    
HeaderElement

    :
    Description ':' String 
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",$3);
            DoFprintf(dialogLevel,"\tOptional Header Elements:\n");
        }
        zzGlobal->_xc_Data.description = _XgStrDup($3);
    }
    
    | Help':' String       
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",$3);
        }
        zzGlobal->_xc_Data.help = _XgStrDup($3);
    }
    
    | HelpFile ':' String  
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",$3);
        }
        zzGlobal->_xc_Data.helpFile.filename = _XgStrDup($3);
    }
    
    | HelpWidgetRef ':' String  
    {
	char *ptr = _XgStrDup($3);
	XCHelpWidgetData *hptr = zzGlobal->_xc_Data.helpData;

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpWidgetRef: %s\n",$3);
        }
	if ( hptr ) {

	    while ( hptr->next ) hptr = hptr->next;
	    hptr->next = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    hptr = hptr->next;
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	} else {
	    hptr = zzGlobal->_xc_Data.helpData = 
		(XCHelpWidgetData *)_XgMalloc(sizeof(XCHelpWidgetData));
	    bzero((char *)hptr, sizeof(XCHelpWidgetData));
	}
	if ( !_XcParseHelpData(hptr,ptr) ) {
          sprintf(errorbuf,"Illegal help data [%s]",ptr);
          yyerror(errorbuf);
	}
	_XgFree(ptr);
    }
    
    | Capture ':' String     
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tCapture: %s\n",$3);
        }
        zzGlobal->_xc_Data.capture = (ISONSTR($3) || ISTRUESTR($3));
    }
    
    | ErrorCodes ':' String
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tErrorcodes: %s\n",$3);
        }
        zzGlobal->_xc_Data.nerrcodes = _XcParseErrorCodes($3,&(zzGlobal->_xc_Data.errorcodes));
	if ( zzGlobal->_xc_Data.nerrcodes == -1 ) {
          sprintf(errorbuf,"Illegal errorcode string [%s]", $3);
          yyerror(errorbuf);
	}
    }
    ;
    
MainProgram

    :
      /* 
       * I suppose you might want to have an xclip interface to
       * a command with no arguments...
       */
    
    | '{' FlagOrParmOrDialogDescriptionList '}'
    ;
    

FlagOrParmOrDialogDescriptionList

    :
    FlagOrParmOrDialogDescriptionList FlagOrParmOrDialogDescription
    
    | FlagOrParmOrDialogDescription
    ;
    
FlagOrParmOrDialogDescription

    :
    FlagDescription

    | ParmDescription

    | DialogDescription
    ;
    
DialogDescription

    :
    Dialog 
    String
    {
        /* 
        * Allocate a new pointer and push it on to the dialog stack.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup($2);
        }  else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    /* 
	     * If the current pointer is NOT of type dialog, go left, else
	     * if the right branch of the current pointer is NULL, go right,
	     * else, go left.
	     */
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
	    PUSHDIALOGPTR(curPtr);
	    curPtr->type = XC_DIALOG;
	    curPtr->name = _XgStrDup($2);
	}
        dialogLevel++;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Dialog Description\n");
            DoFprintf(dialogLevel,"\tDialog Desc: %s\n",$2);
        }
    }
    OptionalDescription
    OptionalDialogElementList
    '{' 
    FlagOrParmOrDialogDescriptionList 
    '}'
    {
        /*
        * Done with this dialog, pop it off the stack and resume processing.
        */
        curPtr = POPDIALOGPTR;
        dialogLevel--;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"END -> Dialog Description\n");
        }
    }
    ;

OptionalDescription
    :
    /* none, it's optional */

    | Description ':' String
    {
	curPtr->desc = _XgStrDup($3);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Desc: %s\n",$3);
        }
    }
    ;

OptionalDialogElementList
    : 
    /* none, it's optional */

    | Help':' String
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",$3);
        }
        curPtr->help = _XgStrDup($3);
    }

    | HelpFile ':' String
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",$3);
        }
        curPtr->helpFile.filename = _XgStrDup($3);
    }
   ;

FlagDescription

    :
    Flag String
    { 
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, $2) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",$2);
	    yyerror(errorbuf);
	}
        /*
        * Allocate a new pointer and link it in to the list.
        */
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_FLAG;
        curPtr->name = _XgStrDup($2);
	CURFLAG = (XCFlagData *)_XgMalloc(sizeof(XCFlagData));
	bzero((char *)CURFLAG,sizeof(XCFlagData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Flag Description\n");
            DoFprintf(dialogLevel,"\tName: %s\n",$2);
        }
    }
    Key ':' String
    { 
	CURFLAG->key = _XgStrDup($6);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tKey: %s\n",$6);
        }
    }
    Description ':' String 
    {
	CURFLAG->desc = _XgStrDup($6);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",$10);
            DoFprintf(dialogLevel,"\tOptional Flag Elements:\n");
        }
    }
    OptFlagDescList
    ';'
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Flag\n");
        }
    }
    ;
    
OptFlagDescList

    :
    /* none */

    | OptionalFlagDescList
    ;
    
OptionalFlagDescList

    :
    OptionalFlagDescList OptionalFlagDesc

    | OptionalFlagDesc
    ;
    
OptionalFlagDesc

    :
    Help ':' String
    {
	CURFLAG->help = _XgStrDup($3);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",$3);
        }
    }

    | HelpFile ':' String
    {
	CURFLAG->helpFile.filename = _XgStrDup($3);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",$3);
        }
    }

    | Default ':' String 
    {
	CURFLAG->def = ISONSTR($3);
	CURFLAG->flagSet = True;
	CURFLAG->answer = curPtr->flag->def;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",$3);
        }
    }

    | Precludes ':' FlagOrParmKey ':'  String
    {
	if (CURFLAG->precludes ) {
	    XCRequireData *rPtr = &CURFLAG->preclude;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR($3);
	    rPtr->name = _XgStrDup($5);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->precludes = True;
	    CURFLAG->preclude.isFlag = ISFLAGSTR($3);
	    CURFLAG->preclude.name = _XgStrDup($5);
	    CURFLAG->preclude.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPreculdes: %s -> %s\n",$3,$5);
        }
    }

    | Requires ':' FlagOrParmKey ':'  String
    {
	if (CURFLAG->requirements ) {
	    XCRequireData *rPtr = &CURFLAG->requires;

	    while ( rPtr->next ) {
		rPtr = rPtr->next;
	    }
	    rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
	    rPtr = rPtr->next;
	    rPtr->isFlag = ISFLAGSTR($3);
	    rPtr->name = _XgStrDup($5);
	    rPtr->next = NULL;
	} else {
	    CURFLAG->requirements = True;
	    CURFLAG->requires.isFlag = ISFLAGSTR($3);
	    CURFLAG->requires.name = _XgStrDup($5);
	    CURFLAG->requires.next = NULL;
	}
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",$3,$5);
        }
    }
    ;
    
ParmDescription

    :
    Parameter String
    {
	/* 
	* Check for uniqueness of the handle name
	*/
	if ( !_XcUniqueName(interface, $2) ) {
	    sprintf(errorbuf,"Non-unique flag or parameter name [%s]",$2);
	    yyerror(errorbuf);
	}
        if ( interface == NULL ) {
            curPtr = interface = 
                (XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
            bzero((char *)interface, sizeof(XCInterfaceData));
        } else {
	    tempPtr = 
		(XCInterfaceData *)_XgMalloc(sizeof(XCInterfaceData));
	    bzero((char *)tempPtr, sizeof(XCInterfaceData));
	    if ( curPtr->type != XC_DIALOG ) {
		curPtr = curPtr->left = tempPtr;
	    } else if ( curPtr->right == NULL ) {
		curPtr = curPtr->right = tempPtr;
	    } else {
		curPtr = curPtr->left = tempPtr;
	    }
        }
        curPtr->type = XC_PARM;
        curPtr->name = _XgStrDup($2);
	curPtr->parm = (XCParmData *)_XgMalloc(sizeof(XCParmData));
	bzero((char *)curPtr->parm,sizeof(XCParmData));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"BEGIN -> Parm Description\n");
            DoFprintf(dialogLevel,"\tKey: %s\n",$2);
        }
    }
    ParmReqList
    {
        /*
        * If this is a database element and an input parameter and has a 
        * default, check it out for existence.
        */
	if ( CURPARM->type == XC_TYPE_UNKNOWN ) {
          sprintf(errorbuf,"No type given for parameter [%s]",curPtr->name);
          yyerror(errorbuf);
	}
        if ( CURPARM->parmSet && CURPARM->isInput &&
             CURPARM->type == XC_TYPE_DB_ELEMENT )
            VerifyDBElement(CURPARM);
    }
    ParmOptionList ';'
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"COMPLETED -> Parm\n");
        }
    }
    ;
    
ParmReqList

    :
    ParmReqList ParmReq

    | ParmReq
    ;
    
ParmReq

    :
    Description ':' String
    {
	CURPARM->desc = _XgStrDup($3);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tDesc: %s\n",$3);
        }
    }

    | 
    {
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tType:");
            fflush(stderr);
        }
    }
    TypeOption
    ;
    
TypeOption

    :
    Type ':' TypeCharacter
    {
        CURPARM->type = XC_TYPE_CHARACTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Character\n");
        }
    }

    | Type ':' TypeEnumerate ':' String
    {
        CURPARM->type = XC_TYPE_ENUMERATE;
	CURPARM->hasModifier = True;
        XcModifierParse(&CURPARM->modifier,XC_TYPE_ENUMERATE,$5);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Enumerate\n");
            DoFprintf(dialogLevel,"\tValues: %s\n", $5);
        }
    }

    | Type ':' TypeInteger OptIntegerModifier
    {
        CURPARM->type = XC_TYPE_INTEGER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Integer\n");
        }
    }

    | Type ':' TypeFloat OptDoubleModifier
    {
        CURPARM->type = XC_TYPE_FLOAT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Float\n");
        }
    }

    | Type ':' TypeDouble OptDoubleModifier
    {
        CURPARM->type = XC_TYPE_DOUBLE;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Double\n");
        }
    }

    | Type ':' TypeLogical OptLogicalModifier
    {
        CURPARM->type = XC_TYPE_LOGICAL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Logical\n");
        }
    }

    | Type ':' TypeFileName OptFilenameModifier
    {
        CURPARM->type = XC_TYPE_FILENAME;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Filename\n");
        }
    }

    | Type ':' 
    {
	CURPARM->type = XC_TYPE_DB_ELEMENT;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Database Element: ");
            fflush(stderr);
        }
    }
    DatabaseElementDesc
    ;

OptIntegerModifier

    :
    /* none */

    | ':' String
    {
        XcModifierParse(&CURPARM->modifier,XC_TYPE_INTEGER,$2);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",$2);
        }
    }
    ;
    
OptDoubleModifier

    :
    /* none */

    | ':' String
    {
        XcModifierParse(&CURPARM->modifier,XC_TYPE_DOUBLE,$2);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",$2);
        }
    }
    ;

OptLogicalModifier

    :
    /* none */

    | ':' String
    {
        XcModifierParse(&CURPARM->modifier,XC_TYPE_LOGICAL,$2);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",$2);
        }
    }
    ;

OptFilenameModifier

    :
    /* none */

    | ':' String
    {
        XcModifierParse(&CURPARM->modifier,XC_TYPE_FILENAME,$2);
        CURPARM->hasModifier = True;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Modifiers: %s",$2);
        }
    }
    ;
    
DatabaseElementDesc

    :
    TypeDatabaseElement ':' 
    DatabaseElementRaster 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_RASTER;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Raster [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementAsciiDlg 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII DLG [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementDlg 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_DLG;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Binary DLG [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementAsciiVector 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_ASCII_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"ASCII Vector [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementVector 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_VECTOR;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Vector [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementSites 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_SITES;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Sites [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementRegion 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_REGION;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Region [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementIcon 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_ICON;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Icon [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementLabel 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_LABEL;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Label [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementGroup 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_GROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Group [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementSubGroup 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_IMAGE_SUBGROUP;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Imagery Sub Group [Status = ");
            fflush(stderr);
        }
    }
    OptionalStatus

    | TypeDatabaseElement ':' 
    DatabaseElementUserDefined 
    {
        CURPARM->value.dbval.type = XC_DB_TYPE_USER_DEFINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"UserDefined\n");
        }
    }
    ;
    
DatabaseElementUserDefined 

    :
    
    ;
    
OptionalStatus

    :
    /* none.. */ 
    {
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Default(New)]\n");
        }
    }

    | ':' 
    {
        CURPARM->value.dbval.status = XC_DB_STATUS_OLD;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"Old]\n");
        }
    }
    StatusOld

    | ':' 
    {
        CURPARM->value.dbval.status = XC_DB_STATUS_NEW;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"New]\n");
        }
    }
    StatusNew
    ;
    
    
ParmOptionList

    :
    ParmOptionList ParmOption

    | ParmOption
    ;
    
ParmOption

    :
    SelectType ':' String
    {
        CURPARM->selecttype = XC_SELECT_UNDETERMINED;
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tSelectType: %s\n",$3);
        }
    }

    | Default ':' String
    {
        if ( CURPARM->type == XC_TYPE_UNKNOWN  &&
             CURPARM->selecttype == XC_SELECT_FALSE ) {
            yyerror("\"default\" defined before \"type\"");
        }

        switch(CURPARM->type) {
        case XC_TYPE_CHARACTER:
            CURPARM->def.cval = _XgStrDup($3);
            CURPARM->value.cval = _XgStrDup($3);
            break;
        case XC_TYPE_ENUMERATE:
            CURPARM->def.cval = _XgStrDup(
		_XcEnumStringToKey(CURPARM->modifier,$3));
            CURPARM->value.cval = _XgStrDup(CURPARM->def.cval);
            break;
        case XC_TYPE_INTEGER:
            CURPARM->def.ival = atoi($3);
            CURPARM->value.ival = CURPARM->def.ival;
            break;
        case XC_TYPE_FLOAT:
#ifndef mips
            CURPARM->def.dval = strtod($3,(char **)NULL);
#else
            CURPARM->def.dval = atof($3);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_DOUBLE:
#ifndef mips
            CURPARM->def.dval = strtod($3,(char **)NULL);
#else
            CURPARM->def.dval = atof($3);
#endif
            CURPARM->value.dval = CURPARM->def.dval;
            break;
        case XC_TYPE_LOGICAL:
            CURPARM->def.bval = (ISONSTR($3) || ISTRUESTR($3));
            CURPARM->value.bval = CURPARM->def.bval;
            break;
        case XC_TYPE_FILENAME:
            CURPARM->def.cval = _XgStrDup($3);
            if ( CURPARM->isInput && access(CURPARM->def.cval,0) != 0 ) {
		sprintf(errorbuf,"invalid default input file name [%s]",$3);
		yyerror(errorbuf);
            }
            CURPARM->value.cval = _XgStrDup($3);
            break;
        case XC_TYPE_DB_ELEMENT:
            CURPARM->def.dbval.desc = _XgStrDup($3);
            CURPARM->value.dbval.desc = _XgStrDup($3);
            break;
        }
        CURPARM->parmSet = True;

        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tDefault: %s\n",$3);
        }
    }

    | Help ':' String
    {
	CURPARM->help = _XgStrDup($3);
        if ( XCverbose  ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelp: %s\n",$3);
        }
    }

    | HelpFile ':' String
    {
	CURPARM->helpFile.filename = _XgStrDup($3);
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tHelpFile: %s\n",$3);
        }
    }

    | Precludes ':' FlagOrParmKey ':'  String
    {
        if (CURPARM->precludes ) {
            XCRequireData *rPtr = &CURPARM->preclude;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR($3);
            rPtr->name = _XgStrDup($5);
            rPtr->next = NULL;
        } else {
            CURPARM->precludes = True;
            CURPARM->preclude.isFlag = ISFLAGSTR($3);
            CURPARM->preclude.name = _XgStrDup($5);
            CURPARM->preclude.next = NULL;
        }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\t\tPrecludes: %s -> %s\n",$3,$5);
        }
    }

    | Requires ':' FlagOrParmKey ':'  String
    {
        if (CURPARM->requirements ) {
            XCRequireData *rPtr = &CURPARM->requires;

            while ( rPtr->next ) {
                rPtr = rPtr->next;
            }
            rPtr->next = (XCRequireData *)XtMalloc(sizeof(XCRequireData));
            rPtr = rPtr->next;
            rPtr->isFlag = ISFLAGSTR($3);
            rPtr->name = _XgStrDup($5);
            rPtr->next = NULL;
        } else {
            CURPARM->requirements = True;
            CURPARM->requires.isFlag = ISFLAGSTR($3);
            CURPARM->requires.name = _XgStrDup($5);
            CURPARM->requires.next = NULL;
        }
        if ( XCverbose ) DoFprintf(dialogLevel,"\t\tRequires: %s -> %s\n",$3,$5);
    }

    | Multiple ':' String
    {
      if ( CURPARM->type == XC_TYPE_FILENAME ||
           CURPARM->type == XC_TYPE_DB_ELEMENT ||
           CURPARM->type == XC_TYPE_ENUMERATE ) {
          CURPARM->multiple = (ISONSTR($3) || ISTRUESTR($3));
      } else {
          sprintf(errorbuf,"[%s] can not be multi-valued",curPtr->name);
          yyerror(errorbuf);
      }

        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tMultiple: %s\n",$3);
        }
    }

    | Input ':' String
    {
        CURPARM->isInput = (ISONSTR($3) || ISTRUESTR($3));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tInput: %s\n",$3);
        }
    }

    | Optional ':' String
    {
        CURPARM->optional  = (ISONSTR($3) || ISTRUESTR($3));
        if ( XCverbose ) {
            DoFprintf(dialogLevel,"\tOptional Parm Elements:\n");
            DoFprintf(dialogLevel,"\t\tOptional: %s\n",$3);
        }
    }
    ;
    
FlagOrParmKey

    :
    Flag { $$ = "flag"; }

    | Parameter { $$ = "parameter"; }
    ;
    
%%
zzinitParser()
{
interface = NULL;
_xc_size = 0;
curPtr = NULL;
tempPtr = NULL;
}
