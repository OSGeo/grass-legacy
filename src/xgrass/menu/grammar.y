/**********************************************************************
   grammar.y    - the xclip parser grammar (yacc)
 *********************************************************************/
%{

#include "xgrass.h"

XgMenuItemRec *curMenuItem;
XgMenuItemListRec *menuItemList, *curMenuItemList = NULL;
XgMenuBarListRec *menuBarList, *curMenuBarList = NULL;
char *curParentLabel = NULL;
Boolean parsingMenuBar = False;

%}

%union {
 char *cval;
};

%start MenuDescription

%token  MenuKey
%token  DefaultMenuKey

%token  F_Quit

%token  F_Label
%token  F_Separator
%token  F_Pulldown
%token  F_Pullright
%token  F_HMenu
%token  F_VMenu

%token  F_Exec
%token  F_ExecCapture
%token  F_ExecHist
%token  F_ExecCaptureHist
%token  F_Xclip

%token  F_DBSet

%token  F_HistoryToggle
%token  F_HistoryClear
%token  F_HistoryEdit
%token  F_HistoryReplay

%token  String

%type <cval> String OptMnemonic OptAccelerator DefaultMenuKey
%%

MenuDescription
    : MenuBar OptMenuList 
      { 
	  LinkMenuSystem(menuBarList, menuItemList); 
	  _XG_Global.menuData.menuBarList = menuBarList;
      }
    ;

MenuBar
    : MenuKey DefaultMenuKey 
      {
          parsingMenuBar = True;
          curParentLabel = StrDup($2);
      }
      '{' MenuItemList '}'
      {
          parsingMenuBar = False;
      }
    ;

OptMenuList
    : /* optional */
    | MenuList
    ;

MenuList
    : MenuList Menu
    | Menu
    ;

Menu
    : MenuKey 
      String 
      {
	  if ( curParentLabel != NULL ) XtFree(curParentLabel);
	  curParentLabel = StrDup($2);
      }
      '{' MenuItemList '}'
    ;

MenuItemList
    : MenuItemList MenuItem
    | MenuItem  
    ;

MenuItem
    : String 
      {
          if ( parsingMenuBar ) {
              if ( curMenuBarList == NULL ) {
                  menuBarList = curMenuBarList = (XgMenuBarListRec *)
                      XtMalloc(sizeof(XgMenuBarListRec));
              } else {
                  curMenuBarList->next = (XgMenuBarListRec *)
                      XtMalloc(sizeof(XgMenuBarListRec));
                  curMenuBarList = curMenuBarList->next;
              }
	      bzero((char *)curMenuBarList, sizeof(XgMenuBarListRec));
          } else {
              if ( curMenuItemList == NULL ) {
                  menuItemList = curMenuItemList = (XgMenuItemListRec *)
                      XtMalloc(sizeof(XgMenuItemListRec));
              } else {
                  curMenuItemList->next = (XgMenuItemListRec *)
                      XtMalloc(sizeof(XgMenuItemListRec));
                  curMenuItemList = curMenuItemList->next;
              }
	      bzero((char *)curMenuItemList, sizeof(XgMenuItemListRec));
          }
          curMenuItem = (XgMenuItemRec *)XtMalloc(sizeof(XgMenuItemRec));
          bzero((char *)curMenuItem, sizeof(XgMenuItemRec));
          curMenuItem->label = StrDup($1);
      }
      OptMnemonic
      {
          curMenuItem->mnemonic = StrDup($3);
      }
      OptAccelerator
      {
          curMenuItem->accelerator = StrDup($5);
      }
      FunctionDesc
      {
          if ( parsingMenuBar ) 
              curMenuBarList->menubar_item = curMenuItem;
          else
              curMenuItemList->menu_item = curMenuItem;
      }
    ;

OptMnemonic
    : /* none, it's optional */ { $$ = NULL; }
    | '_' String { $$ = $2; }
    ;

OptAccelerator
    : /* none, it's optional */ { $$ = NULL; }
    | String { $$ = $1; }
    ;

FunctionDesc
    : F_Quit 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgrassExit;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_Label 
      {
          if ( parsingMenuBar ) {
              yyerror("label not allowed in main menu.");
          } else {
              curMenuItem->class = &xmLabelWidgetClass;
              curMenuItem->submenu_type = XG_MENU_NONE;
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
    | F_Separator 
      {
          if ( parsingMenuBar ) {
              yyerror("separator not allowed in main menu.");
          } else {
              curMenuItem->class = &xmSeparatorWidgetClass;
              curMenuItem->submenu_type = XG_MENU_NONE;
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
    | F_Pulldown String 
      {
          if ( parsingMenuBar ) {
              curMenuItem->class = &xmCascadeButtonWidgetClass;
              curMenuItem->submenu_type = XG_MENU_PULLDOWN;
              curMenuItem->submenu_label = StrDup($2);
              curMenuItem->parent_label = StrDup(curParentLabel);
          } else {
              yyerror("pulldown menu not allowed in sub menus.");
          }
      }
    | F_Pullright String 
      {
          if ( parsingMenuBar ) {
              yyerror("pullright menu not allowed in main menu.");
          } else {
              curMenuItem->class = &xmCascadeButtonWidgetClass;
              curMenuItem->submenu_type = XG_MENU_PULLRIGHT;
              curMenuItem->submenu_label = StrDup($2);
              curMenuItem->parent_label = StrDup(curParentLabel);
          }
      }
    | F_HMenu String 
      {
          curMenuItem->class = &xmCascadeButtonWidgetClass;
          curMenuItem->submenu_type = XG_MENU_TEAR_OFF_H;
          curMenuItem->submenu_label = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_VMenu String 
      {
          curMenuItem->class = &xmCascadeButtonWidgetClass;
          curMenuItem->submenu_type = XG_MENU_TEAR_OFF_V;
          curMenuItem->submenu_label = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_Exec String 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExec;
          curMenuItem->arglist = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_ExecCapture String 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecCapture;
          curMenuItem->arglist = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_ExecHist String 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecHist;
          curMenuItem->arglist = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_ExecCaptureHist String 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgExecCaptureHist;
          curMenuItem->arglist = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_Xclip String 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgXclip;
          curMenuItem->arglist = StrDup($2);
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_DBSet 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgDbSet;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_HistoryToggle 
      {
          curMenuItem->class = &xmToggleButtonWidgetClass;
          curMenuItem->callback = _XgHistoryToggle;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_HistoryClear 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryClear;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_HistoryEdit 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryEdit;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    | F_HistoryReplay 
      {
          curMenuItem->class = &xmPushButtonWidgetClass;
          curMenuItem->callback = _XgHistoryReplay;
          curMenuItem->parent_label = StrDup(curParentLabel);
      }
    ; 

%%
