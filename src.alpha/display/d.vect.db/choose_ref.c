#include <stdio.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/Form.h>
#include "global.h"

XmString choices[10];
void ColCallSelect();
void TableCallSelect(), unmanage_parent();
extern char *G_store();

int 
choose_ref_for_col(Col)
/* chooses which reference set the column participates is to follow */
   struct Column_Info *Col;
{
   Arg al[30];
   int ac, i;

   Widget List, Container, Label;
   struct Widget_Info *Table;

   Table = (struct Widget_Info *) Col->parent_table;
   /* Node = Table->RefGraphNode; */
   if(0 /* is only one set containst column */)
   {
      i = Col->col_num; 
      /* first unhighlight former highlight list */
      if(Table->CurrentLinkSet)
          highlite_links(Table, 0);
      get_link_set(i, Table);
      highlite_links(Table, 1);
      printf("follow link: %d\n", i);
   }
   else /* let user choose ref */
   {
      Container = XmCreateFormDialog(Col->Button, "list Container", NULL, 0);
      ac = 0;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNlabelString,
          XmStringCreate("Select the Table", XmSTRING_DEFAULT_CHARSET));ac++;
      Label = XmCreateLabel(Container, "LB", al, ac);
      XtManageChild(Label);

      ac = 0;
      XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
      XtSetArg(al[ac], XmNtopWidget, Label); ac++;
      XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
      XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;

      XtSetArg(al[ac], XmNautomaticSelection, TRUE); ac++;
      XtSetArg(al[ac], XmNitemCount, 10); ac++;
   choices[0] = XmStringCreate("111111111110", XmSTRING_DEFAULT_CHARSET);
   choices[1] = XmStringCreate("111111111111", XmSTRING_DEFAULT_CHARSET);
   choices[2] = XmStringCreate("111111111112", XmSTRING_DEFAULT_CHARSET); 
   choices[3] = XmStringCreate("111111111113", XmSTRING_DEFAULT_CHARSET); 
   choices[4] = XmStringCreate("111111111114", XmSTRING_DEFAULT_CHARSET); 
   choices[5] = XmStringCreate("111111111115", XmSTRING_DEFAULT_CHARSET); 
   choices[6] = XmStringCreate("111111111116", XmSTRING_DEFAULT_CHARSET); 
   choices[7] = XmStringCreate("111111111117", XmSTRING_DEFAULT_CHARSET); 
   choices[8] = XmStringCreate("111111111118", XmSTRING_DEFAULT_CHARSET); 
   choices[9] = XmStringCreate("111111111119", XmSTRING_DEFAULT_CHARSET); 

      XtSetArg(al[ac], XmNitems, choices); ac++;
      XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT); ac++;
      XtSetArg(al[ac], XmNheight, 100);ac++;
      List = XmCreateScrolledList(Container, "Table list", al, ac);
      XtAddCallback(List, XmNsingleSelectionCallback, unmanage_parent, Container);
      XtAddCallback(List, XmNsingleSelectionCallback, ColCallSelect, Col);
      XtManageChild(List);
      XtManageChild(Container);
   }
}

int
choose_ref_for_table(Table)
   struct Widget_Info *Table;
{
   int ac;
   Arg al[30];
   Widget Label, List, Container;

   /* Node = Table->RefGraphNode; */
   Container = XmCreateFormDialog(FollowLinkB, "list Container", NULL, 0);
   ac = 0;
   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNlabelString,
       XmStringCreate("Select the Table", XmSTRING_DEFAULT_CHARSET));ac++;
   Label = XmCreateLabel(Container, "LB", al, ac);
   XtManageChild(Label);

   ac = 0;
   XtSetArg(al[ac], XmNautomaticSelection, TRUE); ac++;
   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
   XtSetArg(al[ac], XmNtopWidget, Label); ac++;
   XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
   XtSetArg(al[ac], XmNitemCount, 10); ac++;
   choices[0] = XmStringCreate("111111111111", XmSTRING_DEFAULT_CHARSET);
   choices[1] = XmStringCreate("111111111112", XmSTRING_DEFAULT_CHARSET);
   choices[2] = XmStringCreate("111111111113", XmSTRING_DEFAULT_CHARSET); 
   choices[3] = XmStringCreate("111111111114", XmSTRING_DEFAULT_CHARSET); 
   choices[4] = XmStringCreate("111111111115", XmSTRING_DEFAULT_CHARSET); 
   choices[5] = XmStringCreate("111111111116", XmSTRING_DEFAULT_CHARSET); 
   choices[6] = XmStringCreate("111111111117", XmSTRING_DEFAULT_CHARSET); 
   choices[7] = XmStringCreate("111111111117", XmSTRING_DEFAULT_CHARSET); 
   choices[8] = XmStringCreate("111111111118", XmSTRING_DEFAULT_CHARSET); 
   choices[9] = XmStringCreate("111111111119", XmSTRING_DEFAULT_CHARSET); 

   XtSetArg(al[ac], XmNitems, choices); ac++;
   XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT); ac++;
   XtSetArg(al[ac], XmNheight, 100);ac++;
   List = XmCreateScrolledList(Container, "Table list", al, ac);
   XtAddCallback(List, XmNsingleSelectionCallback, unmanage_parent, Container);
   XtAddCallback(List, XmNsingleSelectionCallback, TableCallSelect, Table);
   XtManageChild(List);
   XtManageChild(Container);
}

void TableCallSelect(w, Table, call_data)
   struct Widget_Info *Table;
   Widget w;
   XmListCallbackStruct *call_data;
{
   if(call_data->reason == XmCR_SINGLE_SELECT ||
      call_data->reason == XmCR_DEFAULT_ACTION ||
      call_data->reason == XmCR_BROWSE_SELECT)
   {
      Table->ChosenRefNumber = call_data->item_position;
      printf("chosen Table %d\n", call_data->item_position);
      if(C.AttTableManaged)
      {
	 printf("Already managed\n");
	 XtUnmanageChild(C.AttTable);
	 XtManageChild(C.AttTable);
      }
      else
      {
	 /* first highlighte the chosen set of columns */
         if(Table->CurrentLinkSet)
           highlite_links(Table, 0);
         get_link_set(2, Table);
         highlite_links(Table, 1);
         C.AttTableNumCols = 10;
         C.AttValues = (char **) G_malloc(10 * sizeof(char *));
         C.AttValues[0] = "1";
         C.AttValues[1] = "2";
         C.AttValues[2] = "3";
         C.AttValues[3] = "4";
         C.AttValues[4] = "5";
         C.AttValues[5] = "1";
         C.AttValues[6] = "2";
         C.AttValues[7] = "3";
         C.AttValues[8] = "4";
         C.AttValues[9] = "5";
         C.AttKeys = (char **) G_malloc(1 * sizeof(char *));
         C.AttKeys[0] = "4";
         C.nalloc = 0;
         C.AttTableName = G_store("Table C");
         Create_Edit_Dialog(FollowLinkB, &C);
         XtManageChild(C.AttTable);
	 C.AttTableManaged = 1;
      }
      printf("table follow link\n");
   }
}

void ColCallSelect(w, Col, call_data)
   struct Column_Info *Col;
   Widget w;
   XmListCallbackStruct *call_data;
{
   int i;
   struct Widget_Info *Table;

   if(call_data->reason == XmCR_SINGLE_SELECT ||
      call_data->reason == XmCR_DEFAULT_ACTION ||
      call_data->reason == XmCR_BROWSE_SELECT)
   {
      printf("chosen Table %d\n", call_data->item_position);
      Table = (struct Widget_Info *) Col->parent_table;
      Table->ChosenRefNumber = call_data->item_position;
      i = Col->col_num;
      /* first unhighlight former highlight list */
      if(Table->CurrentLinkSet)
      highlite_links(Table, 0);
      get_link_set(i, Table);
      /* now highlight new link set */
      highlite_links(Table, 1);
      printf("follow link: %d\n", i);
   }
}

void
unmanage_parent(w, Parent, call_data)
   Widget w, Parent;
   XmListCallbackStruct *call_data;
{
   XtUnmanageChild(Parent);
}
