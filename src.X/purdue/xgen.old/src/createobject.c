#include "xgen.h"

/* returns the last widget created for sizing purposes */
Widget
CreateObject(shell,widget,isDynamic)
	Shell *shell;
	Widget widget;
	Boolean isDynamic;
{
	InterfaceObject *o = shell->objects;
	Widget retwidget = (Widget)0;
	Resource *updatefrom = NULL;
	
	if ( isDynamic )
		updatefrom = IndexResource(shell,SHELL,"updatefrom");

    while ( o ) {
		switch(o->type) {
			case LABEL:
				o->widget = retwidget = CreateLabel(o,widget);
				break;
			case MESSAGE:
				o->widget = retwidget = CreateMessage(o,widget);
				break;
			case LIST:
				o->widget = retwidget = CreateList(o,widget);
				break;
			case PUSHBUTTON:
				o->widget = retwidget = CreateButton(o,widget,updatefrom);
				break;
			case TEXTENTRY:
				o->widget = retwidget = CreateTextEntry(o,widget);
				break;
			case TABLE:
				o->widget = retwidget = CreateTable(o,widget);
				break;
			case FILESELECT:
				/*o->widget = retwidget = */CreateFileSelect(o,widget);
				break;
			case SEPARATOR:
				o->widget = retwidget = CreateSeparator(o,widget);
				break;
			case SLIDER:
				o->widget = retwidget = CreateSlider(o,widget);
				break;
			case TOGGLE:
				o->widget = retwidget = CreateToggle(o,widget);
				break;
		}
		o = o->next;
	}
	return retwidget;
}
