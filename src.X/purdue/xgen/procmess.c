#include "xgen.h"

MessageInfo *
ProcessMessage(string,columns)
	char *string;
	int columns;
{
	/* allocate a string to keep a copy in */
	char *save = XtMalloc(strlen(string) +1);
	char *savePtr;
	int maxcharwidth;
	int len;
	int stringlen;
	int colwidth;
	int j;
	Boolean subdivide = False;

	MessageInfo *head;
	MessageInfo *cur;

	int lastpos = 1;
	int count = 1;

	savePtr = strcpy(save,string);

/***************************************************************
 * allocate space for head of message info list and set cur pointer
 **************************************************************/
	cur = head = (MessageInfo *)XtMalloc(sizeof(MessageInfo));
	bzero((char *)head,sizeof(MessageInfo));

/***************************************************************
 * check for NULL input string
 **************************************************************/
	if ( !string ) return(NULL);

	stringlen = strlen(string);
	if ( columns < stringlen ) 
		subdivide = True;
	else {
		cur->startpos = lastpos;
		cur->endpos = strlen(string);
		return(head);
	}

/***************************************************************
 * while we still need to subdivide......
 **************************************************************/
	while ( subdivide ) {
		Boolean got_a_line = False;
		int charcount = 0;
	/***************************************************************
	 * cram as many WORDS as we can into each line
	 **************************************************************/

		while ( !got_a_line ) {
			int i;

			/* we reached a space check to see if the next space 
			 * would put us beyond columns... */
			if ( isspace(*savePtr) ) {
				char *t = savePtr;
				int more = 0;

				*t = ' ';

				*t++; /* increment past space */
				while ( *t && !isspace(*t) ) {
					*t++; more++;
				}
				/* if so, save that part and break loop */
				if ( more + charcount > columns ) {

					/***********************************************************
					 * store appropraite info
					 **********************************************************/
					 cur->startpos = lastpos;
					 cur->endpos = lastpos + charcount;
					/***********************************************************
					 * allocate space for another message info list element
					 **********************************************************/
					cur->next = (MessageInfo *)XtMalloc(sizeof(MessageInfo));
					bzero((char *)cur->next,sizeof(MessageInfo));
					cur = cur->next;
					lastpos += charcount + 1;
					got_a_line = True;
					count++;
				}
			}
			*savePtr++; 
			/* if we reached the end of the string, 
			   save it and break both loops */
			if ( *savePtr == NULL ) {
				if ( !got_a_line ) {

					/***********************************************************
					 * store appropraite info
					 **********************************************************/
					 cur->startpos = lastpos;
					 cur->endpos = lastpos + charcount;
					/***********************************************************
					 * allocate space for another message info list element
					 **********************************************************/
					cur->next = (MessageInfo *)XtMalloc(sizeof(MessageInfo));
					bzero((char *)cur->next,sizeof(MessageInfo));
					cur = cur->next;
					lastpos += charcount;
				}
				got_a_line = True;
				count++;
				subdivide = False;
			}
			charcount++;
		}
		/* we just got another segment, if we aren't at strings end
		   just see if we need to subdivide further... */
		if ( subdivide && !(columns < stringlen - lastpos) ) {
			subdivide = False;
			cur->startpos = lastpos;
			cur->endpos = stringlen;
			count++;
		}
	}

	return(head);

}

FreeMIList(mi)
	MessageInfo *mi;
{
	if ( mi->next != NULL ) {
		FreeMIList(mi->next);
	}
	XtFree(mi);
}
