
/*
 * FILE: llist.c
 *
 * PROGRAMMER: David M. Johnson
 *  
 * FUNCTIONS:
 *
 * These functions implement a generic and double-linked list.
 * Stack type functions are supported by the functions LLpushEnd(),
 * LLpopEnd() and LLpopBegin().  Insertion sort is supported by 
 * the function LLinsert(), LLinsert(), and LLinsert().  The 
 * linked list data type "llist" is defined in ll.h.  
 * 
 * llist *LLinit() 
 * ---------------
 * This function allocates space for a linked list, sets 
 * its pointers to NULL, and returns a pointer to it.
 *
 * int LLpushEnd(llist *list, unsigned char *data)
 * -----------------------------------------------
 * This function appends (pushes) the data item to the end of 
 * the linked list (stack).  It returns the updated number of 
 * items in the list. 
 *
 * unsigned char *LLpopEnd(llist *llist) 
 * -------------------------------------
 * This function removes (pops) the last data item from the 
 * end of the linked list (stack) and returns a pointer to it.
 *
 * unsigned char *LLpopBegin(llist *llist) 
 * ---------------------------------------
 * This function removes (pops) the first data item from the 
 * beginning of the linked list (stack) and returns a pointer 
 * to it.
 *
 * LLfreeList(llist *list) 
 * -----------------------
 * This function destroys a linked list and frees all memory 
 * allocated to it.  It calls LLfreeNodes() to do this.
 * 
 * LLfreeNodes(lnode *node)
 * ------------------------
 * This recursive function removes and de-allocates the linked 
 * list node pointed to by "node" and all that follow it.
 *
 * LLinsert(list,data,compare,dups)
 * -------------------------------- 
 * This function implements a generic insertion sort for the 
 * linked list.  "list" is a pointer to the linked list and "data" 
 * is a pointer to the data item.  The "compare" argument must be
 * a pointer to a function that will compare two data items and 
 * will return -1 if the first is less than the second, 0 if they 
 * are equal and 1 if the first is greater than the second.  The 
 * boolean dups argument determines whether or not duplicate 
 * data items will be allowed in the list.  
 *   
 */

#include <stdio.h>
#include "ll.h"

/**************/
/*** LLinit ***/
/**************/

llist *LLinit()
{
llist *list;
#ifdef DEBUG
printf("LLinit\n");
#endif
list = (llist *)malloc(sizeof(llist));
list->count = 0;
list->begin = NULL;
list->end = NULL;
return(list);
}

/*****************/
/*** LLpushEnd ***/ 
/*****************/

int LLpushEnd(list,data)
llist *list;
unsigned char *data;
{
llnode *newnode,
       *nodeptr;

#ifdef DEBUG
printf("LLpushEnd\n");
#endif
if (list->begin == NULL)  
   {
   /*** FIRST AND ONLY NODE IN LIST ***/ 
   newnode = (llnode *)malloc(sizeof(llnode));
   newnode->data = data;
   newnode->next = NULL;
   newnode->prev = NULL;
   list->begin = newnode;
   list->end = newnode;
   }
else 
   {
   /*** INSERT AT END OF LIST ***/ 
   newnode = (llnode *)malloc(sizeof(llnode));
   newnode->data = data;
   newnode->next = NULL;
   newnode->prev = list->end; 
   list->end->next = newnode;
   list->end = newnode;
   } 
return(++(list->count));
}

/****************/
/*** LLpopEnd ***/
/****************/

unsigned char *LLpopEnd(list)
llist *list;
{
unsigned char *data;
llnode *tmp;

#ifdef DEBUG
printf("LLpopEnd\n");
#endif

if (list->end == NULL) return(NULL);

data = list->end->data;

if (list->end->prev == NULL) 
   {
   /* its the only remaining node in the list */ 
   free(list->end);
   list->end = NULL;
   list->begin = NULL;
   }
else
   {
   tmp = list->end->prev;
   tmp->next = NULL;
   free(list->end);
   list->end = tmp;
   }
(list->count)--;
return(data);
}

/******************/
/*** LLpopBegin ***/
/******************/

unsigned char *LLpopBegin(list)
llist *list;
{
unsigned char *data;
llnode *tmp;

#ifdef DEBUG
printf("LLpopBegin\n");
#endif

if (list->begin == NULL) return(NULL);

data = list->begin->data;

if (list->begin->next == NULL)
   {
   /* its the only remaining node in the list */ 
   free(list->begin);
   list->begin = NULL;
   list->end = NULL;
   }
else
   {
   tmp = list->begin->next;
   tmp->prev = NULL;
   free(list->begin);
   list->begin = tmp;
   }
(list->count)--;
return(data);
}

/******************/
/*** LLfreeList ***/
/******************/

void LLfreeList(list)
llist *list;
{
#ifdef DEBUG
printf("LLfreeList\n");
#endif
if (list != NULL)
   {
   if (list->begin != NULL) LLfreeNodes(list->begin);
   free(list);
   }
list = NULL;
}

/*******************/
/*** LLfreeNodes ***/
/*******************/

void LLfreeNodes(node)
llnode *node;
{
if (node->next != NULL) LLfreeNodes(node->next);
if (node->data != NULL) free(node->data);
free(node);
}

/************************/
/*** LLinsertWithDups ***/
/************************/

int LLinsertWithDups(list,data,compare)
{ 
return(LLinsert(list,data,compare,1)); 
}

/**********************/
/*** LLinsertNoDups ***/
/**********************/

int LLinsertNoDups(list,data,compare)
{
return(LLinsert(list,data,compare,0)); 
}

/****************/
/*** LLinsert ***/
/****************/

int LLinsert(list,data,compare,dups)
llist *list;
unsigned char *data;
int (*compare)();
int dups;
{
int comp;
llnode *newnode;
llnode *lptr;

#ifdef DEBUG
printf("LLinsert\n");
#endif

if (list == NULL) return(-1);

newnode = (llnode *)malloc(sizeof(llnode));
newnode->data = data;

if (list->begin == NULL)
   {
   /* INSERT AS FIRST AND ONLY NODE IN LIST */
   list->begin = list->end = newnode;
   newnode->prev = NULL;
   newnode->next = NULL;
   (list->count)++;
#  ifdef DEBUG
   printf("NODE:    %ld\n",newnode);
   printf("   prev: %ld\n",newnode->prev);
   printf("   next: %ld\n",newnode->next);
#  endif
   return(0);
   }

lptr = list->begin;
while ((comp = (*compare)(data,lptr->data)) == LL_GREATER_THAN)
   {
   if (lptr->next == NULL) 
      {
      comp = LL_NULL;
      break;
      }
   lptr = lptr->next;
   }

if (comp == LL_LESS_THAN)
   {
   /* INSERT IN MIDDLE OR AT BEGINNING OF LIST, BEFORE LPTR */
   newnode->next = lptr;
   newnode->prev = lptr->prev;
   if (lptr->prev == NULL) 
      list->begin = newnode;
   else
      lptr->prev->next = newnode;
   lptr->prev = newnode;
   }
else if (comp == LL_EQUAL && dups)
   {
   /* INSERT IN MIDDLE OR AT BEGINNING OF LIST, BEFORE LPTR */
   newnode->next = lptr;
   newnode->prev = lptr->prev;
   if (lptr->prev == NULL)
      list->begin = newnode;
   else
      lptr->prev->next = newnode;
   lptr->prev = newnode;
   }
else if (comp == LL_EQUAL && !dups)
   {
   /* NO DUPLICATES */
   free(newnode);
   return(0);
   }
else if (lptr->next == NULL)
   {
   /* INSERT AT END OF LIST, AFTER LPTR */
   newnode->next = NULL;
   newnode->prev = lptr;
   lptr->next = newnode;
   list->end = newnode; 
   }
#ifdef DEBUG
printf("NODE:    %ld\n",newnode);
printf("   prev: %ld\n",newnode->prev);
printf("   next: %ld\n",newnode->next);
#endif

(list->count)++;
return(0);
}

