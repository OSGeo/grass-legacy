
/*
 * FILE: ll.h
 *
 * PROGRAMMER: David M. Johnson
 *
 */

/*************************/
/*** LINKED LIST TYPES ***/
/*************************/

typedef struct _llnode 
   {
   int nodeID;
   unsigned char *data;
   struct _llnode *next, *prev;
   } llnode;

typedef struct _llist
   {
   llnode *begin;
   llnode *end;
   int count; 
   } llist;

#define LL_NULL           -2
#define LL_LESS_THAN      -1
#define LL_EQUAL           0
#define LL_GREATER_THAN    1
    
/***********/
/* llist.c */ 
/***********/

llist *LLinit(
#ifndef _NO_PROTO
#endif
);

unsigned char *LLpopEnd(
#ifndef _NO_PROTO
   llist *list
#endif
);

unsigned char *LLpopBegin(
#ifndef _NO_PROTO
   llist *list
#endif
);

LLinsertNoDups(
#ifndef _NO_PROTO
   llist *list,
   unsigned char *data,
   int (*compare)()
#endif
);

LLinsertWithDups(
#ifndef _NO_PROTO
   llist *list,
   unsigned char *data,
   int (*compare)()
#endif
);

LLinsert(
#ifndef _NO_PROTO
   llist *list,
   unsigned char *data,
   int (*compare)(),
   int dups
#endif
);

int LLpushEnd(
#ifndef _NO_PROTO
   llist *list
   unsigned char *data
#endif
);

int LLpushBegin(
#ifndef _NO_PROTO
   llist *list,
   unsigned char *data
#endif
);

void LLfreeList(
#ifndef _NO_PROTO
   llist *list
#endif
);

void LLfreeNodes(
#ifndef _NO_PROTO
   lnode *node;
#endif
);

