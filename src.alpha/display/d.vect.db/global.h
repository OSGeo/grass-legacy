struct Column_Info
{
   Widget Button;
   Widget Label;
   Widget Text;
   Widget Container;
   int col_num;
   void *parent_table; /* used when Column_Info is being passed to callbacks*/
} ;

struct Widget_Info
{
   struct Column_Info     *Cols;
   char            **AttKeys; /* the inex set into the table*/
   char            **AttValues; /* stores the values */
   int nalloc;
   int            AttTableNumCols;
   int cur_col;   /* cur column being edited */
   char      *AttTableName;
   int AttTableManaged;
   Widget AttTable;
   int ChosenRefNumber;
   /* for future use
   Npde RefGraphNode;
   ReferenceEdge *AttLinkSet = 0;
   */
   /* TEMP */ int *CurrentLinkSet;
   /* TEMP */ int CurrentLinkSetSize;
};

#define LINE_TYPE 0
#define AREA_TYPE 1

#ifndef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL Widget shell;
GLOBAL Widget FollowLinkB;
GLOBAL struct Widget_Info A, B, C;
