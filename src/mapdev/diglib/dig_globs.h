#ifdef CONTROL
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int Lines_In_Memory;
GLOBAL char *Mem_Line_Ptr;
GLOBAL char *Mem_curr_position;

