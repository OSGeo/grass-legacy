#ifndef FILE
#  include <stdio.h>
#endif

#define VOID_T char


VOID_T *	link_init ();
void 		link_set_chunk_size ();
VOID_T *	link_new ();
void 		link_dispose ();
void 		link_cleanup ();
void 		link__set_next();		/* link next routine */
VOID_T * 	link__get_next();	/* link next routine */


#define PTR_CNT 10

struct link_head {
    VOID_T **ptr_array;		/* array of pointers to chunks */
    int max_ptr;		/* num of chunks alloced */
    int alloced;		/* size of ptr_array */
    int chunk_size;		/* size of alloc chucks in units */
    int unit_size;		/* size of each user defined unit */
    VOID_T *Unused;		/* Unused list pointer */
    int exit_flag;		/* exit on error ? */
};
