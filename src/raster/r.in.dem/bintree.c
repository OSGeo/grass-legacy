#include <stdlib.h>
#include "gis.h"     /* typedefs CELL and G_* */
#include "bintree.h"
#include "main.h"

static unsigned short ptree_size = 1;
static int queue_fd;
static unsigned long cells_written = 0, num_writes = 0;
static struct ptree ptree[PTREE_SIZE];

/***************************************************************************/
/* This checks the incoming data. As long as it is contiguous on one row,  */
/* it is added to the cache. When we get a data point that is not          */
/* contiguous, the cache is flushed in a single write and restarted with   */
/* that data point. A negative row will always flush the cache, and        */
/* corrupt it in the process.                                              */
/***************************************************************************/
int cache(const cell_row_t row, const cell_row_t col, const cell_data_t data){
  static CELL cache[CACHE_SIZE]; /* dynamic alloc probably not worth it */
  static int cache_len = 0;
  static int buff_row = -1; /* Forces the cache to restart the first time */
  static int buff_col = 0;  /* starting column for the buffer */
  extern int queue_fd;
  extern unsigned long cells_written, num_writes;
  
/* If cache can be continued */
  if (col == buff_col + cache_len && row == buff_row && 
      cache_len < CACHE_SIZE){
    cache[cache_len++] = (CELL)data;
    return cache_len;   /* Old cache expanded; no write */
  }

/* Otherwise */
  if (cache_len)          /* Flush 'em if you got 'em */
    G_put_map_row_random(queue_fd, cache, buff_row, buff_col, cache_len);
  cells_written += cache_len;
  num_writes++;
  buff_row = (int)row;
  buff_col = (int)col;
  cache[0] = (CELL)data;
  cache_len = 1;
  return cache_len;   /* Cache reset */
}

/****************************************************************************/
/* Partially ordered tree                                                   */
/****************************************************************************/
void insert_ptree_node(const cell_row_t row, const cell_row_t col, const cell_data_t data){
  extern unsigned short ptree_size;
  bubble_up(row, col, data, ptree_size);
  if (++ptree_size == PTREE_SIZE) {
    dump_ptree(); 
    ptree_size = 1; /* ...and we're left with an empty tree */
  }
return;
}

/**************************************************/
/* Dump contents to the cache in increasing order */
/**************************************************/
short dump_ptree(){
  extern unsigned short ptree_size;
  extern struct ptree ptree[];
  unsigned short small_child;
  unsigned short parent;
  short retval=0;

  ptree_size -= 2;
  do{
    parent = 1;
    cache(ptree[1].row, ptree[1].col, ptree[1].data);
    retval++;
    while (parent*2+1 <= ptree_size){  /* if both children are legitimate */
      if(ptree[parent*2].row  <  ptree[parent*2+1].row ||
	 (ptree[parent*2].row == ptree[parent*2+1].row &&
	  ptree[parent*2].col <  ptree[parent*2+1].col)){
	small_child = parent * 2;/* if the first is smaller */
      }
      else small_child = parent * 2 + 1; /* if the second child is smaller */
      ptree[parent].row  = ptree[small_child].row; /* move the smaller one up*/
      ptree[parent].col  = ptree[small_child].col;
      ptree[parent].data = ptree[small_child].data;
      parent = small_child;    /* and leave 'parent' as the new hole */
    }
    if (parent*2 == ptree_size){ /* If only one child exists, grab it */
      ptree[parent].row  = ptree[parent*2].row;  
      ptree[parent].col  = ptree[parent*2].col;  
      ptree[parent].data = ptree[parent*2].data;  
      parent = parent * 2;     /* and leave 'parent' at the hole */
    }
    bubble_up(ptree[ptree_size].row, ptree[ptree_size].col, ptree[ptree_size].data, parent);    /* Stick the last item in the hole and shuffle */
  }
  while(--ptree_size);
  if (VERBOSE) fprintf(stdout,"%lu cells in %lu writes; %2.2f cells/write\n",
		       cells_written, num_writes,
		       (float)cells_written / (float)num_writes);
  return retval;
}

/*****************************************************************************/
/* Bubble up new data from 'offset'. This takes the new data as values rather*/
/* than as part of the array to avoid them being overwritten in the process. */
/*****************************************************************************/
short bubble_up(const cell_row_t row, const cell_row_t col, const cell_data_t data, unsigned short offset){
  extern struct ptree ptree[];
  unsigned short child = offset, parent = offset / 2;
  short level = 0;

  if (child == 1){    /* if child is already the root */
    ptree[child].row = row;
    ptree[child].col = col;
    ptree[child].data = data;
    return 0;
  }
  while ((parent) &&                    /* If not at the root */
	 (row < ptree[parent].row ||    /* and parent is smaller */
	  (row == ptree[parent].row && col < ptree[parent].col))){
  ptree[child].row  = ptree[parent].row; /* move parent to child */
  ptree[child].col  = ptree[parent].col; /* perhaps memcpy() should be used? */
  ptree[child].data = ptree[parent].data;
  child = parent;           /* Shift up child and parent */
  parent = child / 2;
  level++;
  }
  ptree[child].row = row;
  ptree[child].col = col;
  ptree[child].data = data;
  return level;
}

/****************************************************************/
/* Set fd so we don't have to keep passing it between functions */
/****************************************************************/
void set_queue_fd (const int fd){
  queue_fd = fd;
}









