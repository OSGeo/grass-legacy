
struct stat_node
   {
   long int cat;             /* cell-file category value */
   long int stat;            /* statistic: number of cells with that cat */
   struct stat_node *next;   /* pointer to next stat_node in list */
   };

struct stat_list
   {
   struct stat_node *ptr;    /* pointer to first stat_node in list */
   long int count,           /* number of stat_nodes in list */
            maxstat,         /* max. statistic in list */
            minstat,         /* min. statistic in list */
            sumstat,         /* sum of all statistics in list */
            maxcat,          /* max. cell-file category value in list */
            mincat;          /* min. cell-file category value in list */
   };

/* structures for determining tic-mark numbering scheme */
struct units 
   {
   char     *name;           /* name of unit (text) */
   long int unit;            /* tic-mark interval */
   long int every;           /* tic_mark number interval */
   };

#ifdef MAIN

struct stat_list dist_stats;

#endif
