
/* constants for RIM interface */
#define BUF_SIZ 156
#define ROW_SIZ 1024
#define EXTRA_SITES 1000
#define INT_OFFSET sizeof(int)/sizeof(int)
#define DOUBLE_OFFSET sizeof(double)/sizeof(int)

#define RIM_COMMAND_STR "rim"
#define RIM_DIR "rim"
#define RIM_SUB_DIR "rim/vect"

/* this is the status number that RIM returns at the End Of Table */
#define RIM_EOT -1

/* commands to the RIMDM function */
#define LOAD "LOAD"
#define GET "GET"
#define PUT "PUT"
#define DEL "DEL"


/* C declaration of the RIM status common block */
extern long int rimcom_;

/* C declaration of the TUPLER common block */
extern long int tupler_;

#ifdef DBIO
   int Rim_buffer[ROW_SIZ];
#else
   extern int Rim_buffer[ROW_SIZ];
#endif


