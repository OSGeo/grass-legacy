/******************************************************************************/
/*                                                                            */
/*     define.h    <fcntl.h> and LTPlus definitions (incl structures)         */
/*                                                                            */
/******************************************************************************/

#include <fcntl.h> 
#define STRLIST         96

/* extern char *ioctl();     removes some indirection warnings  */
typedef struct  
    {
        char button;
        long rawcoord[2];
        long scoord[2];
    } GINFO ;

typedef struct {
        long  action;       /**  0=send, 1=read, 2=wait, 3=baud **/
        long  value;        /**  action value  **/
        char  string[96];   /** used to hold query strings **/
    } ACTION;

    /******** digitizer binary format decoding information **************/
typedef struct {
        long b_no;          /** byte number of x, y, b **/
        long mask;          /** mask of x, y, b sequences **/
        long shift;         /** shifting require for x, y, b locations */
    } RECIPE;

        /******** digitizer device specific information **************/
typedef struct {
            /******* source filename ************/
        char digcursor_fname[256]; /* read the data setup file */
            /******* setup information **********/
        long baud;        /* 300, 600, 1200, 1800, 2400, 4800, 9600, 19200 */
        long parity;         /* parity: none=0, odd=1, even=2 */
        long data_bits;      /* 5,6,7,8 (doesn't include parity, default=8) */
        long stop_bits;      /* on serial line. The choice is 1, or 2  */
            /******* user preference/decoding information */
        long debounce[2];       /* [0] = delay,   [1] = repete rate */
        long relative_coords; /* relative = 1 or absolute = 0 */
        double units_per_inch; /*sensitivity & map-inch size(def=1000dg|200ms)*/
        long sign_type;    /* -1=none,0=0negative,1=1negative,2=2s-complement */
        long y_positive;     /* 0= up,   1 = down     default=0 */
        long x_positive;     /* 0=right,  1=left      default=0 */
            /******* bin/ascii flag **************/
        long file_type;                          /** 0=binary, 1=ascii      **/
            /******* binary and ascii decoding info ***/
        long packet_length[2];  /*** [0] = min length (without opt. bytes)  **/
                                /*** [1] = max length (with optional bytes) **/
            /******* binary decoding info ********/
        unsigned char sync_mask;    /** byte mask that is non-zero for sync **/
        unsigned char sync_offbits; /** byte mask for 0 sync bits **/
        long sync_bytes;     /** mask of bytes with sync info **/
        RECIPE xrecipe[100];
        RECIPE yrecipe[100];
        RECIPE brecipe[100];
        unsigned long xsign_mask;        /** mask for the sign bit of x **/
        unsigned long ysign_mask;        /** mask for the sign bit of y **/
        long xrecipe_length;    /** binary x decoding info **/
        long yrecipe_length;    /** binary y decoding info **/
        long brecipe_length;    /** binary button decoding **/
            /******* ascii decoding info  ********/
        long comma_bytes[10];                    /** ascii file sync info   **/
        long comma_cnt;
        long x_byte1, x_byten;                   /** ascii x decoding info  **/
        long y_byte1, y_byten;                   /** ascii y decoding info  **/
        long b_byte;                             /** ascii button decoding  **/
            /******* start, stop, & query device information **/
        long no_start_actions;
        ACTION start[40];
        long no_query_actions;
        ACTION query[10];
        long no_stop_actions;
        ACTION stop[40];
    }    DIGDEVICE;
    
