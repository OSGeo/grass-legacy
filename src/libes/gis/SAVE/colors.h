struct Colors
{
    CELL min,max    ;   /* min,max color numbers               */
    uchar *red      ;   /* red, green, blu (0-255)             */
    uchar *grn      ;   /* allocated as needed                 */
    uchar *blu      ;
    uchar r0,g0,b0  ;   /* red, green, blue for cat 0          */
    int nalloc;
} ;
