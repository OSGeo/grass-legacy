/*
 * color specification
 * 
 */
struct Spec
   {
   CELL        index;    /* category number */ 
   int         R,G,B;    /* RGB color definition */
   struct Spec *next,    /* next color spec in list */
               *prev;    /* previous color spec in list */
   };

/* 
 * linked-list of color specifications 
 *
 */
struct SpecList
   {
   struct Spec  *first,   /* first color spec in the list */
                *last,    /* last color spec in the list */
                *ptr;     /* color spec currently of interest */
   CELL NumSpecs;         /* number of color specs in the list */
   };

/*
 * FYI
 *
 * This is GRASS color-table structure, as it is defined in gis.h 
 *
 * struct Colors
 *    {
 *    CELL min,max;  
 *    uchar *red;   
 *    uchar *grn;  
 *    uchar *blu;
 *    uchar r0,g0,b0;  
 *    };
 *
 */
