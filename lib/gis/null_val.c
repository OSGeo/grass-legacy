#ifdef DEC_ALPHA
#include <float.h>
#endif
#include "G.h"
#include "gis.h"

#ifdef __FreeBSD__
#	define ANOTHER_CELL_NULL
#endif

#ifdef linux
#	define ANOTHER_CELL_NULL
#endif

/* add other platforms here if
 *  r.mapcalc test=-129
 * fails.
 */

/*************************************************************
*   G_set_f_null_value(f, n)
*      FCELL *f;
*      int n;
*
*   G_set_d_null_value(d, n)
*      DCELL *d;
*      int n;
*
*   G_set_c_null_value(c, n)
*      CELL *c;
*      int n;
*
*   G_set_null_value(c, n, type)
*      CELL *c;
*      int n;
*      RASTER_MAP_TYPE type;
*
*   set n cells to null.
*
***************************************************************
*
*   G_is_f_null_value(f)
*      FCELL *f;
*      int n;
*
*   G_is_d_null_value(d)
*      FCELL *d;
*      int n;
*
*   G_is_c_null_value(c)
*      CELL *c;
*      int n;
*
*   G_is_null_value(c, type)
*      FCELL c;
*      int n;
*      RASTER_MAP_TYPE type;
*
*   check if cell is null
*   
***************************************************************
*   
*   G_insert_f/d/c_null_values (cell, flags, count) 
*   
*      [D/F]CELL *cell; 
*      char *flags; 
*      int count; 
*   
*      For each of the count flags which is true(!=0), set the 
*      corresponding cell to the NULL value.
*   
***************************************************************/

#define FCB          G__.fileinfo[fd]
/*--------------------------------------------------------------------------*/

/* convert type "RASTER_MAP_TYPE" into index */
#define F2I(map_type) \
	(map_type == CELL_TYPE ? 0 : (map_type == FCELL_TYPE ? 1 : 2))

static FCELL FCELL_NULL_PATTERN;
static DCELL DCELL_NULL_PATTERN;
/* the float and double patterns are not used right now
to check numbers abainst. They are only used to set a null
value. To check if the value is null comparison to itself is used */

static CELL CELL_NULL_PATTERN;
static int set_f_pattern(FCELL);
static int set_pattern(CELL);
static int set_d_pattern(DCELL);
static int embed_given_nulls( void *, char *, RASTER_MAP_TYPE, int);

int G__check_null_bit( unsigned char *flags, int bit_num,int n)
{
   int ind, offset;

   /* find the index of the unsigned char in which this bit appears */

   ind = G__null_bitstream_size(bit_num + 1) - 1;

   /* find how many unsigned chars the buffer with bit_num+1 (counting from 0
      has and subtract 1 to get unsigned char index */

   if(ind > G__null_bitstream_size(n) - 1 )
   {
      G_warning("G__check_null_bit: can't access index %d. Size of flags is %d (bit # is %d", ind, G__null_bitstream_size(n) - 1, bit_num);
      return -1;
   }
   offset = (ind+1)*8 - bit_num - 1;

   return ((flags[ind] & ( (unsigned char) 1 << offset)) != 0);
}

int G__set_flags_from_01_random(
/* given array of 0/1 of length n starting from column col
   set the corresponding  bits of flags; 
   total number of bits in flags is ncols */

   char *zero_ones,
   unsigned char *flags,
   int col,int n,int ncols)
{
   int i, k, count, size;
   unsigned char v;

   if(col==0 && n == ncols)
   {
       G__convert_01_flags(zero_ones, flags, n);
       return 0;
   }
   count = 0;
   size = G__null_bitstream_size(ncols);
   for (i = 0; i < size; i++)
   {
      v = 0;
      k = 8;
      while (k-- > 0)
      {
	 if (count >= col && count < (col+ n))  
	    v = v | ((unsigned char) zero_ones[count - col] << k);
         else if(count < ncols)
            v = v | ((unsigned char) G__check_null_bit(flags, count, ncols) 
                                                   <<k);
            /* otherwise  keep this bit the same as it was */
         count++;
      }
      flags[i] = v;
    }
    return 1;

}

int G__convert_01_flags(
   char *zero_ones,
   unsigned char *flags,
   int n)
{
   int i, k, count, size;
   unsigned char *v;

   /* pad the flags with 0's to make size multiple of 8 */
   v = flags;
   size = G__null_bitstream_size(n);
   count = 0;
   for (i = 0; i < size; i++)
   {
      *v = 0;
      k = 8;
      while (k-- > 0)
      {
	 if (count < n)  
	    *v = *v | ((unsigned char) zero_ones[count] << k);
         count++;
      }
      v++;
    }

    return 0;
}

int G__convert_flags_01(
   char *zero_ones,
   unsigned char *flags,
   int n)
{
   int i, k, count, size;
   unsigned char *v;

   count = 0;
   v = flags;
   size = G__null_bitstream_size(n);
   for (i = 0; i < size; i++)
   {
      k = 8;
      while (k-- > 0)
      {
	 if (count < n) 
	 {
	     zero_ones[count] = ((*v & ( (unsigned char) 1 << k)) != 0);
             count++;
	 }
      }
      v++;
    }

    return 0;
}

int G__init_null_bits(
   unsigned char *flags,
   int cols)
{
   int i, size;
   unsigned char *v;

   /* pad the flags with 0's to make size multiple of 8 */
   v = flags;
   size = G__null_bitstream_size(cols);
   for (i = 0; i < size; i++)
   {
      if((i+1) * 8 <= cols)
         *v = (unsigned char) 255;
      else
         *v = (unsigned char) 255 << ((i+1) * 8 - cols);
      v++;
   }

    return 0;
}

/* since on some architectures (sgi) the float whose all
bits are set to one is changed internally when it is passed
as an argument to some function ( some bits are changed to 0),
we need to store machine dependant bit pattern as  FCELL_NULL_PATTERN
and then make byte-wise comparison to this bit pattern.
*/

int G__init_null_patterns()
{
    FCELL f;
    DCELL d;
    CELL c;

    G_set_f_null_value(&f, 1);
    set_f_pattern(f);

    G_set_d_null_value(&d, 1);
    set_d_pattern(d);

    G_set_c_null_value(&c, 1);
    set_pattern(c);

    return 0;
}

static int set_f_pattern( FCELL f)
{
  FCELL_NULL_PATTERN = f;

  return 0;
}


static int set_d_pattern( DCELL d)
{
  DCELL_NULL_PATTERN = d;

  return 0;
}


static int set_pattern( CELL c)
{
  CELL_NULL_PATTERN = c;

  return 0;
}

int G__set_null_value(
    void *rast,
    int n,int null_is_zero,
    RASTER_MAP_TYPE data_type)
{
    if(null_is_zero)
    {
       G_zero((char *) rast, n * G_raster_size(data_type));
       return 0;
    }
    G_set_null_value(rast, n, data_type);

    return 0;
} 

int G_set_null_value(
    void *buf,
    int n,
    RASTER_MAP_TYPE data_type)
{
    switch (data_type)
    {
       case CELL_TYPE: G_set_c_null_value((CELL *) buf, n); break;
       case FCELL_TYPE: G_set_f_null_value((FCELL *) buf, n); break;
       case DCELL_TYPE: G_set_d_null_value((DCELL *) buf, n); break;
    }

    return 0;
} 

int G_set_f_null_value(
    FCELL *f,
    int n)
{
#ifdef DEC_ALPHA
    *f = FLT_MAX;
#else
    unsigned char *p;

    p = (unsigned char *) f;
    G__init_null_bits(p, n * sizeof(FCELL) * 8);
#endif

    return 0;
} 

int G_set_d_null_value(
    DCELL *d,
    int n)
{
#ifdef DEC_ALPHA
+ /*
+       it appears DBL_MAX can not be assigned to a float on an alpha
+       so use FLT_MAX instead
+       *d = DBL_MAX;
+ */
    *d = FLT_MAX;
#else
    unsigned char *p;

    p = (unsigned char *) d;
    G__init_null_bits(p, n * sizeof(DCELL) * 8);
#endif

    return 0;
} 

int G_set_c_null_value(
    CELL *c,
    int n)
{
    int i, size;
    unsigned char *p;
#ifdef ANOTHER_CELL_NULL
    int j;
    union {
	    unsigned char *p;
	    int *NEG_HUGE;
    } conv;
    int NEG_HUGE;

    NEG_HUGE = 0;

    size = sizeof(CELL);
    i = size * 8 - 1;
    while (i-- > 0)
       NEG_HUGE += (1 << i);

    NEG_HUGE++;

    conv.p = (unsigned char *) G_malloc(size);
    *(conv.NEG_HUGE) = NEG_HUGE;
#endif

    p = (unsigned char *) c;

#ifndef ANOTHER_CELL_NULL
    size = sizeof(CELL);
    G__init_null_bits(p, n * size * 8);
#endif

    i = n;
    while(i-- > 0)
    {
#ifdef ANOTHER_CELL_NULL
       for(j = 0; j < size; j++){
          p[j]=conv.p[j];
       }
#else
       *p = *p >> 1;
#endif
       p += size;
    }

#ifdef ANOTHER_CELL_NULL
    G_free(conv.p);
#endif

    return 0;
} 

int G_is_f_null_value(FCELL *f)
{
#ifdef DEC_ALPHA
    return (*f == FLT_MAX );
#else
/*    if(!(*f<0.) && !(*f>0.) && !(*f==0.))*/
    return (*f!=*f);
#endif
}

int G_is_d_null_value(DCELL *d)
{

#ifdef DEC_ALPHA
    return (*d == FLT_MAX );
#else
/*    if(!(*d<0.) && !(*d>0.) && !(*d==0.))*/
    return (*d!=*d);
#endif
}

int G_is_c_null_value(CELL *c)
{
    int i;
    CELL temp;

    temp = *c;
    for(i = 0; i < sizeof (CELL); i++)
    {
       if(((unsigned char*)&temp)[i] != 
               ((unsigned char*)&CELL_NULL_PATTERN)[i])
              return 0;
    }
    return 1;
}

int G_is_null_value(
    void *rast,
    RASTER_MAP_TYPE data_type)
{
    switch(data_type)
    {
       case CELL_TYPE: return G_is_c_null_value((CELL *) rast); 
       case FCELL_TYPE: return G_is_f_null_value((FCELL *) rast);
       case DCELL_TYPE: return G_is_d_null_value((DCELL *) rast);
       default: G_warning("G_is_null_value: wrong data type!");
		return 0;
    }
}

int G_insert_f_null_values(
    FCELL *fcell,
    char *null_row,
    int ncols)
{
    return embed_given_nulls((void *) fcell, null_row, FCELL_TYPE, ncols);
}

int G_insert_d_null_values(
    DCELL *dcell,
    char *null_row,
    int ncols)
{
    return embed_given_nulls((void *) dcell, null_row, DCELL_TYPE, ncols);
}

int G_insert_c_null_values(
    CELL *cell,
    char *null_row,
    int ncols)
{
    return embed_given_nulls((void *) cell, null_row, CELL_TYPE, ncols);
}

int G_insert_null_values(
    void *rast,
    char *null_row,
    int ncols,
    RASTER_MAP_TYPE data_type)
{
    return embed_given_nulls(rast, null_row, data_type, ncols);
}

static int embed_given_nulls(
    void *cell,
    char *nulls,
    RASTER_MAP_TYPE map_type,
    int ncols)
{
    int i, ind;
    CELL *c;
    FCELL *f;
    DCELL *d;

    c = (CELL *) cell;
    f = (FCELL *) cell;
    d = (DCELL *) cell;

    ind = F2I(map_type);

    for(i = 0; i < ncols; i++)
    {
      if(nulls[i])
         switch (ind)
         {
            case 0:  G_set_c_null_value(c+i, 1); break;
            case 1:  G_set_f_null_value(f+i, 1); break;
            case 2:  G_set_d_null_value(d+i, 1); break;
         }
    }
    return 1;
}

