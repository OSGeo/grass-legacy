#include <string.h>
#include "gis.h"
#include "portable.h"
#include "Vect.h"

struct dig_head *Cur_Head;

static char *buffer = NULL;
static int buf_alloced = 0;

static int 
buf_alloc (int needed)
{
    char *p;                                                                    
    int cnt;

    if (needed <= buf_alloced)
        return (0);
    cnt = buf_alloced;
    p = dig__alloc_space (needed, &cnt, 100, buffer, 1);
    if (p == NULL)
        return (dig_out_of_memory ());
    buffer = p;
    buf_alloced = cnt;
    return (0);
}

/***************************** READ ************************************/
/*  These are the routines to read from the Portable Vector Format.
   These routines must handle any type size conversions between the
   portable format and the native machine.
 */

/* Change byte order */
/*static void
chorder ( void * ptr, int size , int step, int count)
{
    int            i,j;
    unsigned char  c;

    for (i = 0; i < count; i++)
      {
        for (j = 0; j < size/2; j++)
          {
            c = ((unsigned char *) ptr)[j];
            ((unsigned char *) c)[j] = ((unsigned char *) c)[size-j-1];
            ((unsigned char *) c)[size-j-1] = c;
          }
	ptr += step;
      }	
}

/* read doubles from the PVF file */
int 
dig__fread_port_D (		
		    double *buf,
		    int cnt,
		    FILE * fp)
{
  int i, j, ret;
  unsigned char *c1, *c2;

  if ( Cur_Head->dbl_quick)
    {
      if (0 >= (ret = fread (buf, PORT_DOUBLE, cnt, fp)))
        return (ret);
    }  
  else
    {
      /* read into buffer */
      buf_alloc (cnt * PORT_DOUBLE);
      if (0 >= (ret = fread (buffer, PORT_DOUBLE, cnt, fp)))
        return (ret);
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
      c2 = (unsigned char *) buf;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_DOUBLE; j++)
            {
	  	c2[Cur_Head->dbl_cnvrt[j]] = c1[j];
	    }
	  c1 += PORT_DOUBLE;
	  c2 += sizeof (double);
	}
    }
  return (cnt);
}

/* read floats from the PVF file */
int 
dig__fread_port_F (		
		    float *buf,
		    int cnt,
		    FILE * fp)
{
  int i, j, ret;
  unsigned char *c1, *c2;

  if ( Cur_Head->flt_quick)
    {
      if (0 >= (ret = fread (buf, PORT_FLOAT, cnt, fp)))
        return (ret);
    }  
  else
    {
      /* read into buffer */
      buf_alloc (cnt * PORT_FLOAT);
      if (0 >= (ret = fread (buffer, PORT_FLOAT, cnt, fp)))
        return (ret);
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
      c2 = (unsigned char *) buf;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_FLOAT; j++)
            {
	  	c2[Cur_Head->flt_cnvrt[j]] = c1[j];
	    }
	  c1 += PORT_FLOAT;
	  c2 += sizeof (float);
	}
    }
  return (cnt);
}

/* read longs from the PVF file */
int 
dig__fread_port_L (		
		    long *buf,
		    int cnt,
		    FILE * fp)
{
  int i, j, ret;
  unsigned char *c1, *c2;

  if ( Cur_Head->lng_quick )
    {
#if NATIVE_LONG == PORT_LONG
      if (0 >= (ret = fread (buf, PORT_LONG, cnt, fp)))
          return (ret);
#else
      /* read into buffer */
      buf_alloc (cnt * PORT_LONG);
      if (0 >= (ret = fread (buffer, PORT_LONG, cnt, fp)))
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(long)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
  #if LONG_ORDER == ENDIAN_LITTLE
      c2 = (unsigned char *) buf;
  #else
      c2 = (unsigned char *) buf + NATIVE_LONG - PORT_LONG;
  #endif 
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */	
  #if LONG_ORDER == ENDIAN_LITTLE
	  if ( c1[PORT_LONG-1] & 0x80 )
              memset (c2, 0xff, sizeof(long)); 
  #else
	  if ( c1[0] & 0x80 )
              memset (c2, 0xff, sizeof(long)); 
  #endif 
	  memcpy (c2, c1, PORT_LONG);   
	  c1 += PORT_LONG;
	  c2 += sizeof (long);
	}
#endif 
    }  
  else
    {
      /* read into buffer */
      buf_alloc (cnt * PORT_LONG);
      if (0 >= (ret = fread (buffer, PORT_LONG, cnt, fp)))  
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(long)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
      c2 = (unsigned char *) buf;
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */	
          if ( Cur_Head->byte_order == ENDIAN_LITTLE )
	      if ( c1[PORT_LONG-1] & 0x80 )
                  memset (c2, 0xff, sizeof(long)); 
	  else
	      if ( c1[0] & 0x80 )
                  memset (c2, 0xff, sizeof(long)); 
          for (j = 0; j < PORT_LONG; j++)
	  	c2[Cur_Head->lng_cnvrt[j]] = c1[j];
	  c1 += PORT_LONG;
	  c2 += sizeof (long);
	}
    }
  return (cnt);
}

/* read ints from the PVF file */
int 
dig__fread_port_I (		
		    int *buf,
		    int cnt,
		    FILE * fp)
{
  int i, j, ret;
  unsigned char *c1, *c2;

  if ( Cur_Head->int_quick )
    {
#if NATIVE_INT == PORT_INT
      if (0 >= (ret = fread (buf, PORT_INT, cnt, fp)))
          return (ret);
#else
      /* read into buffer */
      buf_alloc (cnt * PORT_INT);
      if (0 >= (ret = fread (buffer, PORT_INT, cnt, fp)))
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(int)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
  #if INT_ORDER == ENDIAN_LITTLE
      c2 = (unsigned char *) buf;
  #else
      c2 = (unsigned char *) buf + NATIVE_INT - PORT_INT;
  #endif 
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */	
  #if INT_ORDER == ENDIAN_LITTLE
	  if ( c1[PORT_INT-1] & 0x80 )
              memset (c2, 0xff, sizeof(int)); 
  #else
	  if ( c1[0] & 0x80 )
              memset (c2, 0xff, sizeof(int)); 
  #endif 
	  memcpy (c2, c1, PORT_INT);   
	  c1 += PORT_INT;
	  c2 += sizeof (int);
	}
#endif 
    }  
  else
    {
      /* read into buffer */
      buf_alloc (cnt * PORT_INT);
      if (0 >= (ret = fread (buffer, PORT_INT, cnt, fp)))  
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(int)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
      c2 = (unsigned char *) buf;
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */	
          if ( Cur_Head->byte_order == ENDIAN_LITTLE )
	      if ( c1[PORT_INT-1] & 0x80 )
                  memset (c2, 0xff, sizeof(int)); 
	  else
	      if ( c1[0] & 0x80 )
                  memset (c2, 0xff, sizeof(int)); 
          for (j = 0; j < PORT_INT; j++)
	  	c2[Cur_Head->int_cnvrt[j]] = c1[j];
	  c1 += PORT_INT;
	  c2 += sizeof (int);
	}
    }
  return (cnt);
}

/* read shorts from the PVF file */
int 
dig__fread_port_S (		
		    short *buf,
		    int cnt,
		    FILE * fp)
{
  int i, j, ret;
  unsigned char *c1, *c2;

  if ( Cur_Head->shrt_quick )
    {
#if NATIVE_SHORT == PORT_SHORT
      if (0 >= (ret = fread (buf, PORT_SHORT, cnt, fp)))
          return (ret);
#else
      /* read into buffer */
      buf_alloc (cnt * PORT_SHORT);
      if (0 >= (ret = fread (buffer, PORT_SHORT, cnt, fp)))
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(short)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
  #if SHORT_ORDER == ENDIAN_LITTLE
      c2 = (unsigned char *) buf;
  #else
      c2 = (unsigned char *) buf + NATIVE_SHORT - PORT_SHORT;
  #endif 
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */
  #if SHORT_ORDER == ENDIAN_LITTLE
	  if ( c1[PORT_SHORT-1] & 0x80 )
              memset (c2, 0xff, sizeof(short)); 
  #else
	  if ( c1[0] & 0x80 )
              memset (c2, 0xff, sizeof(short)); 
  #endif 
	  memcpy (c2, c1, PORT_SHORT);
	  c1 += PORT_SHORT;
	  c2 += sizeof (short);
	}
#endif 
    }  
  else
    {
      /* read into buffer */
      buf_alloc (cnt * PORT_SHORT);
      if (0 >= (ret = fread (buffer, PORT_SHORT, cnt, fp)))  
          return (ret);
      /* set buffer to zero (positive numbers) */
      memset (buf, 0, cnt * sizeof(short)); 
      /* read from buffer in changed order */
      c1 = (unsigned char *) buffer;
      c2 = (unsigned char *) buf;
      for (i = 0; i < cnt; i++)
	{
	  /* set to FF if the value is negative */	
          if ( Cur_Head->byte_order == ENDIAN_LITTLE )
	      if ( c1[PORT_SHORT-1] & 0x80 )
                  memset (c2, 0xff, sizeof(short)); 
	  else
	      if ( c1[0] & 0x80 )
                  memset (c2, 0xff, sizeof(short)); 
          for (j = 0; j < PORT_SHORT; j++)
	  	c2[Cur_Head->shrt_cnvrt[j]] = c1[j];
	  c1 += PORT_SHORT;
	  c2 += sizeof (short);
	}
    }
  return (cnt);
}

/* read chars from the PVF file */
int 
dig__fread_port_C (
		    char *buf,
		    int cnt,
		    FILE * fp)
{
  return fread (buf, PORT_CHAR, cnt, fp);
}

/* read plus_t from the PVF file */
/* plus_t is defined as int so we only retype pointer and use int function */
int 
dig__fread_port_P (
		    plus_t * buf,
		    int cnt,
		    FILE * fp)
{
  int *ibuf, ret;

  ibuf = (int *) buf;

  if ((ret = dig__fread_port_I (ibuf, cnt, fp)) < 0)
    return ret;

  return (cnt);
}

/***************************** WRITE ************************************/

int 
dig__fwrite_port_D (		/* DOUBLE */
		     double *buf,
		     int cnt,
		     FILE * fp)
{
  int i,j;	
  unsigned char *c1, *c2;	
  if ( Cur_Head->dbl_quick )
      return fwrite (buf, PORT_DOUBLE, cnt, fp);
  else
    {
      buf_alloc (cnt * PORT_DOUBLE);
      c1 = (unsigned char *) buf;
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_DOUBLE; j++)
	  	c2[j] = c1[Cur_Head->dbl_cnvrt[j]];
	  c1 += sizeof (double);
	  c2 += PORT_DOUBLE;
	} 
      return fwrite (buffer, PORT_DOUBLE, cnt, fp);
    }
}

int 
dig__fwrite_port_F (		/* FLOAT */
		     float *buf,
		     int cnt,
		     FILE * fp)
{
  int i,j;	
  unsigned char *c1, *c2;	
  if ( Cur_Head->flt_quick )
      return fwrite (buf, PORT_FLOAT, cnt, fp);
  else
    {
      buf_alloc (cnt * PORT_FLOAT);
      c1 = (unsigned char *) buf;
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_FLOAT; j++)
	  	c2[j] = c1[Cur_Head->flt_cnvrt[j]];
	  c1 += sizeof (float);
	  c2 += PORT_FLOAT;
	} 
      return fwrite (buffer, PORT_FLOAT, cnt, fp);
    }
}

int 
dig__fwrite_port_L (		/* LONG */
		     long *buf,
		     int cnt,
		     FILE * fp)
{
  int i,j;	
  unsigned char *c1, *c2;	
  
  if ( Cur_Head->lng_quick )
#if NATIVE_LONG == PORT_LONG
      return fwrite (buf, PORT_LONG, cnt, fp);
#else
    {
      buf_alloc (cnt * PORT_LONG);
  #if LONG_ORDER == ENDIAN_LITTLE
      c1 = (unsigned char *) buf;
  #else
      c1 = (unsigned char *) buf + NATIVE_LONG - PORT_LONG;
  #endif 
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
        {
          memcpy (c2, c1, PORT_LONG);
          c1 += PORT_LONG;
          c2 += sizeof (long);
	}  
      return fwrite (buffer, PORT_LONG, cnt, fp);
    }  
#endif
  else
    {
      buf_alloc (cnt * PORT_LONG);
      c1 = (unsigned char *) buf;
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_LONG; j++)
	  	c2[j] = c1[Cur_Head->lng_cnvrt[j]];
	  c1 += sizeof (long);
	  c2 += PORT_LONG;
	} 
      return fwrite (buffer, PORT_LONG, cnt, fp);
    }
}

int 
dig__fwrite_port_I (		/* INT */
		     int *buf,
		     int cnt,
		     FILE * fp)
{
  int i,j;	
  unsigned char *c1, *c2;	
  
  if ( Cur_Head->int_quick )
#if NATIVE_INT == PORT_INT
      return fwrite (buf, PORT_INT, cnt, fp);
#else
    {
      buf_alloc (cnt * PORT_INT);
  #if INT_ORDER == ENDIAN_LITTLE
      c1 = (unsigned char *) buf;
  #else
      c1 = (unsigned char *) buf + NATIVE_INT - PORT_INT;
  #endif 
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          memcpy (c2, c1, PORT_INT);
          c1 += PORT_INT;
          c2 += sizeof (int);
	}
      return fwrite (buffer, PORT_INT, cnt, fp);
#endif
  else
    {
      buf_alloc (cnt * PORT_INT);
      c1 = (unsigned char *) buf;
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_INT; j++)
	  	c2[j] = c1[Cur_Head->int_cnvrt[j]];
	  c1 += sizeof (int);
	  c2 += PORT_INT;
	} 
      return fwrite (buffer, PORT_INT, cnt, fp);
    }
}

int 
dig__fwrite_port_S (		/* SHORT */
		     short *buf,
		     int cnt,
		     FILE * fp)
{
  int i,j;	
  unsigned char *c1, *c2;	
  
  if ( Cur_Head->shrt_quick )
#if NATIVE_SHORT == PORT_SHORT
      return fwrite (buf, PORT_SHORT, cnt, fp);
#else
    {
      buf_alloc (cnt * PORT_SHORT);
  #if SHORT_ORDER == ENDIAN_LITTLE
      c1 = (unsigned char *) buf;
  #else
      c1 = (unsigned char *) buf + NATIVE_SHORT - PORT_SHORT;
  #endif 
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          memcpy (c2, c1, PORT_SHORT);
          c1 += PORT_SHORT;
          c2 += sizeof (short);
	}
      return fwrite (buffer, PORT_SHORT, cnt, fp);
#endif
  else
    {
      buf_alloc (cnt * PORT_SHORT);
      c1 = (unsigned char *) buf;
      c2 = (unsigned char *) buffer;
      for (i = 0; i < cnt; i++)
	{
          for (j = 0; j < PORT_SHORT; j++)
	  	c2[j] = c1[Cur_Head->shrt_cnvrt[j]];
	  c1 += sizeof (short);
	  c2 += PORT_SHORT;
	} 
      return fwrite (buffer, PORT_SHORT, cnt, fp);
    }
}

/* plus_t is defined as int so we only retype pointer and use int function */
int 
dig__fwrite_port_P (		/* PLUS_T->INT */
		     plus_t * buf,
		     int cnt,
		     FILE * fp)
{
  return ( dig__fwrite_port_I ( (int *) buf, cnt, fp) );   	
}

int 
dig__fwrite_port_C (		/* CHAR */
		     char *buf,
		     int cnt,
		     FILE * fp)
{
  return fwrite (buf, PORT_CHAR, cnt, fp);
}

void
dig__init_head_portable ( struct dig_head *head )
{
  int i;
  
  if ( head->byte_order == DOUBLE_ORDER )
      head->dbl_quick = TRUE;
  else
      head->dbl_quick = FALSE;
  
  for ( i = 0; i < PORT_DOUBLE; i++ )
    {
      if ( head->byte_order == ENDIAN_BIG )
        head->dbl_cnvrt[i] = dbl_cnvrt[i];
      else
        head->dbl_cnvrt[i] = dbl_cnvrt[PORT_DOUBLE - i];
    }
  
  if ( head->byte_order == FLOAT_ORDER )
      head->flt_quick = TRUE;
  else
      head->flt_quick = FALSE;
  
  for ( i = 0; i < PORT_FLOAT; i++ )
    {
      if ( head->byte_order == ENDIAN_BIG )
        head->flt_cnvrt[i] = flt_cnvrt[i];
      else
        head->flt_cnvrt[i] = flt_cnvrt[PORT_FLOAT - i];
    }
  
  if ( head->byte_order == LONG_ORDER )
      head->lng_quick = TRUE;
  else
      head->lng_quick = FALSE;
  
  for ( i = 0; i < PORT_LONG; i++ )
    {
      if ( head->byte_order == ENDIAN_BIG )
        head->lng_cnvrt[i] = lng_cnvrt[i];
      else
        head->lng_cnvrt[i] = lng_cnvrt[PORT_LONG - i];
    }
  
  if ( head->byte_order == INT_ORDER )
      head->int_quick = TRUE;
  else
      head->int_quick = FALSE;
  
  for ( i = 0; i < PORT_INT; i++ )
    {
      if ( head->byte_order == ENDIAN_BIG )
        head->int_cnvrt[i] = int_cnvrt[i];
      else
        head->int_cnvrt[i] = int_cnvrt[PORT_INT - i];
    }
  
  if ( head->byte_order == SHORT_ORDER )
      head->shrt_quick = TRUE;
  else
      head->shrt_quick = FALSE;
  
  for ( i = 0; i < PORT_SHORT; i++ )
    {
      if ( head->byte_order == ENDIAN_BIG )
        head->shrt_cnvrt[i] = shrt_cnvrt[i];
      else
        head->shrt_cnvrt[i] = shrt_cnvrt[PORT_SHORT - i];
    }

 return; 
}

int 
dig__set_cur_head (head) 
    struct dig_head *head;
{ 
    Cur_Head = head; 
    return 0;
}

int
dig__byte_order_out()
{
    if ( DOUBLE_ORDER == ENDIAN_LITTLE )   
        return ( ENDIAN_LITTLE );
    else
	return ( ENDIAN_BIG );
}

int
dig__write_head ( struct Map_info *Map )
{
    unsigned char buf[GRASS_V_DIG_HEAD_LENGTH];	

    fseek (Map->dig_fp, 0L, 0);

    memset ( buf, 0, GRASS_V_DIG_HEAD_LENGTH );
    
    buf[0] = GRASS_V_VERSION_MAJOR;
    buf[1] = GRASS_V_VERSION_MINOR;
    buf[2] = GRASS_V_EARLIEST_MAJOR;
    buf[3] = GRASS_V_EARLIEST_MINOR;

    buf[4] = Map->head.byte_order;
    buf[5] = Map->head.with_z;
    
    if (0 >= dig__fwrite_port_C ( buf, GRASS_V_DIG_HEAD_LENGTH, Map->dig_fp))
        return (0);

    return (1);    
}


int
dig__read_head ( struct Map_info *Map )
{
    unsigned char buf[GRASS_V_DIG_HEAD_LENGTH];	

    fseek (Map->dig_fp, 0L, 0);
    
    if (0 >= dig__fread_port_C ( buf, GRASS_V_DIG_HEAD_LENGTH, Map->dig_fp))
        return (0);

    Map->head.Version_Major = buf[0];
    Map->head.Version_Minor = buf[1];
    Map->head.Back_Major    = buf[2];
    Map->head.Back_Minor    = buf[3];

    Map->head.byte_order    = buf[4];
    Map->head.with_z        = buf[5];

    return (1);
}



