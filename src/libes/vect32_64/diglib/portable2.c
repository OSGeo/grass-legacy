#include <string.h>
#include "gis.h"
#include "portable.h"
#include "digit.h"

struct dig_head *Cur_In_Head;
struct dig_head *Cur_Out_Head;



int dig__set_cur_in_head (head) struct dig_head *head;
{ Cur_In_Head = head; return 0;}

int dig__set_cur_out_head (head) struct dig_head *head;
{ Cur_Out_Head = head; return 0;}

struct dig_head *dig__get_cur_in_head () { return Cur_In_Head; }
struct dig_head *dig__get_cur_out_head () { return Cur_Out_Head; }

/***************************** READ ************************************/
/*  These are the routines to read from the Portable Vector Format.
    The data sizes are defined in portable.h
    These routines must handle any type size conversions between the
     portable format and the native machine.
*/

int dig__fread_port_D (	/* DOUBLE */
    double  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    /* read doubles from the PVF file */

    if (0 >= (ret = fread (buf, PORT_DOUBLE,  cnt, fp)))
	return (ret);
#if NATIVE_DOUBLE == 8
    dig__double_convert (buf, buf, cnt, Cur_In_Head);
#else
    /* If the native double is <> 8 bytes, start coding. */
    G_fatal_error("diglib/portable2.c - Cannot handle %d byte doubles\n",
       sizeof(double));
#endif
    return (cnt);
}

int dig__fread_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    /* read floats from the PVF file */

    if (0 >= (ret = fread (buf, PORT_FLOAT,  cnt, fp)))
	return (ret);
#if NATIVE_FLOAT == 4
    dig__float_convert (buf, buf, cnt, Cur_In_Head);
#else
    /* If the native float is <> 4 bytes, start coding. */
    G_fatal_error("diglib/portable2.c - Cannot handle %d byte floats\n",
       sizeof(float));
#endif
    return (cnt);
}

int dig__fread_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    int ret;

#if NATIVE_LONG == 4
    /* There is no size translation required */
    /* read longs from the PVF file */
    if (0 >= (ret = fread (buf, PORT_LONG,  cnt, fp))) {
	return (ret);
    }
    dig__long_convert (buf, buf, cnt, Cur_In_Head);
#else
    int lngtmp;
    char *port_buf;
    int i;
    /* port_buf holds the PVF longs from the file */
    if(0 >= (port_buf = (char *)G_malloc(PORT_LONG * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");

    /* Read PORT_LONG bytes into a NATIVE_LONG buffer */
#if NATIVE_LONG == 8
  #if NATIVE_INT == PORT_LONG
    /* The native integer is the same size as a PORT_LONG,
        so just read and promote. */
    for(i=0;i<cnt;i++) {
        memcpy(&lngtmp,port_buf+(i * PORT_LONG),PORT_LONG);
        buf[i] = (long)lngtmp;
    }
  #else
    G_fatal_error("diglib/portable2.c - Cannot convert PVF longs to %d-byte ints",sizeof(int));
  #endif
#else
    /* If the native long is <> 4 or 8 bytes, start coding. */
    G_fatal_error("diglib/portable2.c - Cannot handle %d byte longs\n",
       sizeof(long));
#endif /* NATIVE_LONG == 8 */
    G_free(port_buf);
#endif /* NATIVE_LONG == 4 */
    return (cnt);
}

#ifdef INCLUDE_SHORT
int dig__fread_port_S (	/* SHORT */
    short  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    /* read shorts from the PVF file */

    if (0 >= (ret = fread (buf, PORT_SHORT,  cnt, fp)))
	return (ret);
#if NATIVE_SHORT == 2
    dig__short_convert (buf, buf, cnt, Cur_In_Head);
#else
    /* If the native short is <> 2 bytes, start coding. */
    G_fatal_error("diglib/portable2.c - Cannot handle %d byte shorts\n",
       sizeof(short));
#endif
    return (cnt);
}
#endif

int dig__fread_port_I (	/* INT */
    int  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    long *lbuf;
    /* read longs from the PVF file and convert to ints */

    if (NULL == (lbuf = (long *) dig__convert_buffer (cnt * PORT_LONG)))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) <= 0) return ret;

    dig__long_convert_to_int (lbuf, buf, cnt, Cur_In_Head);
    return (cnt);
}

int dig__fread_port_P (	/* PLUS_T */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    long *lbuf;

    if (NULL == (lbuf = (long *) dig__convert_buffer (cnt * PORT_LONG)))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) < 0) return ret;

    dig__long_convert_to_plus_t (lbuf, buf, cnt, Cur_In_Head);
    return (cnt);
}

int dig__fread_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return fread (buf, PORT_CHAR, cnt, fp);
}

/***************************** WRITE ************************************/

int dig__fwrite_port_D (	/* DOUBLE */
    double  *buf,
    int cnt,
    FILE *fp)
{
    double *Ptmp;

    if (NULL == (Ptmp = dig__double_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
#if NATIVE_DOUBLE == 8
#endif
    return  fwrite (Ptmp, PORT_DOUBLE,  cnt, fp);
}

int dig__fwrite_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    float *Ptmp;

    if (NULL == (Ptmp = dig__float_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
#if NATIVE_FLOAT == 4
#endif
    return  fwrite (Ptmp, PORT_FLOAT,  cnt, fp);
}

int dig__fwrite_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
#if NATIVE_LONG == 4
    long *Ptmp;

    if (NULL == (Ptmp = dig__long_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_LONG,  cnt, fp);
#elif NATIVE_LONG == 8
    /* convert to 4-byte longs */
  #if PORT_LONG == NATIVE_INT
    int i,tmpint;
    char *outbuf;

    outbuf = G_malloc(PORT_LONG * cnt);
    for(i=0;i<cnt;i++) { 
       tmpint = (int) buf[i];
       memcpy(outbuf+(i * PORT_LONG),&tmpint,PORT_LONG);
    }
    G_free(outbuf);
    i = fwrite(outbuf,PORT_LONG, cnt, fp);
    return i;
  #endif
#endif
}

#ifdef INCLUDE_SHORT
int dig__fwrite_port_S (	/* SHORT */
    short  *buf,
    int cnt,
    FILE *fp)
{
    short *Ptmp;

    if (NULL == (Ptmp = dig__short_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_SHORT,  cnt, fp);
}
#endif

/* converts ints to longs */
int dig__fwrite_port_I (	/* INT->LONG */
    int  *buf,
    int cnt,
    FILE *fp)
{
#if NATIVE_LONG == 4
    long *Ptmp;

    if (NULL == (Ptmp = dig__int_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_LONG,  cnt, fp);
#elif NATIVE_LONG == 8
  #if NATIVE_INT == PORT_LONG
    /* No length conversion is necessary, just lie. */
    return fwrite (buf,PORT_LONG,cnt,fp);
  #endif
#endif
}

/* converts plus_t to longs */
int dig__fwrite_port_P (	/* PLUS_T->LONG */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
#if NATIVE_LONG == 4
    long *Ptmp;

    if (NULL == (Ptmp = dig__plus_t_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_LONG,  cnt, fp);
#elif NATIVE_LONG == 8
    long *lbuf;
    int ret,i;

    lbuf = (long *)G_malloc(NATIVE_LONG * cnt);
    for (i=0;i<cnt;i++)
        lbuf[i] = (long) buf[i];
    ret = dig__fwrite_port_L(lbuf,cnt,fp);
    G_free(lbuf);
    return ret;
#else
    G_fatal_error("libes/vect/diglib/portable2.c can't handle %d-byte longs\n",
       sizeof(long));
#endif
}

int dig__fwrite_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return  fwrite (buf, PORT_CHAR,  cnt, fp);
}
