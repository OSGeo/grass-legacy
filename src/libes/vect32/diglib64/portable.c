#include <string.h>
#include "gis.h"
#include "portable.h"
#include "digit.h"
/*
**  Major modification for 64-bit compatibility  
**     12/27/1999   Bill Hughes
**  This file contains the routines to translate the PVF buffers
**   into native data elements.  In this incarnation, these 
**   functions cannot deal with non-portable vector files (version 3)
**   since the sizes are (potentially) wrong by the time the
**   execution reaches this point.
**
**  Original code:
**  Written by Dave Gerdes  9/1988
**  US Army Construction Engineering Research Lab
*/
union type_conv {
	double d;
	float  f;
	long   l;
	short  s;
	unsigned char   c[PORT_DOUBLE];
};
static union type_conv u;

int dig__Init_portable_code(int portable)
{
    /* The "First Time Checkout" aka "Run-Time Endian Test" is
       not required in this library.  This function is for
       compatibility with the 32-bit diglib.   */
    return 0;
}

int dig__fill_head_portable (struct dig_head *head)
{
    /* The header information that was 'filled' from this
        function is not part of the header for this
        implementation.  This function is a NOP for
        compatibility with other Vlib functions.
    */
    return 0;
}

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
*/

int dig__fread_port_D (	/* DOUBLE */
    double  *buf,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register char *p_buf,*in;

    /* read doubles from the PVF file */
    if(0 >= (p_buf = (char *)G_malloc(PORT_DOUBLE * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");

    if (0 >= (ret = fread (p_buf, PORT_DOUBLE,  cnt, fp))) {
        if(ferror(fp)) perror("dig__fread_port_D:");
        if(feof(fp)) G_warning("End of file\n");
	return (ret);
    }

    in = p_buf;
    for (i = 0 ; i < cnt ; i++) {
        u.d = 0.0L;
        for (j = 0 ; j < PORT_DOUBLE ; j++)
            u.c[dbl_cnvrt[j]] = *in++;
        buf[i] = u.d;
    }
    G_free(p_buf);

    return (ret);
}

int dig__fread_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register char *p_buf, *in;
    /* read floats from the PVF file */
    if(0 >= (p_buf = (char *)G_malloc(PORT_FLOAT * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");

    if (0 >= (ret = fread (p_buf, PORT_FLOAT, cnt, fp)))
	return (ret);

    in = p_buf;
    for (i = 0 ; i < cnt ; i++) {
        u.d = 0.0;
        for (j = 0 ; j < PORT_FLOAT ; j++)
            u.c[flt_cnvrt[j]] = *in++;
        buf[i] = u.f;
    }
    G_free(p_buf);

    return (cnt);
}

int dig__fread_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register char *p_buf,*in;

    if(0 >= (p_buf = (char *)G_malloc(PORT_LONG * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");
    /* port_buf holds the PVF longs from the file */
    if (0 >= (ret = fread (p_buf, PORT_LONG,  cnt, fp)))
	return (ret);

    in = p_buf;
    /* Read PORT_LONG bytes into a NATIVE_LONG buffer */
    for (i = 0 ; i < cnt ; i++) {
        u.l = 0L;
        for (j = 0 ; j < PORT_LONG ; j++)
            u.c[lng_cnvrt[j]] = *in++;
        buf[i] = u.l;
    }
    G_free(p_buf);

    return (cnt);
}

#ifdef INCLUDE_SHORT
int dig__fread_port_S (	/* SHORT */
    short  *out,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register short tmp;
    register char *Ptmp,*in;
    /* read shorts from the PVF file */

    if(NULL >= (Ptmp = (char *)G_malloc(cnt * PORT_SHORT)))
        G_fatal_error("diglib: out of memory");
    if (0 >= (ret = fread (Ptmp, PORT_SHORT,  cnt, fp)))
	return (ret);

    in = Ptmp;
    for (i = 0 ; i < cnt ; i++) {
	for (j = 1 ; j <= PORT_SHORT ; j++) {
	    u.c[shrt_cnvrt[j]] = *in++;
	}
	out[i] = u.s;
    }
    G_free(Ptmp);

    return (cnt);
}
#endif

int dig__fread_port_I (	/* INT */
    int  *buf,
    int cnt,
    FILE *fp)
{
    register int i, ret;
    register long *lbuf;
    /* read longs from the PVF file and convert to ints */

    if (NULL == (lbuf = (long *) G_malloc (cnt * sizeof(long))))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) <= 0) return ret;
    for (i = 0 ; i < cnt ; i++)
        buf[i] = (int) lbuf[i];
    G_free(lbuf);

    return (cnt);
}

int dig__fread_port_P (	/* PLUS_T */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
    /* Plus_t data is stored as long */
    /* the Plus_t datatype _is_ long, so this conversion is pedantic */
    register int i, ret;
    register long *lbuf;

    if (NULL == (lbuf = (long *) G_malloc (cnt * sizeof(long))))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) <= 0) return ret;
    for (i = 0 ; i < cnt ; i++)
        buf[i] = (plus_t) lbuf[i];
    G_free(lbuf);

    return (cnt);
}

int dig__fread_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    fread (buf, PORT_CHAR, cnt, fp);
    return cnt;
}

/***************************** WRITE ************************************/

int dig__fwrite_port_D (	/* DOUBLE */
    double  *in,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register char *out,*Ptmp;

    if((char *)NULL >= (out = (char *)G_malloc(cnt * PORT_DOUBLE)))
        G_fatal_error("Out of Memory in diglib\n");

    Ptmp = out;
    for (i = 0 ; i < cnt ; i++) {
        u.d = in[i];
        for (j = 0 ; j < PORT_FLOAT ; j++)
            *out++ = u.c[dbl_cnvrt[j]];
    }
    ret = fwrite (Ptmp, PORT_DOUBLE,  cnt, fp);
    G_free(Ptmp);

    return ret;
}

int dig__fwrite_port_F (	/* FLOAT */
    float  *in,
    int cnt,
    FILE *fp)
{
    register int i, j, ret;
    register char *out,*Ptmp;

    if((char *)NULL >= (out = (char *)G_malloc(cnt * PORT_FLOAT)))
        G_fatal_error("Out of Memory in diglib\n");

    Ptmp = out;
    for (i = 0 ; i < cnt ; i++) {
        u.f = in[i];
        for (j = 0 ; j < PORT_FLOAT ; j++)
            *out++ = u.c[flt_cnvrt[j]];
    }
    ret = fwrite (Ptmp, PORT_FLOAT,  cnt, fp);
    G_free(Ptmp);

    return ret;
}

int dig__fwrite_port_L (	/* LONG */
    long  *in,
    int cnt,
    FILE *fp)
{
    register char *out,*Ptmp;
    register int i, j, ret;

    if((char *)NULL >= (out = (char *)G_malloc(cnt * PORT_LONG)))
        G_fatal_error("Out of Memory in diglib\n");

    Ptmp = out;
    for (i = 0 ; i < cnt ; i++) {
        u.l = in[i];
        for (j = 0 ; j < PORT_LONG ; j++)
            *out++ = u.c[lng_cnvrt[j]];
    }
    ret = fwrite (Ptmp, PORT_LONG,  cnt, fp);
    G_free(Ptmp);

    return ret;
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

/* store ints as longs */
int dig__fwrite_port_I (	/* INT->LONG */
    int  *in,
    int cnt,
    FILE *fp)
{
    register long *l_tmp;
    register int i, ret;

    if((long *)NULL >= (l_tmp = (long *)G_malloc(cnt * sizeof(long))))
        return dig_out_of_memory();

    for(i=0;i<cnt;i++) l_tmp[i] = (long)in[i];
    ret = dig__fwrite_port_L (l_tmp, cnt, fp);
    G_free(l_tmp);

    return ret;
}

/* store plus_t as long */
int dig__fwrite_port_P (	/* PLUS_T->LONG */
    plus_t  *in,
    int cnt,
    FILE *fp)
{
    /* This is unnecessary, since the Plus_t datatype is
        currently defined as a long.  Call it pedantic. */
    register int i, ret;
    register long *l_tmp;

    if((long *)NULL >= (l_tmp = (long *)G_malloc(cnt * sizeof(long))))
        return dig_out_of_memory();

    for(i=0;i<cnt;i++) l_tmp[i] = (long)in[i];
    ret = dig__fwrite_port_L (l_tmp, cnt, fp);
    G_free(l_tmp);

    return ret;
}

int dig__fwrite_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return  fwrite (buf, PORT_CHAR,  cnt, fp);
}
