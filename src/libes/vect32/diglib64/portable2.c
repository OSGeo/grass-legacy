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
*/

int dig__fread_port_D (	/* DOUBLE */
    double  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    char *p_buf;

    /* read doubles from the PVF file */
    if(0 >= (p_buf = (char *)G_malloc(PORT_DOUBLE * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");

    if (0 >= (ret = fread (p_buf, PORT_DOUBLE,  cnt, fp))) {
        if(ferror(fp)) perror("dig__fread_port_D:");
        if(feof(fp)) G_warning("End of file\n");
	return (ret);
    }
    dig__to_double(buf,p_buf,cnt,Cur_In_Head);
    G_free(p_buf);

    return (ret);
}

int dig__fread_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    char *p_buf;
    /* read floats from the PVF file */
    if(0 >= (p_buf = (char *)G_malloc(PORT_FLOAT * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");

    if (0 >= (ret = fread (p_buf, PORT_FLOAT, cnt, fp)))
	return (ret);

    dig__to_float (buf, p_buf, cnt, Cur_In_Head);
    G_free(p_buf);

    return (cnt);
}

int dig__fread_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    char *p_buf;

    if(0 >= (p_buf = (char *)G_malloc(PORT_LONG * cnt)))
        G_fatal_error("Out of memory reading longs from vector file\n");
    /* port_buf holds the PVF longs from the file */
    if (0 >= (ret = fread (p_buf, PORT_LONG,  cnt, fp)))
	return (ret);

    /* Read PORT_LONG bytes into a NATIVE_LONG buffer */
    dig__to_long (buf, p_buf, cnt, Cur_In_Head);
    G_free(p_buf);

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

    dig__short_convert (buf, buf, cnt, Cur_In_Head);

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

    if (NULL == (lbuf = (long *) G_malloc (cnt * sizeof(long))))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) <= 0) return ret;
    dig__long_convert_to_int (buf, lbuf, cnt, Cur_In_Head);
    G_free(lbuf);

    return (cnt);
}

int dig__fread_port_P (	/* PLUS_T */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
    int ret;
    long *lbuf;

    if (NULL == (lbuf = (long *) G_malloc (cnt * sizeof(long))))
	return (-1);

    if((ret = dig__fread_port_L(lbuf,cnt,fp)) < 0) return ret;
    dig__long_convert_to_plus_t (buf, lbuf, cnt, Cur_In_Head);
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
    double  *buf,
    int cnt,
    FILE *fp)
{
    char *Ptmp;

    if (NULL == (Ptmp = dig__from_double (NULL, buf, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_DOUBLE,  cnt, fp);
}

int dig__fwrite_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    char *Ptmp;

    if (NULL == (Ptmp = dig__from_float (NULL, buf, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_FLOAT,  cnt, fp);
}

int dig__fwrite_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    char *Ptmp;

    if (NULL == (Ptmp = dig__from_long (NULL, buf, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_LONG,  cnt, fp);
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
    char *Ptmp;

    if (NULL == (Ptmp = dig__from_int (NULL, buf, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, PORT_LONG,  cnt, fp);
}

/* converts plus_t to longs */
int dig__fwrite_port_P (	/* PLUS_T->LONG */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
    /* This is unnecessary, since the Plus_t datatype is
        currently defined as a long.  Call it pedantic. */
    char *Ptmp;
    int ret;
#ifdef PLUS_T_ISNT_LONG
    long *l_tmp;

    if(NULL >= (l_tmp = (long *)G_malloc(cnt * sizeof(long)))) {
        return dig_out_of_memory();

    for(i=0;i<cnt,i++) l_tmp[i] = (long)buf[i];

    if (NULL == (Ptmp = dig__from_long (NULL, l_tmp, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    G_free(l_tmp);
#else
    if (NULL == (Ptmp = dig__from_long (NULL, (long *)buf, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
#endif
    ret = fwrite (Ptmp, PORT_LONG,  cnt, fp);
    G_free(Ptmp);

    return ret;
}

int dig__fwrite_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return  fwrite (buf, PORT_CHAR,  cnt, fp);
}
