#include "Vect.h"

struct dig_head *Cur_In_Head;
struct dig_head *Cur_Out_Head;



int dig__set_cur_in_head (head) struct dig_head *head;
{ Cur_In_Head = head; return 0;}

int dig__set_cur_out_head (head) struct dig_head *head;
{ Cur_Out_Head = head; return 0;}

struct dig_head *dig__get_cur_in_head () { return Cur_In_Head; }
struct dig_head *dig__get_cur_out_head () { return Cur_Out_Head; }

/***************************** READ ************************************/
int dig__fread_port_D (	/* DOUBLE */
    double  *buf,
    int cnt,
    FILE *fp)
{
    int ret;

    if (0 >= (ret = fread (buf, sizeof (double),  cnt, fp)))
	return (ret);
    dig__double_convert (buf, buf, cnt, Cur_In_Head);
    return (cnt);
}

int dig__fread_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    int ret;

    if (0 >= (ret = fread (buf, sizeof (float),  cnt, fp)))
	return (ret);
    dig__float_convert (buf, buf, cnt, Cur_In_Head);
    return (cnt);
}

int dig__fread_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    int ret;

    if (0 >= (ret = fread (buf, sizeof (long),  cnt, fp)))
	return (ret);
    dig__long_convert (buf, buf, cnt, Cur_In_Head);
    return (cnt);
}

#ifdef INCLUDE_SHORT
int dig__fread_port_S (	/* SHORT */
    short  *buf,
    int cnt,
    FILE *fp)
{
    int ret;

    if (0 >= (ret = fread (buf, sizeof (short),  cnt, fp)))
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

    if (NULL == (lbuf = (long *) dig__convert_buffer (cnt * sizeof (long))))
	return (-1);

    if (0 >= (ret = fread (lbuf, sizeof (long), cnt, fp)))
	return (ret);
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

    if (NULL == (lbuf = (long *) dig__convert_buffer (cnt * sizeof (long))))
	return (-1);

    if (0 >= (ret = fread (lbuf, sizeof (long), cnt, fp)))
	return (ret);
    dig__long_convert_to_plus_t (lbuf, buf, cnt, Cur_In_Head);
    return (cnt);
}

int dig__fread_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return fread (buf,  sizeof (char), cnt, fp);
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
    return  fwrite (Ptmp, sizeof (double),  cnt, fp);
}

int dig__fwrite_port_F (	/* FLOAT */
    float  *buf,
    int cnt,
    FILE *fp)
{
    float *Ptmp;

    if (NULL == (Ptmp = dig__float_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, sizeof (float),  cnt, fp);
}

int dig__fwrite_port_L (	/* LONG */
    long  *buf,
    int cnt,
    FILE *fp)
{
    long *Ptmp;

    if (NULL == (Ptmp = dig__long_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, sizeof (long),  cnt, fp);
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
    return  fwrite (Ptmp, sizeof (short),  cnt, fp);
}
#endif

/* converts ints to longs */
int dig__fwrite_port_I (	/* INT->LONG */
    int  *buf,
    int cnt,
    FILE *fp)
{
    long *Ptmp;

    if (NULL == (Ptmp = dig__int_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, sizeof (long),  cnt, fp);
}

/* converts plus_t to longs */
int dig__fwrite_port_P (	/* PLUS_T->LONG */
    plus_t  *buf,
    int cnt,
    FILE *fp)
{
    long *Ptmp;

    if (NULL == (Ptmp = dig__plus_t_convert (buf, NULL, cnt, Cur_Out_Head)))
	return dig_out_of_memory();
    return  fwrite (Ptmp, sizeof (long),  cnt, fp);
}

int dig__fwrite_port_C (	/* CHAR */
    char  *buf,
    int cnt,
    FILE *fp)
{
    return  fwrite (buf, sizeof (char),  cnt, fp);
}
