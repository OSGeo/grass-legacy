#include <stdio.h>
#include "usgs.h"

#define INTLEN 6
#define FLOATLEN 12
#define DOUBLELEN 24

int get_int(num)
int *num;
{
    int tmp;
    char str[INTLEN+1];

    if ((record_pos + INTLEN) > RECORD_SIZE) next_record();
    if ((buffer + INTLEN) >= buf_end) {
        record_pos += buf_end - buffer;
        buffer = buf_start;
        if (get_buf() <= 0) {
	    unlink(inf);
	    unlink(of);
	    fprintf(stderr, "\n Reading profile %d of %d\n", bas_e, P_cols);
            G_fatal_error("get_int: can't get more buffer - Unexpected end of file.\n");
        }
    }
#ifdef DEBUG
    { int i;
      printf("int:");
      for(i=0; i<25; i++) printf("%c", buffer[i]);
      printf("\n");
    }
#endif

    G_strncpy(str, buffer, INTLEN);
    str[INTLEN] = '\0';
    if (sscanf(str, " %d", &tmp) != 1) {
        *num = 0;
        return 0;
    }
    *num = tmp;
    record_pos += INTLEN;
    return INTLEN;
}


get_double(num)
double *num;
{
    double tmp;
    char str[DOUBLELEN+1], *ptr;

    if ((record_pos + DOUBLELEN) > RECORD_SIZE) next_record();
    if ((buffer + DOUBLELEN) >= buf_end) {
        record_pos += buf_start - buffer;
        buffer = buf_start;
        if (get_buf() <= 0) {
	    unlink(inf);
	    unlink(of);
	    fprintf(stderr, "\n Reading profile %d of %d\n", bas_e, P_cols);
            G_fatal_error("get_int: can't get more buffer - Unexpected end of file.\n");
        }
    }
#ifdef DEBUG
    { int i;
      printf("dbl:");
      for(i=0; i<25; i++) printf("%c", buffer[i]);
      printf("\n");
    }
#endif
    G_strncpy(str, buffer, DOUBLELEN);
    str[DOUBLELEN] = '\0';
    ptr = G_index(str, 'D');
    if (ptr != NULL) {
      *ptr = 'e';
      if (*(ptr+1) != '+') {
        *num = 0.0;
        return 0;
      }
    }
    if (sscanf(str, " %lf", &tmp) != 1)
        return 0;
    *num = tmp;
    record_pos += DOUBLELEN;
    return DOUBLELEN;
}


get_float(num)
float *num;
{
    float tmp;
    char str[FLOATLEN+1];

    if ((record_pos + FLOATLEN) > RECORD_SIZE) next_record();
    if ((buffer + FLOATLEN) >= buf_end) {
        record_pos += buf_start - buffer;
        buffer = buf_start;
        if (get_buf() <= 0) {
	    unlink(inf);
	    unlink(of);
	    fprintf(stderr, "\n Reading profile %d of %d\n", bas_e, P_cols);
            G_fatal_error("get_int: can't get more buffer - Unexpected end of file.\n");
        }
    }
#ifdef DEBUG
    { int i;
      printf("flt:");
      for(i=0; i<25; i++) printf("%c", buffer[i]);
      printf("\n");
    }
#endif

    G_strncpy(str, buffer, FLOATLEN);
    str[FLOATLEN] = '\0';
    if (sscanf(str, " %f", &tmp) != 1) {
        *num = 0.0;
        return 0;
    }
    *num = tmp;
    record_pos += FLOATLEN;
    return FLOATLEN;
}

