#include <stdio.h>
#include "usgs.h"

#define INTLEN 6
#define FLOATLEN 12
#define DOUBLELEN 24

char *index();

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
            fprintf(stderr, "get_int: can't get more buffer\n");
            return 0;
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
        fprintf(stderr, "get_int: error reading number: \n");
        fprintf(stderr, " (%s)\n", str);
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
            fprintf(stderr, "get_double: can't get more buffer\n");
            return 0;
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
    ptr = index(str, 'D');
    if (ptr != NULL) {
      *ptr = 'e';
      if (*(ptr+1) != '+') {
        fprintf(stderr, "get_double: error reading number:\n");
        fprintf(stderr, " (%s)\n", str);
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
            fprintf(stderr, "get_float: can't get more buffer\n");
            return 0;
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
        fprintf(stderr, "get_float: error reading number: \n");
        fprintf(stderr, " (%s)\n", str);
        *num = 0.0;
        return 0;
    }
    *num = tmp;
    record_pos += FLOATLEN;
    return FLOATLEN;
}

