/* %W% %G% */
#include <stdio.h>
#include "usgs.h"

char *index();

int get_int(num)
int *num;
{
    int tmp;
    char str[7];

    if ((buffer + 6) >= buf_end) {
        if (get_buf() <= 0) {
            fprintf(stderr, "get_int: can't get more buffer\n");
            return 0;
        }
    }
    bcopy(buffer, str, 6);
    str[6] = '\0';
    if (sscanf(str, "%d", &tmp) != 1) {
        fprintf(stderr, "get_int: error reading number: \n");
        fprintf(stderr, " (%s)\n", str);
        *num = 0;
        return 0;
    }
    *num = tmp;
    return 6;
}


get_double(num)
double *num;
{
    double tmp;
    char str[25], *ptr;

    if ((buffer + 24) >= buf_end) {
        if (get_buf() <= 0) {
            fprintf(stderr, "get_double: can't get more buffer\n");
            return 0;
        }
    }
    bcopy(buffer, str, 24);
    str[24] = '\0';
    ptr = index(str, 'D');
    if ((ptr == NULL) || (*(ptr+1) != '+')) {
        fprintf(stderr, "get_double: error reading number:\n");
        fprintf(stderr, " (%s)\n", str);
        *num = 0.0;
        return 0;
    }
    *ptr = 'e';
    if (sscanf(str, "%lf", &tmp) != 1)
        return 0;
    *num = tmp;
    return 24;
}


get_float(num)
float *num;
{
    float tmp;
    char str[13];

    if ((buffer + 12) >= buf_end) {
        if (get_buf() <= 0) {
            fprintf(stderr, "get_float: can't get more buffer\n");
            return 0;
        }
    }
    bcopy(buffer, str, 12);
    str[12] = '\0';
    if (sscanf(str, "%f", &tmp) != 1) {
        fprintf(stderr, "get_float: error reading number: \n");
        fprintf(stderr, " (%s)\n", str);
        *num = 0.0;
        return 0;
    }
    *num = tmp;
    return 12;
}

