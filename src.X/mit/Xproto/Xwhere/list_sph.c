#include <stdio.h>
list_spheroids()
{
    int i;
    char *CC_spheroid_name();

    for (i = 0; CC_spheroid_name(i); i++) {
        if (i % 3 == 0)
            fprintf(stderr, "  ");
        fprintf(stderr, "%-20s ", CC_spheroid_name(i));
        if (i % 3 == 2)
            fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
}
