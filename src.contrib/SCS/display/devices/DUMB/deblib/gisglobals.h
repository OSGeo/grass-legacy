/* FILENAME: gisglobals.h
   This file must be included in any program that uses the raster and
   mouse functions.        Paul W. Carlson, July 1987.
 */

int mouse_x, mouse_y;                    /* mouse coordinates  */
int wind_x1, wind_y1, wind_x2, wind_y2;  /* window coordinates */

/*==== Array of bit values that are ORed together for color patterns =====

    Bit patterns for bit planes:

	25% pixels set:  00 10 00 10  even lines
			 10 00 10 00  odd lines

	50% pixels set:  01 10 01 10  even lines
			 10 01 10 01  odd lines

	75% pixels set:  11 01 11 01  even lines
			 01 11 01 11  odd lines			      */

unsigned char bit_vals[2][5][8] =
            /*     0   1   2   3   4   5   6   7   <----  x & 7
                ----------------------------------                      */

               {   0,  0,  0,  0,  0,  0,  0,  0,   /*   0%,  even lines */
                   0,  0, 32,  0,  0,  0,  2,  0,   /*  25%,  even lines */
                   0, 64, 32,  0,  0,  4,  2,  0,   /*  50%,  even lines */
                 128, 64,  0, 16,  8,  4,  0,  1,   /*  75%,  even lines */
                 128, 64, 32, 16,  8,  4,  2,  1,   /* 100%,  even lines */

                   0,  0,  0,  0,  0,  0,  0,  0,   /*   0%, odd lines */
                 128,  0,  0,  0,  8,  0,  0,  0,   /*  25%, odd lines */
                 128,  0,  0, 16,  8,  0,  0,  1,   /*  50%, odd lines */
                   0, 64, 32, 16,  0,  4,  2,  1,   /*  75%, odd lines */
                 128, 64, 32, 16,  8,  4,  2,  1 }; /* 100%, odd lines */


/*=============== Array of color pattern numbers ========================

The color pattern numbers = i | (j << 3) | (k << 6)  where i, j, and k are
the codes for the percentage of red, green, and blue bits set.  The codes
are: 0 = 0%, 1 = 25%, 2 = 50%, 3 = 75%, and 4 = 100%.

   % blue --->     0   25   50  75  100
               ---------------------------*/ 
color_pats[] = {   0,  64, 128, 192, 256,   /*   0% green */
                   8,  72, 136, 200, 264,   /*  25% green */
 /*  0% red  */   16,  80, 144, 208, 272,   /*  50% green */
                  24,  88, 152, 216, 280,   /*  75% green */
                  32,  96, 160, 224, 288,   /* 100% green */

                   1,  65, 129, 193, 257,   /*   0% green */
                   9,  73, 137, 201, 265,   /*  25% green */
/*  25% red  */   17,  81, 145, 209, 273,   /*  50% green */
                  25,  89, 153, 217, 281,   /*  75% green */
                  33,  97, 161, 225, 289,   /* 100% green */

                   2,  66, 130, 194, 258,   /*   0% green */
                  10,  74, 138, 202, 266,   /*  25% green */
/*  50% red  */   18,  82, 146, 210, 274,   /*  50% green */
                  26,  90, 154, 218, 282,   /*  75% green */
                  34,  98, 162, 226, 290,   /* 100% green */

                   3,  67, 131, 195, 259,   /*   0% green */
                  11,  75, 139, 203, 267,   /*  25% green */
/*  75% red  */   19,  83, 147, 211, 275,   /*  50% green */
                  27,  91, 155, 219, 283,   /*  75% green */
                  35,  99, 163, 227, 291,   /* 100% green */

                   4,  68, 132, 196, 260,   /*   0% green */
                  12,  76, 140, 204, 268,   /*  25% green */
/* 100% red  */   20,  84, 148, 212, 276,   /*  50% green */
                  28,  92, 156, 220, 284,   /*  75% green */
                  36, 100, 164, 228, 292 }; /* 100% green */
