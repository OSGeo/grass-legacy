/*
 code "borrowed" from JPEG-5 config code
*/

#include <stdio.h>

int is_shifting_signed (arg)
     long arg;
/* See whether right-shift on a long is signed or not. */
{
  long res = arg >> 4;

  if (res == -0x7F7E80CL) {	/* expected result for signed shift */
    return 1;			/* right shift is signed */
  }

  /* see if unsigned-shift hack will fix it. */
  /* we can't just test exact value since it depends on width of long... */
  res |= (~0L) << (32-4);

  if (res == -0x7F7E80CL) {	/* expected result now? */
    return 0;			/* right shift is unsigned */
  }

  fprintf(stderr, "Right shift isn't acting as I expect it to.\n");
  fprintf(stderr, "I fear the DCT software will not work at all.\n\n");
  exit(1);
}

main()
{

  if (is_shifting_signed(-0x7F7E80B1L)) {
    printf("do not define RIGHT_SHIFT_IS_UNSIGNED\n");
  }
  else {
    printf("define RIGHT_SHIFT_IS_UNSIGNED\n");
  }

}
