#include <stdio.h>

main()
{
  char *a, *b, *malloc();

  fprintf(stdout,"sizeof int = %d\n",sizeof(int));
  fprintf(stdout,"sizeof int * = %d\n",sizeof(int *));
  fprintf(stdout,"sizeof char * = %d\n",sizeof(char *));
  a = malloc(16);
  b = malloc(32);
  fprintf(stdout,"a = %x, *(a-1) = %x\n",a,before(a));
  fprintf(stdout,"b = %x, *(b-1) = %x\n",b,before(b));
}

before(a)
char **a;
{
  return(*(a-1));
}
