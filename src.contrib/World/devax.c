# include <stdio.h>
/* Used to swab the 2-byte data */

main(argc, argv) char **argv ;
{
int i, mla, mlo, f, r, b ;
int new ;
short int p ;
short int x ;
int pla, plo ;
long n ;
struct { short int la, lo ; } ll ;

if ((f = open ("usr/dict/world", 0)) < 0)
   { perror ("usr/dict/world") ;  exit() ;}
if ((new = open ("usr/dict/world.new", 1)) < 0)
   { perror ("usr/dict/world.new") ;  exit() ;}

while(read (f, &pla, 1))
    {
    read (f, &plo, 1) ;
    write (new, &pla, 1) ;
    write (new, &plo, 1) ;
    printf ("pla/plo %d %d\n", pla, plo) ;

    read (f, &x, 2) ;
    swab(&x, &p, 2) ;
    printf ("%d\n", p) ;
    write (new, &p, 2) ;

    for (i = 0  ; i < p  ; i++)
        { read (f, &x, 2) ;  swab(&x, &ll.la, 2) ;
          read (f, &x, 2) ;  swab(&x, &ll.lo, 2) ;
          write (new, &ll.la, 2) ;
          write (new, &ll.lo, 2) ;
        }
    }
}
