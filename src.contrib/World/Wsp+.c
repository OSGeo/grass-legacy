# include <stdio.h>
#define DEG (180.*.0001/3.141592654)

main(argc, argv) char **argv ;
{
int i, mla, mlo, f, r, b ;
short int p ;
int pla, plo ;
long n ;
struct { short int la, lo ; } ll ;

if (argc == 1)
   { printf ("Usage: Wsp+ <lat/10> <lon/10>\n") ;  exit () ; }

mla = atoi(argv[1]) ;  mlo = atoi(argv[2]) ;

freopen ("usr/dict/world.x", "r", stdin) ;
while (scanf ("%d%d%D", &pla, &plo, &n) == 3)
      if (mla == pla && mlo==plo)
         goto found ;
printf ("not found\n") ; return ;

found:
if ((f = open ("usr/dict/world", 0)) < 0)
   { perror ("usr/dict/world") ;  exit() ;}

for ( lseek (f, n, 0) ; ; )
    {
    printf ("at offset %d\n", lseek(f, 0, 1)) ;
    read (f, &pla, 1) ;  read (f, &plo, 1) ;
    printf ("pla/plo %d %d\n", pla, plo) ;
    if (pla != mla || plo != mlo)
       break ;

    read (f, &p, 2) ; printf ("%d\n", p) ;

    for (i = 0  ; i < p  ; i++)
        { read (f, &ll.la, 2) ;  read (f, &ll.lo, 2) ;
          printf ("%d %d\n", ll.la, ll.lo) ; }
	  /*
          printf ("%.4f %.4f\n", ll.la * DEG, ll.lo * DEG) ; }
	  */
    }
}
