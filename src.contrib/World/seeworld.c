# include <stdio.h>

struct { char lat, lon ;  short int n ; } H ;
struct { short int x, y ; } D ;

main ()
{
freopen ("usr/dict/world", "r", stdin) ;

while ((H.lat = getchar()) != EOF && (H.lon = getchar()) != EOF)
      {
      fread (&H.n, sizeof H.n, 1, stdin) ;
      printf ("lat: %d, lon: %d, n: %d\n", H.lat * 10, H.lon * 10, H.n) ;
      while (H.n--)
            {
            fread (&D, sizeof D.x, 2, stdin) ;
            printf ("x: %d, y: %d\n", D.x, D.y) ;
            }
      }
}
