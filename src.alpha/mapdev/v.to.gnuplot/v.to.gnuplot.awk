#!AWK
# v.to.gnuplot - extract vector data for plotting
# Author: James Darrell McCauley, Purdue University
# Usage: GRASS-GRID> v.clean file
#        GRASS-GRID> v.out.ascii i=file o=file
#        GRASS-GRID> v.to.gnuplot < $LOCATION/dig_ascii/file > data
#        GRASS-GRID> g.gnuplot
#        gnuplot> plot 'data' notitle with lines
BEGIN { n=-1; segments=0; }
{
  if (NR > 14)
  {
    if ($1 == "A" || $1 == "L")
    {
      n=segments=$2;
    }
    else
    {
      if ( n != segments )
      { 
        if (n%2 == 1)
        {
          printf "%s %s\n", prev_b, prev_a;
          printf "%s %s\n", $2,$1;
        }
        else
        {
          printf "%s %s\n", prev_b, prev_a;
          printf "%s %s\n", $2,$1;
        }
        print "";
      }
      else
        n--;
      prev_a=$1;
      prev_b=$2;
    }
  }
}
