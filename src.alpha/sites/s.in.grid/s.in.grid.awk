# s.in.grid
# James Darrell McCauley, 25 Oct 1994, mccauley@ecn.purdue.edu
# converts output of Arc's GRIDASCII to a GRASS site list
# Usage: s.in.grid output | s.in.ascii sites=file fs='|'
BEGIN {format="%.8f|%.8f|%f\n";}
{
  if (NR==1)
     ncols=$2;
  else if (NR==2)
     nrows=$2;
  else if (NR==3)
  {
    if ($1=="xllcorner")
    {
      xllcorner=$2;
      corner=1;
    }
    else
    {
      xllcenter=$2;
      corner=0;
    }
  }
  else if (NR==4)
  {
    if ( corner == 1)
      yllcorner=$2;
    else
      yllcenter=$2;
  }
  else if (NR==5)
    cellsize=$2;
  else if (NR==6)
  {
    nodata=$2;
    if (corner==1)
    {
       xllcenter=xllcorner+0.5*cellsize;
       yllcenter=yllcorner+0.5*cellsize;
    }
  }
  else
  {
    x=xllcenter;
    y=yllcenter+(NR-7)*cellsize;
# sanity check 
    if (NF==ncols) 
      for(i=0;i<ncols;++i)
      {
        if ($(i+1) != nodata)
          printf format, x,y,$(i+1);
        x+=cellsize;
      }
  }
}
