/* this if the comparison function for the qsort */
int dblcomp (i, j)
  double *i, *j;
{
  if (*i - *j < 0)
    return -1;
  else if (*i - *j > 0)
    return 1;
  else
    return 0;
}


