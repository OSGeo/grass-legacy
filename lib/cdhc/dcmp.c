int dcmp (i, j)
    double *i, *j;
{
    if (*i < *j)
        return  -1;
 
    if (*i > *j)
        return 1;
 
    return 0;
}

