/* *****************************************************
*  Corrected, handles properly negative cell values now 
*    Jacques Bouchard - bouchard@onera.fr 7/98
*         
* *******************************************************/
int value (unsigned char *data, int n, int sflag)
{
    int v;

    v = *data++;
    if (sflag && v > 127) v -= 256;
    while (--n > 0) v = v * 256 + *data++;

    return v;
}
