/* Function: Erase		P.W. Carlson		April 1990 */

extern int SCREEN_RIGHT, SCREEN_BOTTOM;

Erase()
{
    put_chr('E');
    put_int(0);
    put_int(0);
    put_int(SCREEN_RIGHT);
    put_int(SCREEN_BOTTOM);
}
