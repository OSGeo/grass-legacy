/* this program determines if the data type "short" is big
 * enough for storing row/col numbers (minimum 2 bytes).
 * If not, use int. Writes the output:
 *
 *   #define ROWCOL short       if big enough
 *   #define ROWCOL int         if not big enough
 */
main()
{
    printf ("#define ROWCOL %s\n", sizeof(short) > 1 ? "short" : "int");
    exit(0);
}
