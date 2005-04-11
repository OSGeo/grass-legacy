/*
 *****************************************************************
 * endian test
 *
 * G_is_little_endian ()
 *
 * Test if machine is little or big endian (required for file coding)
 *
 * Returns: 1 little endian
 *          0 big endian
 *
 * This endian test was taken from ./src.contrib/GMSL/NVIZ2.2/TOGL/apps/image.c
 * Markus Neteler
 * neteler geog.uni-hannover.de
 */


/*!
 * \brief test little ENDIAN
 *
 * Test if machine is little or big endian.
 * Returns:
 * 1 little endian
 * 0 big endian
 *
 *  \return int
 */

int G_is_little_endian (void)    
{
    union
    {
        int testWord;
        char testByte[sizeof(int)];
    } endianTest;
    
    int swapFlag;

    endianTest.testWord = 1;
    
    if (endianTest.testByte[0] == 1)
    {
        swapFlag = 1; /*true: little endian */
    }
    else
    {
        swapFlag = 0; /* false: big endian */
    }
    return swapFlag;
}
