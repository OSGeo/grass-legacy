
/*
**
**  Init Vector system
**    Called by Vect_open_* ();
**
*/

Vect_init ()
{
    static int First_Time = 1;
    if (First_Time)
    {
	dig__Init_Portable_structs ();
	First_Time = 0;
    }
}
