
V_flush ()
{
    R_flush ();
    R_stabilize ();	/* needed for X windows */
}
