flush_keyboard ()
{
    char buf[100];

    set_keyboard ();
    while (key_hit (buf))
	;
    unset_keyboard ();
}
