erase ()
{
    char *color;
    char *get_background_color();
    char command[100];
    int n;

    sprintf (command, "Derase %s", color = get_background_color());
    delete_key ("script");
    delete_key ("cell");
    G_system (command);
}
