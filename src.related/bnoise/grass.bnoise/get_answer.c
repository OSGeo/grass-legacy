
char
get_answer()
{
    char inputchar;
    char dummy;

    while(1)
    {
        inputchar = getchar();
        dummy = getchar();
        if (inputchar == 'Y')
          inputchar = 'y';
        if (inputchar == 'N')
          inputchar = 'n';
        if (inputchar == 'n' || inputchar == 'y')
            break;
        printf("Please enter y or n\n");
    }

    return inputchar;

}
