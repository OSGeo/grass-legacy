finish()
{

    char answer;
    char get_answer();

    printf("Do you really want to quit now? [y/n] ");
    answer = get_answer();

    if (answer == 'y')
        exit(0);

    return;

}
