verify_perms(group, other)
{
    char ques[1024];
    char *explain_perms();

    sprintf (ques, "Permissions selected as follows:\n\n");
    strcat (ques, "GROUP: "); strcat (ques, group?"yes\n":"no\n");
    strcat (ques, "OTHER: "); strcat (ques, other?"yes\n":"no\n");
    strcat (ques, "\n");
    strcat (ques, explain_perms (group, other, 1));
    strcat (ques, "\n\nIs this ok? ");

    return G_yes (ques,1);
}
