#!/bin/sh

gawk 'BEGIN {line = "----------------------------------------------------------------------"}\
function patch() {\
    if (n > 0) {\
        close("diff.tmp");\
        printf "\n%s\nPATCH %s :\n%s\n",line,orig,line;\
        system("cat diff.tmp");\
        printf "\nPRESS y TO APPLY THE PATCH, return TO SKIP : ";\
        getline r <"/dev/stdin";\
        if (r == "y" || r == "yes") system("patch -n " orig " diff.tmp");\
    }\
}\
{\
    if (substr($0,1,5) == "diff ") {\
        patch();
        orig = $(NF-1);\
        indiff = 1;\
        n = 0;\
    } else if (indiff) {\
        if (! match(substr($0,1,1),"[A-Za-z]")) {n++; print $0 > "diff.tmp"}\
        else indiff = 0;\
    }\
}\
END {patch(); system("rm -f diff.tmp")}' $*
