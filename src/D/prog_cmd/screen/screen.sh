#!/bin/sh
Dclear.screen
if [ $? != 0 ]
then
    exit 1
fi
Dnew full_screen 0 100 0 100
Dchoose full_screen
exit 0
