# help-scripting.tcl
# displays help screen for scripting
# created by al

help {Scripting} {-width 55} {-justify left} \
{
Scripting with tcltkgrass
---------------------------------------------------
This is a small utility to save interactively 
started commands to a shell script.

You can start a script recording session with 
Config->Scripting->Start Scripting.

If you want to end the session please select 
Config->Scripting->Stop Scripting.
You will be asked for a filename to save the shell 
script in.

All commands are saved to the shell script, but 
interactive commands (like v.digit, p.icons, p.labels 
etc.) are commented in the saved script because it 
is not possible to record all keystrokes to re-run 
these commands.

You can replay the script with 
Config->Scripting->Play Script.

There are several constraints with this though. 
No checks are done if the monitors are runnig and 
if the sequence of the commands makes any sense. 
The size and screen position of the monitors is 
not saved. 
If you start a command that does not work as 
supposed, the command is saved to the script. If 
your command raises an error (exit status not zero) 
the line will be discarded. You should manually 
edit the script file to remove or correct such 
errors.

This module gives only a starting point for your 
own script development in GRASS. You can use a 
manually created script as a skeleton for a 
more elaborate script with configurable filenames, 
loop constructs etc. Read the documentation of 
GRASS GIS for more details.


The scripting utility was written by Andreas Lange, 
(andreas.lange@rhein-main.de). If you find bugs 
please contact the GRASS maintainers.

Use at your own risk. 
All copyrights reserved, 
May 2000, Andreas Lange

}

