"""
PACKAGE: digit

CLASSES:
 * VEdit
 * VDigit

PURPOSE: Digitization tool wxPython GUI prototype

         Note: Initial version under development

         Progress:
          (1) v.edit called on the background (class VEdit)
          (2) Reimplentation of v.digit (VDigit)

AUTHORS: The GRASS Development Team
         Martin Landa

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import cmd

#
class Digit:
    """
    Abstract digitization class
    """
    pass

#
class VEdit(Digit):
    """
    Prototype of digitization class based on v.edit command
    """
    def AddPoint (self, map, x, y):
        """
        Add point to the vector map
        """
        addstring="""P 1
                    %f %f""" % (x,y)
        command = """v.edit -n  map=%s tool=add""" % (map)

        # run the command
        vedit = cmd.Command(cmd=command, stdin=addstring)
        
        # result?
        if vedit.returncode == 0:
            pass
        else:
            print "FAILURE"
            for msg in vedit.module_msg:
                print msg[1]

#
class VDigit(Digit):
    """
    Prototype of digitization class based on v.digit reimplementation

    Under development (wxWidgets C/C++ background)
    """
    pass


##############################
# digitization class instance
##############################

Digit = VEdit()
