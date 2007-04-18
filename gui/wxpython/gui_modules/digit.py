"""
MODULE: digit

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
from debug import Debug as Debug

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

    Note: This should be replaced by VDigit class.
    """
    def AddPoint (self, map, x, y):
        """
        Add point to the vector map
        """
        addstring="""P 1
                    %f %f""" % (x,y)

        Debug.msg (3, "VEdit.AddPoint(): x=%f, y=%f" % (x, y))
        self._AddFeature (map=map, input=addstring)

    def _AddFeature (self, map, input):
        """
        General method which adds feature to the vector map
        """
        command = """v.edit -n  map=%s tool=add""" % (map)

        # run the command
        vedit = cmd.Command(cmd=command, stdin=input)
        
        # result?
        if vedit.returncode == 0:
            pass
        else:
            print "FAILURE (%d)" % vedit.returncode
            print "cmd=%s; input=%s" % (command, input)
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
