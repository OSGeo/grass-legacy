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

class Digit:
    """
    Abstract digitization class
    """
    def __init__(self):
        pass

class VEdit(Digit):
    """
    Prototype of digitization class based on v.edit command

    Note: This should be replaced by VDigit class.
    """
    def AddPoint (self, map, type, x, y, z=None):
        """
        Add point/centroid to the vector map layer
        """
        if type == "centroid":
            key = "C"
        else:
            key = "P"
        
        addstring="""%s 1 1
                    %f %f\n1 1""" % (key, x, y)

        Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s, x=%f, y=%f" % \
                   (map, type, x, y))
        
        self._AddFeature (map=map, input=addstring)

    def AddLine (self, map, type, coords):
        """
        Add line/boundary to the vector map layer
        """
        if len(coords) < 2:
            return

        if type == "boundary":
            key = "B"
        else:
            key = "L"
            
        addstring="""%s %d 1\n""" % (key, len(coords))
        for point in coords:
            addstring += """%f %f\n""" % \
                (float(point[0]), float(point [1]))

        addstring += "1 1"

        Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s" % \
                   (map, type))

        self._AddFeature (map=map, input=addstring)

    def _AddFeature (self, map, input):
        """
        General method which adds feature to the vector map
        """
        command = ["v.edit", "-n", "map=%s" % map, "tool=add"]

        # run the command
        vedit = cmd.Command(cmd=command, stdin=input)
        
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
