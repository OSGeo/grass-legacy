"""
PACKAGE:   cmd

CLASSES:
    * EndOfCommand
    * Command

PURPOSE:   Command interface

AUTHORS:   The GRASS Development Team
           Original author: Jachym Cepicky
           Martin Landa

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os, sys
import wx

from debug import Debug as Debug

usePopenClass = True

try:
   import subprocess
except:
   CompatPath = os.getenv("GISBASE") + "/etc/wx"
   sys.path.append(CompatPath)
   from compat import subprocess as subprocess

import grassenv

class EndOfCommand(Exception):
    """
    End of command indicator
    """
    def __str__(self):
       return "End of command"

class Command:
    """
    Run command on the background

    Parameters:
     cmd     - command string (given as list)
     stdin   - standard input stream
     verbose - verbose mode (GRASS commands '--v')
     wait    - wait for childer execution
     dlgMsg  - type of error message (None, gui, txt) [only if wait=True]

    Usage:
        cmd = Command(cmd=['d.rast', 'elevation.dem'], verbose=True, wait=True)

        if cmd.returncode == None:
            print 'RUNNING'
        elif cmd.returncode == 0:
            print 'SUCCESS'
        else:
            print 'FAILURE (%d)' % cmd.returncode

        for msg in cmd.module_msg:
            if msg[0] == 'GRASS_INFO_PERCENT':
                print 'Percent done: %d' % (int(msg[1]))
            else:
                print 'General message:', msg[1]
    """
    def __init__ (self, cmd, stdin=None, verbose=False, wait=True, dlgMsg='gui'):
        # input
        self.module_stdin = None
        self.cmd    = cmd

        self.module = None

        # output
        self.module_stderr = None
        self.module_msg    = [] # list of messages (msgtype, content)

        os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
        # run command
        if not usePopenClass:
           Debug.msg(3, "Command.__init__(): [popen3] cmd=%s" % ' '.join(cmd))
           (self.module_stdin, self.module_stdout, self.module_stderr) = \
                               os.popen3(' '.join(self.cmd))
        else:
           Debug.msg(3, "Command.__init__(): [Popen] cmd=%s" % ' '.join(cmd))
           self.module = subprocess.Popen(self.cmd,
                                          stdin=subprocess.PIPE,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.PIPE,
                                          close_fds=True)
           self.module_stdin  = self.module.stdin
           self.module_stderr = self.module.stderr
           self.module_stdout = self.module.stdout

        if stdin:
           self.module_stdin.write(stdin)
           self.module_stdin.close()

        os.environ["GRASS_MESSAGE_FORMAT"] = "text"

        try:
           self.Run(verbose)
        except EndOfCommand:
           pass

        if self.module:
            if wait:
                self.module.wait()
            self.returncode = self.module.returncode

            # failed?
            if dlgMsg and self.returncode != 0:
               # print error messages
               for msg in self.module_msg:
                  print >> sys.stderr, msg[1]

               if dlgMsg == "gui":
                  dlg = wx.MessageDialog(None, _("Execution failed: '%s'") % \
                                            ' '.join(self.cmd), _("Error"),
                                         wx.OK | wx.ICON_ERROR)
                  dlg.ShowModal()
                  dlg.Destroy()
               else: # otherwise 'txt'
                  print >> sys.stderr, _("Execution failed: '%s'") % self.cmd


        else:
            self.returncode = None

        Debug.msg (3, "Command(): cmd=%s, wait=%d, returncode=%d" % \
                      (self.cmd, wait, self.returncode))

    def Run(self, verbose=False):
        """
        Process messages read from stderr
        """
        msgtype = None
        content = None
        line    = None

        while 1:
            line = self.module_stderr.readline()
            if not line or line.find("GRASS_INFO_END") > -1:
                raise EndOfCommand
            if line.find(':') > -1:
                msgtype, content = line.split(":", 1)
                if verbose:
                    self.module_msg.append((msgtype, content.strip()))
                else: # write only fatal errors and warnigs
                    if msgtype.find("GRASS_INFO_ERROR") > -1 or \
                            msgtype.find("GRASS_INFO_WARNING") > -1:
                        self.module_msg.append((msgtype, content.strip()))

        return

# testing ...
if __name__ == "__main__":
    #print __doc__

    # d.rast verbosely, wait for process termination
    print "Running d.rast..."

    cmd = Command(cmd=["d.rast", "elevation.dem"], verbose=True, wait=True)

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode

    for msg in cmd.module_msg:
        if msg[0] == "GRASS_INFO_PERCENT":
            print "Percent done: %d" % (int(msg[1]))
        else:
            print "General message:", msg[1]

    # v.net.path silently, wait for process termination
    print "Running v.net.path for 0 593527.6875 4925297.0625 602083.875 4917545.8125..."

    cmd = Command(cmd=["v.net.path", "in=roads@PERMANENT", "out=tmp dmax=100000", "--o"],
                  stdin="0 593527.6875 4925297.0625 602083.875 4917545.8125",
                  verbose=False,
                  wait=True)

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode

    # d.vect silently, do not wait for process termination
    # returncode will be None
    print "Running d.vect tmp..."

    cmd = Command(["d.vect", "tmp"], verbose=False, wait=False)

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode
