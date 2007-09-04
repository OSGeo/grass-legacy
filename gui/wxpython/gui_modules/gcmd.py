"""
PACKAGE: gcmd

CLASSES:
    * EndOfCommand
    * Command

PURPOSE:   GRASS command interface

AUTHORS:   The GRASS Development Team
           Original author: Jachym Cepicky
           Various updates: Martin Landa

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

# use Popen class or os.popen3 method
usePopenClass = True

import os, sys
import wx # GUI dialogs...

if usePopenClass:
    try:
        import subprocess
    except:
        CompatPath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "compat")
        sys.path.append(CompatPath)
        import subprocess

# debugging ...
GuiModulePath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "gui_modules")
sys.path.append(GuiModulePath)
from debug import Debug as Debug

class EndOfCommand(Exception):
    """
    End of command indicator
    """
    def __str__(self):
        return "End of command"

class Command:
    """
    Run (GRASS) command on the background

    Parameters:
    cmd     - command string (given as list)
    stdin   - standard input stream
    verbose - verbose mode (GRASS commands '--v') (default: False)
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

    """
    def __init__ (self, cmd, stdin=None,
                  verbose=False, wait=True, dlgMsg='gui'):
        #
        # input
        #
        self.module_stdin = None
        self.cmd          = cmd

        #
        # GRASS module 
        #
        self.module = None

        #
        # output
        #
        self.module_stderr = None
        self.module_msg    = [] # list of messages (msgtype, content)

        #
        # set message formatting
        #
        message_format = os.getenv("GRASS_MESSAGE_FORMAT")
        os.environ["GRASS_MESSAGE_FORMAT"] = "gui"

        #
        # run command ...
        #
        if not usePopenClass: # do not use Popen class
            Debug.msg(4, "Command.__init__(): [popen3] cmd='%s'" % ' '.join(cmd))

            (self.module_stdin, self.module_stdout, self.module_stderr) = \
                                os.popen3(' '.join(self.cmd))
        else: # Popen class (default)
            Debug.msg(4, "Command.__init__(): [Popen] cmd='%s'" % ' '.join(cmd))

            self.module = subprocess.Popen(self.cmd,
                                           stdin=subprocess.PIPE,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.PIPE,
                                           close_fds=True)
            # set up streams
            self.module_stdin  = self.module.stdin
            self.module_stderr = self.module.stderr
            self.module_stdout = self.module.stdout

        if stdin: # read stdin if requested ...
            self.module_stdin.write(stdin)
            self.module_stdin.close()

        os.environ["GRASS_MESSAGE_FORMAT"] = message_format

        #
        # read stderr
        # ...
        self.messages = []
        self.errors   = []
        self.warnings = []
        try:
            self.__ProcessMessages() # -> messages, errors, warnings
        except EndOfCommand:
            pass

        if self.module:
            if wait:
                self.module.wait()
            self.returncode = self.module.returncode

            # failed?
            if dlgMsg and self.returncode != 0:
                if dlgMsg == 'gui': # GUI dialog
                    dlg = wx.MessageDialog(None,
                                           ("Execution failed: '%s'") % (' '.join(self.cmd)),
                                           ("Error"), wx.OK | wx.ICON_ERROR)
                    dlg.ShowModal()
                    dlg.Destroy()
                else: # otherwise 'txt'
                    print >> sys.stderr, "Execution failed: '%s'" % (' '.join(self.cmd))
                    print >> sys.stderr, "Details:"
                    for err in self.errors:
                        print >> sys.stderr, " %s" % err
        else:
            self.returncode = None # running ?

        if self.returncode is not None:
            Debug.msg (3, "Command(): cmd='%s', wait=%d, returncode=%d" % \
                       (' '.join(self.cmd), wait, self.returncode))
        else:
            Debug.msg (3, "Command(): cmd='%s', wait=%d, returncode=?" % \
                       (' '.join(self.cmd), wait))

    def __ProcessMessages(self):
        """
        Read messages/warnings/errors from stderr
        """
        msgtype = None
        content = None
        line    = None

        while True:
            line = self.module_stderr.readline()
            if not line:
                raise EndOfCommand
            if line.find(':') > -1:
                msgtype, content = line.split(":", 1)
                content = content.strip()
                if msgtype.find("GRASS_INFO_ERROR"):
                    self.errors.append(content)
                elif msgtype.find("GRASS_INFO_WARNING") > -1:
                    self.warnings.append(content)
                else:
                    self.messages.append(content)

    def ReadStdOutput(self):
        """Read standard output and return list

        Note: Remove '\n' from output (TODO: '\r\n' ??)
        """
        lineList = []
        while True:
            line = self.module_stdout.readline()
            if not line:
                break
            line = line.replace('\n', '')
            lineList.append(line)

        return lineList

# testing ...
if __name__ == "__main__":
    SEP = "-----------------------------------------------------------------------------"

    print SEP

    # d.rast verbosely, wait for process termination
    print "Running d.rast..."

    cmd = Command(cmd=["d.rast", "elevation.dem"], verbose=True, wait=True, dlgMsg='txt')

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode

    print SEP

    # v.net.path silently, wait for process termination
    print "Running v.net.path for 0 593527.6875 4925297.0625 602083.875 4917545.8125..."

    cmd = Command(cmd=["v.net.path", "in=roads@PERMANENT", "out=tmp dmax=100000", "--o"],
                  stdin="0 593527.6875 4925297.0625 602083.875 4917545.8125",
                  verbose=False,
                  wait=True, dlgMsg='txt')

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode

    print SEP

    # d.vect silently, do not wait for process termination
    # returncode will be None
    print "Running d.vect tmp..."

    cmd = Command(["d.vect", "tmp"], verbose=False, wait=False, dlgMsg='txt')

    if cmd.returncode == None:
        print "RUNNING"
    elif cmd.returncode == 0:
        print "SUCCESS"
    else:
        print "FAILURE (%d)" % cmd.returncode
