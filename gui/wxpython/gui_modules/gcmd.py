"""
MODULE: gcmd

CLASSES:
    * EndOfCommand
    * Command

PURPOSE:   GRASS command interface

AUTHORS:   The GRASS Development Team
           Original author: Jachym Cepicky
           Various updates: Martin Landa <landa.martin gmail.com>

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

# debugging & log window
GuiModulePath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "gui_modules")
sys.path.append(GuiModulePath)

import wxgui_utils
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
    verbose - verbose mode [0; 3]
    wait    - wait for childer execution
    dlgMsg  - type of error message (None, gui, txt) [only if wait=True]
    log     - log window or None

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
                  verbose=0, wait=True, dlgMsg='gui',
                  stdout=None, stderr=None):
        #
        # input
        #
        self.module_stdin = None
        self.cmd          = cmd
        self.dlgMsg       = dlgMsg

        #
        # set verbosity level
        #
        if verbose == 0 and '--q' not in self.cmd:
            self.cmd.append('--q')
        elif verbose == 3 and '--v' not in self.cmd:
            self.cmd.append('--v')
        else:
            verbosity = os.getenv("GRASS_VERBOSE")
            os.environ["GRASS_VERBOSE"] = str(verbose)
            if verbosity:
                os.environ["GRASS_VERBOSE"] = verbosity
        #
        # GRASS module 
        #
        self.module = None

        #
        # output
        #
        self.module_stderr = None

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

            if stdout is None:
                out = subprocess.PIPE
            elif stdout == sys.stdout:
                out = None
            else:
                out = stdout

            if stderr is None:
                err = subprocess.PIPE
            elif stderr == sys.stderr:
                err = None
            else:
                err = stderr

            self.module = subprocess.Popen(self.cmd,
                                           stdin=subprocess.PIPE,
                                           stdout=out,
                                           stderr=err,
                                           close_fds=False)
            # set up streams
            self.module_stdin  = self.module.stdin
            self.module_stderr = self.module.stderr
            self.module_stdout = self.module.stdout

        if stdin: # read stdin if requested ...
            self.module_stdin.write(stdin)
            self.module_stdin.close()

        if self.module:
            if wait:
                self.module.wait()

            if stderr is None:
                # list of messages (<- stderr)
                # -> [(type, content)] type = (error, warning, message)
                self.module_msg = self.__ProcessStdErr() # -> self.module_msg

            self.returncode = self.module.returncode
            # failed?
            if self.dlgMsg and self.returncode != 0:
                if self.dlgMsg == 'gui': # GUI dialog
                    dlg = wx.MessageDialog(None,
                                           ("Execution failed: '%s'\n\n" 
                                            "Details:\n%s") % (' '.join(self.cmd),
                                                               self.PrintModuleOutput()),
                                           ("Error"), wx.OK | wx.ICON_ERROR)
                    dlg.ShowModal()
                    dlg.Destroy()
                else: # otherwise 'txt'
                    print >> sys.stderr, "Execution failed: '%s'" % (' '.join(self.cmd))
                    print >> sys.stderr, "\nDetails:\n%s" % self.PrintModuleOutput()

        else:
            self.returncode = None # running ?

        if self.returncode is not None:
            Debug.msg (3, "Command(): cmd='%s', wait=%d, returncode=%d" % \
                       (' '.join(self.cmd), wait, self.returncode))
        else:
            Debug.msg (3, "Command(): cmd='%s', wait=%d, returncode=?" % \
                       (' '.join(self.cmd), wait))

        if message_format:
            os.environ["GRASS_MESSAGE_FORMAT"] = message_format
        else:
            os.unsetenv("GRASS_MESSAGE_FORMAT")
        
    def __ProcessStdErr(self):
        """
        Read messages/warnings/errors from stderr
        """
        lines = self.ReadErrOutput()

        msg = []

        type    = None
        content = ""
        for line in lines:
            if len(line) == 0:
                continue
            if 'GRASS_' in line: # error or warning
                if 'GRASS_INFO_WARNING' in line: # warning
                    type = "WARNING"
                elif 'GRASS_INFO_ERROR' in line: # error
                    type = "ERROR"
                elif 'GRASS_INFO_END': # end of message
                    msg.append((type, content))
                    type = None
                    content = ""
                
                if type:
                    content += line.split(':')[1].strip()
            else: # stderr
                msg.append((None, line.strip()))

        return msg

    def __ReadOutput(self, stream):
        """Read stream and return list of lines

        Note: Remove '\n' from output (TODO: '\r\n' ??)
        """
        lineList = []

        if stream is None:
            return lineList

        while True:
            line = stream.readline()
            if not line:
                break
            line = line.replace('\n', '')
            lineList.append(line)

        return lineList
                    
    def ReadStdOutput(self):
        """Read standard output and return list"""

        return self.__ReadOutput(self.module_stdout)
    
    def ReadErrOutput(self):
        """Read standard error output and return list"""
        
        return self.__ReadOutput(self.module_stderr)

    def PrintModuleOutput(self, error=True, warning=True, message=True, rest=False):
        """Print module errors, warnings, messages..."""
        
        msgString = ""
        for type, msg in self.module_msg:
            if type:
                if (type == 'ERROR' and error) or \
                        (type == 'WARNING' and warning) or \
                        (type == 'MESSAGE' and message):
                    msgString += " " + type + ": " + msg + "\n"
            else:
                msgString += " " + msg + "\n"

        return msgString

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

    cmd = Command(cmd=["v.net.path", "in=roads@PERMANENT", "out=tmp", "dmax=100000", "--o"],
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
