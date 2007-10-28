"""
MODULE: gcmd

CLASSES:
    * Command
    * RunCommand

PURPOSE:   GRASS command interface

AUTHORS:   The GRASS Development Team
           Original author: Jachym Cepicky
           Various updates: Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os, sys
import time
import fcntl
from threading import Thread

import wx # GUI dialogs...

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

class Command:
    """
    Run GRASS command

    Parameters:
    cmd     - command given as list
    stdin   - standard input stream
    verbose - verbose mode [0, 3]
    wait    - wait for child execution terminated
    dlgMsg  - type of error message (None, gui, txt), only if wait is True
    stdout  - redirect standard output or None
    stderr  - redirect standard error output or None

    If stdout/err is redirected, write() method is required for the given classes.

    Usage:
        cmd = Command(cmd=['d.rast', 'elevation.dem'], verbose=3, wait=True)

        if cmd.returncode == None:
            print 'RUNNING?'
        elif cmd.returncode == 0:
            print 'SUCCESS'
        else:
            print 'FAILURE (%d)' % cmd.returncode

    """
    def __init__ (self, cmd, stdin=None,
                  verbose=0, wait=True, dlgMsg='gui',
                  stdout=None, stderr=None):

        self.cmd = cmd

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
        # set message formatting
        #
        message_format = os.getenv("GRASS_MESSAGE_FORMAT")
        os.environ["GRASS_MESSAGE_FORMAT"] = "gui"

        #
        # run command
        #
        self.cmdThread = RunCommand(cmd, stdin,
                                    stdout, stderr)
        
        #
        # start thread
        #
        self.cmdThread.start()

        if wait:
            self.cmdThread.join()
            self.cmdThread.module.wait()
            self.returncode = self.cmdThread.module.returncode
        else:
            self.cmdThread.join(0.1)
            self.returncode = None

        if self.returncode is not None:
            Debug.msg (3, "Command(): cmd='%s', wait=%s, returncode=%d, alive=%s" % \
                           (' '.join(cmd), wait, self.returncode, self.cmdThread.isAlive()))
            if dlgMsg and self.returncode != 0:
                if dlgMsg == 'gui': # GUI dialog
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
            Debug.msg (3, "Command(): cmd='%s', wait=%s, returncode=?, alive=%s" % \
                           (' '.join(cmd), wait, self.cmdThread.isAlive()))


        if message_format:
            os.environ["GRASS_MESSAGE_FORMAT"] = message_format
        else:
            os.unsetenv("GRASS_MESSAGE_FORMAT")

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
            line = line.replace('\n', '').strip()
            lineList.append(line)

        return lineList
                    
    def ReadStdOutput(self):
        """Read standard output and return list"""

        return self.__ReadOutput(self.cmdThread.module.stdout)
    
    def ReadErrOutput(self):
        """Read standard error output and return list"""
        
        return self.__ReadOutput(self.cmdThread.module.stderr)

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

    def PrintModuleOutput(self, error=True, warning=True, message=True):
        """Store module errors, warnings, messages to output string"""

        msgString = ""
        for type, msg in self.__ProcessStdErr():
            if type:
                if (type == 'ERROR' and error) or \
                        (type == 'WARNING' and warning) or \
                        (type == 'MESSAGE' and message):
                    msgString += " " + type + ": " + msg + "\n"
            else:
                msgString += " " + msg + "\n"

        return msgString


class RunCommand(Thread):
    """See Command class"""
    def __init__ (self, cmd, stdin=None,
                  stdout=None, stderr=None):

        Thread.__init__(self)

        self.cmd          = cmd
        self.stdin        = stdin
        self.stdout       = stdout
        self.stderr       = stderr

        self.module       = None

    def run(self):
        """Run command"""
        self.module = subprocess.Popen(self.cmd,
                                       stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       close_fds=False)

        if self.stdin: # read stdin if requested ...
            self.module.stdin.write(self.stdin)
            self.module.stdin.close()

        if not self.module:
            return

        if self.stdout and self.stderr:
            # make stdout/stderr non-blocking
            stdout_fileno = self.module.stdout.fileno()
            stderr_fileno = self.module.stderr.fileno()
            
            flags = fcntl.fcntl(stdout_fileno, fcntl.F_GETFL)
            fcntl.fcntl(stdout_fileno, fcntl.F_SETFL, flags| os.O_NONBLOCK)
            
            flags = fcntl.fcntl(stderr_fileno, fcntl.F_GETFL)
            fcntl.fcntl(stderr_fileno, fcntl.F_SETFL, flags| os.O_NONBLOCK)
            
            # wait for the process to end, sucking in stuff until it does end
            while self.module.poll() is None:
                try:
                    self.stdout.write(self.module.stdout.read())
                except IOError:
                    pass
                
                try:
                    self.stderr.write(self.module.stderr.read())
                except IOError:
                    pass
                
                time.sleep(0.1)
            
            # get the last output
                    
            try:
                self.stdout.write(self.module.stdout.read())
                pass
            except IOError:
                pass
            
            try:
                self.stderr.write(self.module.stderr.read())
            except IOError:
                pass


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
