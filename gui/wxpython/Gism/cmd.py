import grassenv
import os
class EndOfCommand(Exception):
    def __str__(self):
       return "End of command"

class Command:
    def __init__ (self,cmd,stdin=None,verbose=False):
        self.module_stdout = None
        self.module_stdin = None
        self.cmd = cmd
        self.line = None


        os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
        (self.module_stdin, self.module_stdout, self.module_stderr) = os.popen3(self.cmd)
        os.environ["GRASS_MESSAGE_FORMAT"] = "text"
       
        if stdin:
            self.module_stdin.write(stdin)
            self.module_stdin.close()

        if not verbose:
            self.RunS()

    def Run(self):
        """
        run command verbosely
        
        Returns: (msgtype, content)

        Usage:
            cmd = Command("d.rast elevation.dem")
            try:
                (msgtype,content) = cmd.RunV()
                while 1:
                    if msgtype == "GRASS_INFO_PERCENT":
                        print "Percent done: %d" % (int(content))
                    else:
                        print "General message:", content
                    (msgtype,content) = cmd.RunV()
            except EndOfCommand:
                print "end"
        """
        msgtype = None
        cont = None
        line = None

        line = self.module_stderr.readline()
        while 1:
            if not line:
                raise EndOfCommand()
            if line.find(':') > -1:
                break
            line = self.module_stderr.readline()
        msgtype, cont = line.split(":")

        cont=cont.strip()
        return (msgtype,cont.strip())

    def RunS(self):
        """
        run command silently
        
        Returns: 
            None if OK
        Usage:
            cmd = Command("d.rast elevation.dem")
            if cmd.RunS():
                print "ERRRROR"

        FIXME: maybe use os.system instead?
        """

        line = self.module_stderr.readline()
        while 1:
            if not line:
                break
            line =self.module_stderr.readline()
        return

if __name__ == "__main__":
    print "Running d.rast"
    cmd=Command("d.rast elevation.dem")
    try:
        (msgtype,content) = cmd.RunV()
        while 1:
            if msgtype == "GRASS_INFO_PERCENT":
                print "Percent done: %d" % (int(content))
            else:
                print "General message:", content
            (msgtype,content) = cmd.RunV()
    except EndOfCommand:
        print "konec"

    print "Running v.net.path for 0 593527.6875 4925297.0625 602083.875 4917545.8125"
    cmd=Command("v.net.path in=roads out=tmp --o", "0 593527.6875 4925297.0625 602083.875 4917545.8125")
    cmd.RunS()
    print "Running d.vect tmp"
    cmd = Command("d.vect tmp")
    cmd.RunS()
