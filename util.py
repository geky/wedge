

def getline(line):
    while line and not isinstance(line, int):
        if isinstance(line, list):
            for l in line:
                line = getline(l)
                if line:
                    break
        else:
            try:
                line = line.line
            except AttributeError:
                pass

    return line

class CompileException(Exception):
    def __init__(self, message, line=None):
        self.message = message
        self.line = getline(line)

    def __str__(self):
        if self.line:
            return "%s line %d" % (self.message, self.line)
        else:
            return "%s" % self.message
    
