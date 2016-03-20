from pprint import pprint
import os
import sexpdata as sx
import sys

# Function defs
#   - return type
#   - arguments
#   - expressions (body)

# Lambdas (functions)
# Closures (via symbol table)

# Function calls
#   - function name
#   - arguments
#   - types

# Prefix to infix
# Type inference

class Parser(object):
    def __init__(self, filename):
        self.filename = filename
        self.tree = []
        self.line = 1
        self.column = 1
        self.root = {}

    def parse(self, source):
        self.parse_list(source, self.root)

    def _skip_line(self, source):
        i = 0
        while source[i] != "\n":
            i += 1
        return source

    def _token(self, source):
        # Comment?
        if source[0] == ";":
            source = self._skip_line(source)


    def parse_list(self, source, env):
        pass


class Compiler(object):
    def __init__(self):
        pass

    def compile(self, cmd, ast, output):
        print("Writing %s" % output)


if __name__ == "__main__":
    for filename in sys.argv[1:]:
        with open(filename, "rt") as file:
            p = Parser(filename)
            ast = p.parse(file.read())

            output = os.path.splitext(filename)[0]
            c = Compiler()
            c.compile(["g++", "-xc++", "-"], ast, output)
