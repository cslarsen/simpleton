from pprint import pprint
import sexpdata as sx
import sys

def gensym(name):
    if not hasattr(gensym, "_counter"):
        gensym._counter = -1
    gensym._counter += 1
    return "%s_%d" % (name, gensym._counter)

class Argument(object):
    def __init__(self, name, type):
        self.type = type
        self.name = name

    def emit(self):
        write = sys.stdout.write
        write("%s %s" % (self.type, self.name))

class Function(object):
    def __init__(self, name, args, body):
        self.name = name
        self.args = args
        self.body = body
        self.return_type = "double"

    def emit_sig(self):
        write = sys.stdout.write
        write("%s" % self.return_type)
        write(" %s" % self.name)

        write("(")
        for n, arg in enumerate(self.args):
            if n>0:
                write(", ")
            arg.emit()

        write(")")

    def emit(self):
        name = gensym("function")
        write = sys.stdout.write

        write = sys.stdout.write
        self.emit_sig()
        write("\n")
        write("{\n")

        for expr in self.body[:-1]:
            write("  ")
            expr.emit()
            write(";\n")

        write("  return ")
        self.body[-1].emit()
        write(";\n")

        write("}\n")

class Call(object):
    def __init__(self, name, args):
        self.name = name
        self.args = args
        self.infix = False

    def emit_infix(self):
        write = sys.stdout.write
        for n, arg in enumerate(self.args):
            if n > 0:
                write(self.name)
            if isinstance(arg, str):
                write('"%s"' % arg.replace('"', '\\"'))
            else:
                write(arg.value())

    def emit(self):
        if self.infix:
            return self.emit_infix()
        write = sys.stdout.write
        write("%s(" % self.name)
        for n, arg in enumerate(self.args):
            if n > 0:
                write(", ")
            if isinstance(arg, str):
                write('"%s"' % arg.replace('"', '\\"'))
            elif isinstance(arg, list):
                translate_expr(arg).emit()
            elif isinstance(arg, int):
                write(str(arg))
            else:
                write(arg.value())
        write(")")

def translate_func_name(name):
    return {
        "print": "printf",
    }.get(name, name)

def translate_expr(src):
    func = src[0].value()
    args = []
    for arg in src[1:]:
        if isinstance(arg, str):
            args.append(arg.replace("\n", "\\n"))
        else:
            args.append(arg)

    call = Call(translate_func_name(func), args)
    if func in ["*", "-", "/"]:
        call.infix = True
    return call

def func(funcs, f):
    name = f[1][0].value()
    args = map(lambda v: Argument(v.value(), "double"), f[1][1:])
    body = []

    for expr in f[2:]:
        body.append(translate_expr(expr))

    funcs[name] = Function(name, args, body)

def parse(source):
    t = sx.loads(source)
    funcs = {}
    main = []
    for l in t:
        if l[0] == sx.Symbol("define"):
            func(funcs, l)
        else:
            main.append(translate_expr(l))

    return funcs, main

class Body(object):
    def __init__(self, body):
        self.body = body

    def emit(self):
        write = sys.stdout.write
        for expr in self.body[:-1]:
            write("  ")
            expr.emit()
            write(";\n")

        write("  return ")
        self.body[-1].emit()
        write(";\n")

def translate(source):
    funcs, main = parse(source)
    write = sys.stdout.write
    write("#include <stdio.h>\n")
    write("\n")
    for func in funcs.values():
        func.emit()
    write("\n")
    write("int main(int argc, char** argv)\n")
    write("{\n")
    Body(main).emit()
    write("}\n")

if __name__ == "__main__":
    for filename in sys.argv[1:]:
        with open(filename, "rt") as file:
            translate("(%s)" % file.read())
