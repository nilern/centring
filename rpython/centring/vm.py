
class Fiber(object):
    def __init__(self, instrs):
        self._instrs = instrs
        self._pc = 0

    def run(self):
        try:
            while self._pc < len(self._instrs):
                opc = self._pc
                self._pc += 1
                self._instrs[opc].execute(self)
        except FiberResult as res:
            return res.args[0]
