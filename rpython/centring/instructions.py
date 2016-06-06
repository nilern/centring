class FiberResult(Exception):
    pass

class Opcode(object):
    pass

class Halt(Opcode):
    def __init__(self, fd):
        self._fd = fd

    def execute(self, fiber):
        raise FiberResult(fiber.fetch(self._fd))
