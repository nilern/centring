from rply import LexerGenerator, ParserGenerator
from rply.token import BaseBox
import sys

import centring.value

### (Asm) Lexer

lg = LexerGenerator()

lg.add('SYMBOL', '[a-zA-Z\-]+')
lg.add('INT', '-?[0-9]+')
lg.add('LPAREN', '\(')
lg.add('RPAREN', '\)')

lg.ignore('[ \t\r\f\v]+')

lexer = lg.build()

### (Asm) Parser

pg = ParserGenerator(['SYMBOL', 'INT', 'LPAREN', 'RPAREN'])

@pg.production('sexpr : SYMBOL')
def parse_symbol(p):
    return p[0]

@pg.production('sexpr : INT')
def parse_int(p):
    return IntBox(int(p[0].getstr()))

@pg.production('sexpr : LPAREN RPAREN')
def parse_sexpr(p):
    return ListBox([])

@pg.production('sexpr : LPAREN list_elems RPAREN')
def parse_sexpr(p):
    return p[1]

@pg.production('list_elems : sexpr')
def parse_elems(p):
    return ListBox([p[0]])

@pg.production('list_elems : sexpr list_elems')
def parse_elems(p):
    return ListBox([p[0]] + p[1].get_list())

parser = pg.build()

###

class IntBox(BaseBox):
    def __init__(self, i):
        self.int_val = i

class ListBox(BaseBox):
    def __init__(self, ls):
        self.list_val = ls

    def get_list(self):
        return self.list_val

### main

def main(argv):
    print parser.parse(lexer.lex(argv[1]))
    return 0

def target(driver, args):
    return main, None

if __name__ == '__main__':
    main(sys.argv)
