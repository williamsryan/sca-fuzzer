from pyparsing import Keyword, Word, nums, oneOf, Suppress
from dateutil.parser import parse as parse_date

class Node(object):
    def __init__(self, tokens):
        self._tokens = tokens

class Bool(Node): 
    val: bool

    def __init__(self, tokens):
        self.val = (tokens[0]=='#t')

    def __str__(self) -> str:
        return '#t' if self.val else '#f'


class Reg(Node): 
    val: int

    def __init__(self, tokens):
        self.val = tokens[0]

    def __str__(self) -> str:
        return str(self.val)

class Bs(Node): 
    keyword: str
    val: Reg

    def __init__(self, tokens):
        self.keyword = tokens[0]
        self.val = int(tokens[1].val)

    def __str__(self) -> str:
        return "({0} {1})".format(self.keyword, self.val)


class Pred(Node):
    keyword: str
    val: Bool

    def __init__(self, tokens):
        self.keyword = tokens[0]
        self.val = tokens[1].val

    def __str__(self) -> str:
        return "({0} {1})".format(self.keyword, self.val)


class Expr(Node): 
    keyword: str
    pred: Pred
    bs: Bs

    def __init__(self, tokens):
        self.keyword = tokens[0]
        self.pred = tokens[1]
        self.bs = tokens[2]

    def __str__(self) -> str:
        return "({0} {1} {2})".format(self.keyword, self.pred, self.bs)


LBRACE,RBRACE = map(Suppress, "()")

IF = Keyword('IF')
BOOL = Keyword('BOOL')
REG = Keyword('REG')

boolval = oneOf("#t #f")
regval = Word(nums)
bs = LBRACE + REG + regval + RBRACE
pred = LBRACE + BOOL + boolval + RBRACE
expr = LBRACE + IF + pred + bs + RBRACE 

boolval.addParseAction(Bool)
regval.addParseAction(Reg)
bs.addParseAction(Bs)
pred.addParseAction(Pred)
expr.addParseAction(Expr)

class Parser:
    expr_str: str

    def __init__(self, expr):
        self.expr_str = expr
    
    def parse(self):
        cexpr = self.expr_str
        e = expr.parseString(cexpr)
        return e[0]