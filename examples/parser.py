from pyparsing import Keyword, Word, nums, oneOf, Suppress, Literal, Optional

class Node(object):
    def __init__(self, tokens):
        self._tokens = tokens

class Bool(Node):
    val: bool

    def __init__(self, tokens):
        self.val = (tokens[0] == '#t')

    def __str__(self) -> str:
        return '#t' if self.val else '#f'

class Reg(Node):
    val: int

    def __init__(self, tokens):
        self.val = tokens[0]

    def __str__(self) -> str:
        return str(self.val)

class Mem(Node):
    val: int

    def __init__(self, tokens):
        self.val = tokens[0]

    def __str__(self) -> str:
        return str(self.val)
    
class Pc(Node):
    keyword: str

    def __init__(self, tokens):
        self.keyword = tokens[0]
    
    def __str__(self) -> str:
        return self.keyword
    
class Bs(Node):
    keyword: str
    val: Reg | Mem

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

LBRACE, RBRACE = map(Suppress, "()")

IF = Keyword('IF')
BOOL = Keyword('BOOL')
REG = Keyword('REG')
LOAD = Keyword('MEM-LOAD')
STORE = Keyword('MEM-STORE')
PC = Literal('PC')

boolval = oneOf("#t #f")
# TODO: looks like this parser fails on negative values. Update later if this is a use case.
# I did find a result that showed a negative value, then this failed. Not sure if the issue is here
# or with the value that was synthesized.
regval = Word(nums)
memval = Word(nums)

reg_bs = LBRACE + REG + regval + RBRACE
mem_bs = LBRACE + (LOAD | STORE) + memval + RBRACE
pc = LBRACE + PC + RBRACE
pred = LBRACE + BOOL + boolval + RBRACE
# mem_load = LBRACE + LOAD + memval + RBRACE
# mem_store = LBRACE + STORE + memval + regval + RBRACE
expr = LBRACE + IF + pred + (reg_bs | mem_bs | pc) + RBRACE

boolval.addParseAction(Bool)
regval.addParseAction(Reg)
reg_bs.addParseAction(Bs)
memval.addParseAction(Mem)
mem_bs.addParseAction(Bs)
pc.addParseAction(Pc)
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
