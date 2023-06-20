from pyparsing import Keyword, Word, nums, oneOf, Suppress, Literal

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

class Pc(Node):
    keyword: str

    def __init__(self, tokens):
        self.keyword = tokens[0]

    def __str__(self) -> str:
        return str(self.keyword)
    
class Bs(Node):
    keyword: str
    val: (Reg | Pc)

    def __init__(self, tokens):
        self.keyword = tokens[0]
        self.val = int(tokens[1].val)

    def __str__(self) -> str:
        if type(self.val) is Reg:
            return "({0} {1})".format(self.keyword, self.val)
        elif type(self.val) is Pc:
            return str(self.val.keyword)

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
PC = Literal('PC')

boolval = oneOf("#t #f")
# TODO: looks like this parser fails on negative values. Update later if this is a use case.
# I did find a result that showed a negative value, then this failed. Not sure if the issue is here
# or with the value that was synthesized.
regval = Word(nums)

reg_bs = LBRACE + REG + regval + RBRACE
pc = LBRACE + PC + RBRACE
pred = LBRACE + BOOL + boolval + RBRACE
expr = LBRACE + IF + pred + (reg_bs | pc) + RBRACE

# This expects the form:
# (IF (BOOL #f) (REG 12))

boolval.addParseAction(Bool)
regval.addParseAction(Reg)
reg_bs.addParseAction(Bs)
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
