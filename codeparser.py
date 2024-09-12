import ply.lex as lex
import ply.yacc as yacc


#List of Reserved Keywords
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'exit': 'EXIT',
    'read': 'READ',
    'write': 'WRITE',
    'return': 'RETURN',
    'int': 'INT',
    'float': 'FLOAT'
}

# List of tokens
tokens = [
    'IDENTIFIER', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LP', 'RP',
    'LBR', 'RBR',
    'LBK', 'RBK',
    'SC', 'ASSIGN', 'CM', #'SQ',
    'GT', 'LT', 'GE', 'LE', 'EQ', 'NE',
    'AND', 'OR', 'NOT', 'FLOATCON', 'INTCON'#, 'EXPR',# 'LETTER', 'DIGIT', 'POSITIVE'
] + list(reserved.values())



# Regular expression rules for tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LP = r'\('
t_RP = r'\)'
t_LBR = r'\{'
t_RBR = r'\}'
t_ASSIGN = r'='
t_SC = r';'
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_EQ = r'=='
t_NE = r'!='
t_AND = r'&&'
t_CM = r','
t_LBK = r'\['
t_RBK = r'\]'
t_NOT = r'!'
t_OR = r'\|\|'

# Define a rule for ignoring whitespace
t_ignore = ' \t'



# Parser Rules

def p_Program(p):
    '''Program : DeclList Procedures
               | Procedures'''

def p_Procedures(p):
    '''Procedures : ProcedureDecl Procedures
                  | ProcedureDecl'''

def p_ProcedureDecl(p):
    '''ProcedureDecl : ProcedureHead ProcedureBody'''

def p_ProcedureHead(p):
    '''ProcedureHead : FunctionDecl DeclList
                     | FunctionDecl'''

def p_FunctionDecl(p):
    '''FunctionDecl : Type IDENTIFIER LP RP LBR'''

def p_ProcedureBody(p):
  '''ProcedureBody : StatementList RBR'''

def p_DeclList(p):
    '''DeclList : Type IdentifierList SC
                 | DeclList Type IdentifierList SC'''

def p_IdentifierList(p):
    '''IdentifierList : VarDecl
                      | IdentifierList CM VarDecl'''

def p_VarDecl(p):
    '''VarDecl : IDENTIFIER
                | IDENTIFIER LBK INTCON RBK'''

def p_Type(p):
  '''Type : INT
          | FLOAT'''

def p_StatementList(p):
    '''StatementList : Statement
                     | StatementList Statement'''

def p_Statement(p):
    '''Statement : Assignment
                 | IfStatement
                 | WhileStatement
                 | IOStatement
                 | ReturnStatement
                 | ExitStatement
                 | CompoundStatement'''

def p_Assignment(p):
    '''Assignment : Variable ASSIGN Expr SC'''
    print(f"{p[0]} = {p[2]}")

def p_IfStatement(p):
    # '''IfStatement : IF TestAndThen ELSE CompoundStatement
    #                 | IF TestAndThen'''
    '''IfStatement : IF LP Expr RP Statement ELSE Statement'''

# def p_TestAndThen(p):
#    # '''TestAndThen : Test CompoundStatement'''
#    'TestAndThen : LP Expr RP Statement'

# def p_Test(p):
#     '''Test : LP Expr RP'''

def p_WhileStatement(p):
    '''WhileStatement : WhileToken WhileExpr Statement'''

def p_WhileToken(p):
    '''WhileToken : WHILE'''

def p_WhileExpr(p):
    '''WhileExpr : LP Expr RP'''

def p_IOStatement(p):
    '''IOStatement : READ LP Variable RP SC
                    | WRITE LP Expr RP SC
                    | WRITE LP StringConstant RP SC'''

def p_ReturnStatement(p):
    '''ReturnStatement : RETURN Expr SC'''

def p_ExitStatement(p):
    '''ExitStatement : EXIT SC'''

def p_CompoundStatement(p):
    '''CompoundStatement : LBR StatementList RBR'''

def p_Expr(p):
    '''Expr : Expr AND SimpleExpr
            | Expr OR SimpleExpr
            | SimpleExpr
            | NOT SimpleExpr'''

def p_SimpleExpr(p):
    '''SimpleExpr : SimpleExpr EQ AddExpr
                   | SimpleExpr NE AddExpr
                   | SimpleExpr LE AddExpr
                   | SimpleExpr LT AddExpr
                   | SimpleExpr GE AddExpr
                   | SimpleExpr GT AddExpr
                   | AddExpr'''

def p_AddExpr(p):
    '''AddExpr : AddExpr PLUS MulExpr
                | AddExpr MINUS MulExpr
                | MulExpr'''

def p_MulExpr(p):
    '''MulExpr : MulExpr TIMES Factor
                | MulExpr DIVIDE Factor
                | Factor'''

def p_Factor(p):
    '''Factor : Variable
              | Constant
              | IDENTIFIER LP RP
              | LP Expr RP'''

def p_Variable(p):
    '''Variable : IDENTIFIER
                | IDENTIFIER LBK Expr RBK'''

def p_StringConstant(p):
    '''StringConstant : STRING'''

def p_Constant(p):
    '''Constant : INTCON
                | FLOATCON'''

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'IDENTIFIER')    # Check for reserved words
    return t

def t_STRING(t):
    r'(\'[^\']*\')'
    return t

def t_FLOATCON(t):
  r'\d+\.\d+'
  t.value = float(t.value)
  return t

def t_INTCON(t):
  r'\d+'
  t.value = int(t.value)
  return t

# def t_LETTER(t):
#   r'[A-Za-z]'
#   t.type = 'LETTER'
#   return t

# def t_DIGIT(t):
#   r'[0-9]'
#   t.type = 'DIGIT'
#   return t

# def t_POSITIVE(t):
#   r'[1-9]'
#   t.type = 'POSITIVE'
#   return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character {t.value[0]!r}")
    t.lexer.skip(1)

def t_eof(t):
    return None

def p_error(p):
    if p:
         print("Syntax error at token", p.type)
         # Just discard the token and tell the parser it's okay.
         parser.errok()
    else:
         print("Syntax error at EOF")

# Build the lexer and parser
lexer = lex.lex()
parser = yacc.yacc()

# Test the parser with sample input
data = '''
int main() {
int x, y = 2;
float z_9 = 4.5
read(x); read(y);
while ((x!=0) || (y!=0)) {
write (x*y);
read (x); read (y);
write('string');
}
exit;
}
'''


lexer.input(data)
while True:
    tok = lexer.token()
    if not tok: 
        break
    print(tok)

parser.parse(data, lexer=lexer)

