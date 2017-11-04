{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AST where

import Data.Word (Word32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens

data Program = Program [ Statement ]
               deriving  (Show,Read,Eq)

data Identifier = Identifier {
                    _id_id :: Text
                  }
                  deriving (Show,Read,Eq,Generic)

data LitType = StringLit Text | TrueLit | FalseLit | NumLit Double | Regex
             deriving (Show,Read,Eq,Generic)

data Literal = Literal LitType
               deriving (Show,Read,Eq)

data Function = Function {
      _funcName :: Maybe Identifier
    , _lam :: Lambda
    } deriving  (Show,Read,Eq)

data Lambda = Lambda {
      _patterns :: [ Pattern ]
    , _blk :: Block
    } deriving  (Show,Read,Eq,Generic)

data Block =  Block {
      _statements :: [ Statement ]
    } deriving (Show,Read,Eq)

data ForDecl = ForVar VariableDecl | ForExpr Expression
              deriving (Show,Read,Eq)

data Statement = EmptyStatement 
               | BlockStatement Block
               | ExpressionStatement Expression
               | IfStatement Expression Statement (Maybe Statement)
               | LabeledStatement Identifier Statement
               | BreakStatement (Maybe Identifier)
               | ContinueStatement (Maybe Identifier)
               | WithStatement Expression Statement
               | SwitchStatement Expression [ SwitchCase ] Bool
               | ReturnStatement (Maybe Expression)
               | ThrowStatement Expression
               | TryStatement Block (Maybe CatchClause) [ CatchClause ] (Maybe Block)
               | WhileStatement Expression Statement
               | DoWhileStatement Statement Expression
               | ForStatement (Maybe ForDecl) (Maybe Expression) (Maybe Expression) Statement
               | ForInStatement ForDecl Expression Statement Bool
               | ForOfStatement ForDecl Expression Statement
               | DebuggerStatement
               | FunctionDeclaration Function
               | VariableDeclaration VariableDecl
                 deriving (Show,Read,Eq)

data VariableDecl = VariableDecl {
        _var_decls :: [VariableDeclarator]
      } deriving (Show,Read,Eq)

data VariableDeclarator = VariableDeclarator {
        _var_patt :: Pattern
      , _var_expr :: Maybe Expression
      } deriving (Show,Read,Eq)

data ObjectKind = Init | Get | Set deriving (Show,Read,Eq)
data ObjectKey = ObjLit Literal | ObjId Identifier
               deriving (Show,Read,Eq)
data ObjectProp = ObjectProp {key :: ObjectKey
                             ,value :: Expression
                             ,kind :: ObjectKind}
                deriving (Show,Read,Eq,Generic)

data MemberProp = MemId Identifier | MemExpr Expression
                deriving (Show,Read,Eq)

data Expression = ThisExpression
                | ArrayExpression [Maybe Expression]
                | ObjectExpression [ObjectProp]
                | FunctionExpression Function
                | SequenceExpression [Expression]
                | UnaryExpression UnaryOperator Bool Expression
                | BinaryExpression BinaryOperator Expression Expression
                | AssignmentExpression AssignmentOperator Expression Expression
                | UpdateExpression UpdateOperator Expression Bool
                | LogicalExpression LogicalOperator Expression Expression
                | ConditionalExpression Expression Expression Expression
                | NewExpression Expression [Expression]
                | CallExpression Expression [Expression]
                | MemberExpression Expression [MemberProp]
                | IdentifierExpression Identifier
                | LiteralExpression Literal
                 deriving (Show,Read,Eq)

data Pattern = ObjectPattern {
                 _patt_obj :: [(ObjectKey,Pattern)]
               }
             | ArrayPattern {
                 _patt_arr :: [Maybe Pattern]
               }
             | IdentifierPattern {
                 _patt_id :: Identifier
               }
             | ExprPattern {
                 _patt_expr :: Expression
               }
               deriving (Show,Read,Eq)

data SwitchCase = SwitchCase (Maybe Expression) [Statement]
                  deriving (Show,Read,Eq)

data CatchClause = CatchClause Pattern (Maybe Expression) Block
                   deriving (Show,Read,Eq)

data ComprehensionBlock = ComprehensionBlock Pattern Expression Bool
                          deriving (Show,Read,Eq)
                        

data UnaryOperator = Negate | Positive | Bang | Tilde | TypeOf | Void | Delete
                     deriving (Show,Read,Eq)

data BinaryOperator = Equal | NotEqual | Same | NotSame | LT | LTE | GT | GTE 
                    | LShift | RShift | RRShift | Plus | Minus | Times | Div 
                    | Mod | BinOr | BinXor | BinAnd | In | InstanceOf | DotDot
                      deriving (Show,Read,Eq)

data LogicalOperator = Or | And
                     deriving (Show,Read,Eq)

data AssignmentOperator = Assign | PlusAssign | MinusAssign | MultAssign | DivAssign | ModAssign
                        | LShiftAssign | RShiftAssign | RRShiftAssign | OrAssign | XorAssign
                        | AndAssign
                          deriving (Show,Read,Eq)

data UpdateOperator = Increment | Decrement
                      deriving (Show,Read,Eq)

makeLenses ''Lambda
makeLenses ''Function
makeLenses ''Identifier
makeLenses ''Program
makeLenses ''VariableDeclarator
makeLenses ''VariableDecl
makeLenses ''Block
makeLenses ''Pattern
