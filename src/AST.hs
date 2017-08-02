{-# LANGUAGE DeriveGeneric #-}

module AST where

import Data.Word (Word32)
import Data.Text (Text)
import GHC.Generics (Generic)

data Node a = Node a (Maybe SourceLocation)
              deriving (Show,Read,Eq,Generic)
type ExprNode = Node Expression
type StateNode = Node Statement
type BlkNode = Node Block
type IdNode = Node Identifier
type PatNode = Node Pattern

data SourceLocation = SourceLocation { source :: Maybe Text
                                     , start :: Position
                                     , end :: Position
                                     } deriving (Show,Read,Eq,Generic)

data Position = Position { line :: Word32 -- TODO add the >= 1 to the type
                         , column :: Word32
                         } deriving (Show,Read,Eq,Generic)

data Identifier = Identifier { name :: Text }
                  deriving (Show,Read,Eq,Generic)

data LitType = StringLit Text | BoolLit Bool | NumLit Float | Regex
             deriving (Show,Read,Eq,Generic)

data Literal = Literal (Maybe LitType)
               deriving (Show,Read,Eq)

data Function = Function (Maybe IdNode) Lambda
                deriving  (Show,Read,Eq)

data Program = Program [ StateNode ]
               deriving  (Show,Read,Eq)

data LambdaBody = LBlk BlkNode | LExpr ExprNode
                  deriving (Show,Read,Eq)

data Lambda = Lambda {params :: [ PatNode ]
                     , defaults :: [ ExprNode ]
                     , rest :: Maybe IdNode
                     , body :: LambdaBody
                     , generator :: Bool
                     , expression :: Bool } deriving  (Show,Read,Eq,Generic)

data Block =  Block [ StateNode ]
           deriving (Show,Read,Eq)

data ForDecl = ForVar (Node VariableDecl) | ForExpr (ExprNode)
              deriving (Show,Read,Eq)

data Statement = EmptyStatement 
               | BlockStatement Block
               | ExpressionStatement ExprNode
               | IfStatement ExprNode StateNode (Maybe StateNode)
               | LabeledStatement IdNode StateNode
               | BreakStatement (Maybe IdNode)
               | ContinueStatement (Maybe IdNode)
               | WithStatement ExprNode StateNode
               | SwitchStatement ExprNode [ Node SwitchCase ] Bool
               | ReturnStatement (Maybe ExprNode)
               | ThrowStatement ExprNode
               | TryStatement BlkNode (Maybe (Node CatchClause)) [ Node CatchClause ] (Maybe BlkNode)
               | WhileStatement ExprNode StateNode
               | DoWhileStatement StateNode ExprNode
               | ForStatement (Maybe ForDecl) (Maybe ExprNode) (Maybe ExprNode) StateNode
               | ForInStatement ForDecl ExprNode StateNode Bool
               | ForOfStatement ForDecl ExprNode StateNode
               | LetStatement [Node VariableDeclarator] StateNode
               | DebuggerStatement
               | FunctionDeclaration Function
               | VariableDeclaration VariableDecl
                 deriving (Show,Read,Eq)

data VariableKind = Var | Let | Const deriving (Show,Read,Eq)

data VariableDecl = VariableDecl [Node VariableDeclarator] VariableKind
                    deriving (Show,Read,Eq)

data VariableDeclarator = VariableDeclarator PatNode (Maybe ExprNode)
                          deriving (Show,Read,Eq)

data ObjectKind = Init | Get | Set deriving (Show,Read,Eq)
data ObjectKey = ObjLit (Node Literal) | ObjId IdNode
               deriving (Show,Read,Eq)
data ObjectProp = ObjectProp {key :: ObjectKey
                             ,value :: ExprNode
                             ,kind :: ObjectKind}
                deriving (Show,Read,Eq,Generic)

data MemberProp = MemId IdNode | MemExpr ExprNode
                deriving (Show,Read,Eq)

data Expression = ThisExpression
                | ArrayExpression [Maybe ExprNode]
                | ObjectExpression [ObjectProp]
                | FunctionExpression Function
                | ArrowExpression Lambda
                | SequenceExpression [ExprNode]
                | UnaryExpression UnaryOperator Bool ExprNode
                | BinaryExpression BinaryOperator ExprNode ExprNode
                | AssignmentExpression AssignmentOperator ExprNode ExprNode
                | UpdateExpression UpdateOperator ExprNode Bool
                | LogicalExpression LogicalOperator ExprNode ExprNode
                | ConditionalExpression ExprNode ExprNode ExprNode
                | NewExpression ExprNode [ExprNode]
                | CallExpression ExprNode [ExprNode]
                | MemberExpression ExprNode MemberProp Bool
                | YieldExpression (Maybe ExprNode)
                | ComprehensionExpression ExprNode [Node ComprehensionBlock] (Maybe ExprNode)
                | GeneratorExpression ExprNode [Node ComprehensionBlock] (Maybe ExprNode)
                | GraphExpression Word32 (Node Literal)
                | GraphIndexExpression Word32
                | LetExpression [(PatNode, Maybe ExprNode)] ExprNode
                | IdentifierExpression Identifier
                | LiteralExpression Literal
                 deriving (Show,Read,Eq)

data Pattern = ObjectPattern [(ObjectKey,Pattern)]
             | ArrayPattern [Maybe PatNode]
             | IdentifierPattern Identifier
             | ExprPattern Expression
               deriving (Show,Read,Eq)

data SwitchCase = SwitchCase (Maybe ExprNode) [StateNode]
                  deriving (Show,Read,Eq)

data CatchClause = CatchClause PatNode (Maybe ExprNode) BlkNode
                   deriving (Show,Read,Eq)

data ComprehensionBlock = ComprehensionBlock PatNode ExprNode Bool
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

