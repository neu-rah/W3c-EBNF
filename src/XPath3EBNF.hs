{-# LANGUAGE QuasiQuotes #-}

module XPath3EBNF where
import Text.Heredoc





xpath3_ebnf=[here|
/* XML Path Language (XPath) 3.0
 * version http://www.w3.org/TR/2014/REC-xpath-30-20140408/
 * extracted from http://www.w3.org/TR/xpath-30/ on Wed Jun 21, 2017, 22:48 (UTC)
 */

XPath    ::= Expr
ParamList
         ::= Param ("," Param)*
Param    ::= "$" EQName TypeDeclaration?
FunctionBody
         ::= EnclosedExpr
EnclosedExpr
         ::= "{" Expr "}"
Expr     ::= ExprSingle ("," ExprSingle)*
ExprSingle
         ::= ForExpr
           | LetExpr
           | QuantifiedExpr
           | IfExpr
           | OrExpr
ForExpr  ::= SimpleForClause "return" ExprSingle
SimpleForClause
         ::= "for" SimpleForBinding ("," SimpleForBinding)*
SimpleForBinding
         ::= "$" VarName "in" ExprSingle
LetExpr  ::= SimpleLetClause "return" ExprSingle
SimpleLetClause
         ::= "let" SimpleLetBinding ("," SimpleLetBinding)*
SimpleLetBinding
         ::= "$" VarName ":=" ExprSingle
QuantifiedExpr
         ::= ("some" | "every") "$" VarName "in" ExprSingle ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle
IfExpr   ::= "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
OrExpr   ::= AndExpr ( "or" AndExpr )*
AndExpr  ::= ComparisonExpr ( "and" ComparisonExpr )*
ComparisonExpr
         ::= StringConcatExpr ( (ValueComp
           | GeneralComp
           | NodeComp) StringConcatExpr )?
StringConcatExpr
         ::= RangeExpr ( "||" RangeExpr )*
RangeExpr
         ::= AdditiveExpr ( "to" AdditiveExpr )?
AdditiveExpr
         ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
MultiplicativeExpr
         ::= UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
UnionExpr
         ::= IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
IntersectExceptExpr
         ::= InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
InstanceofExpr
         ::= TreatExpr ( "instance" "of" SequenceType )?
TreatExpr
         ::= CastableExpr ( "treat" "as" SequenceType )?
CastableExpr
         ::= CastExpr ( "castable" "as" SingleType )?
CastExpr ::= UnaryExpr ( "cast" "as" SingleType )?
UnaryExpr
         ::= ("-" | "+")* ValueExpr
ValueExpr
         ::= SimpleMapExpr
GeneralComp
         ::= "=" | "!=" | "<" | "<=" | ">" | ">="
ValueComp
         ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge"
NodeComp ::= "is" | "<<" | ">>"
SimpleMapExpr
         ::= PathExpr ("!" PathExpr)*
PathExpr ::= ("/" RelativePathExpr?)
           | ("//" RelativePathExpr)
           | RelativePathExpr
          /* xgc: leading-lone-slash */
RelativePathExpr
         ::= StepExpr (("/" | "//") StepExpr)*
StepExpr ::= PostfixExpr | AxisStep
AxisStep ::= (ReverseStep | ForwardStep) PredicateList
ForwardStep
         ::= (ForwardAxis NodeTest) | AbbrevForwardStep
ForwardAxis
         ::= ("child" "::")
           | ("descendant" "::")
           | ("attribute" "::")
           | ("self" "::")
           | ("descendant-or-self" "::")
           | ("following-sibling" "::")
           | ("following" "::")
           | ("namespace" "::")
AbbrevForwardStep
         ::= "@"? NodeTest
ReverseStep
         ::= (ReverseAxis NodeTest) | AbbrevReverseStep
ReverseAxis
         ::= ("parent" "::")
           | ("ancestor" "::")
           | ("preceding-sibling" "::")
           | ("preceding" "::")
           | ("ancestor-or-self" "::")
AbbrevReverseStep
         ::= ".."
NodeTest ::= KindTest | NameTest
NameTest ::= EQName | Wildcard
Wildcard ::= "*"
           | (NCName ":" "*")
           | ("*" ":" NCName)
           | (BracedURILiteral "*")
          /* ws: explicit */
PostfixExpr
         ::= PrimaryExpr (Predicate | ArgumentList)*
ArgumentList
         ::= "(" (Argument ("," Argument)*)? ")"
PredicateList
         ::= Predicate*
Predicate
         ::= "[" Expr "]"
PrimaryExpr
         ::= Literal
           | VarRef
           | ParenthesizedExpr
           | ContextItemExpr
           | FunctionCall
           | FunctionItemExpr
Literal  ::= NumericLiteral | StringLiteral
NumericLiteral
         ::= IntegerLiteral | DecimalLiteral | DoubleLiteral
VarRef   ::= "$" VarName
VarName  ::= EQName
ParenthesizedExpr
         ::= "(" Expr? ")"
ContextItemExpr
         ::= "."
FunctionCall
         ::= EQName ArgumentList
          /* xgc: reserved-function-names */
          /* gn: parens */
Argument ::= ExprSingle | ArgumentPlaceholder
ArgumentPlaceholder
         ::= "?"
FunctionItemExpr
         ::= NamedFunctionRef | InlineFunctionExpr
NamedFunctionRef
         ::= EQName "#" IntegerLiteral
          /* xgc: reserved-function-names */
InlineFunctionExpr
         ::= "function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody
SingleType
         ::= SimpleTypeName "?"?
TypeDeclaration
         ::= "as" SequenceType
SequenceType
         ::= ("empty-sequence" "(" ")")
           | (ItemType OccurrenceIndicator?)
OccurrenceIndicator
         ::= "?" | "*" | "+"
          /* xgc: occurrence-indicators */
ItemType ::= KindTest | ("item" "(" ")") | FunctionTest | AtomicOrUnionType | ParenthesizedItemType
AtomicOrUnionType
         ::= EQName
KindTest ::= DocumentTest
           | ElementTest
           | AttributeTest
           | SchemaElementTest
           | SchemaAttributeTest
           | PITest
           | CommentTest
           | TextTest
           | NamespaceNodeTest
           | AnyKindTest
AnyKindTest
         ::= "node" "(" ")"
DocumentTest
         ::= "document-node" "(" (ElementTest | SchemaElementTest)? ")"
TextTest ::= "text" "(" ")"
CommentTest
         ::= "comment" "(" ")"
NamespaceNodeTest
         ::= "namespace-node" "(" ")"
PITest   ::= "processing-instruction" "(" (NCName | StringLiteral)? ")"
AttributeTest
         ::= "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
AttribNameOrWildcard
         ::= AttributeName | "*"
SchemaAttributeTest
         ::= "schema-attribute" "(" AttributeDeclaration ")"
AttributeDeclaration
         ::= AttributeName
ElementTest
         ::= "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
ElementNameOrWildcard
         ::= ElementName | "*"
SchemaElementTest
         ::= "schema-element" "(" ElementDeclaration ")"
ElementDeclaration
         ::= ElementName
AttributeName
         ::= EQName
ElementName
         ::= EQName
SimpleTypeName
         ::= TypeName
TypeName ::= EQName
FunctionTest
         ::= AnyFunctionTest
           | TypedFunctionTest
AnyFunctionTest
         ::= "function" "(" "*" ")"
TypedFunctionTest
         ::= "function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType
ParenthesizedItemType
         ::= "(" ItemType ")"
EQName   ::= QName | URIQualifiedName

<?TOKENS?>

IntegerLiteral
         ::= Digits
DecimalLiteral
         ::= ("." Digits) | (Digits "." [0-9]*)
          /* ws: explicit */
DoubleLiteral
         ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
          /* ws: explicit */
StringLiteral
         ::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
          /* ws: explicit */
URIQualifiedName
         ::= BracedURILiteral NCName
          /* ws: explicit */
BracedURILiteral
         ::= "Q" "{" [^{}]* "}"
          /* ws: explicit */
EscapeQuot
         ::= '""'
EscapeApos
         ::= "''"
Comment  ::= "(:" (CommentContents | Comment)* ":)"
          /* ws: explicit */
          /* gn: comments */
/*QName  ::= [http://www.w3.org/TR/REC-xml-names/#NT-QName]*/
QName    ::= PrefixedName | UnprefixedName
PrefixedName
         ::= Prefix ':' LocalPart
UnprefixedName
         ::= LocalPart
Prefix   ::= NCName
LocalPart::= NCName
          /* xgc: xml-version */
/*NCName  := [http://www.w3.org/TR/REC-xml-names/#NT-NCName]*/
NCName   ::= Name - (Char* ':' Char*)	/* An XML Name, minus the ":" */
          /* xgc: xml-version */
/*Char   ::= [http://www.w3.org/TR/REC-xml#NT-Char]*/
Char     ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]	/* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
          /* xgc: xml-version */
Digits   ::= [0-9]+
CommentContents
         ::= (Char+ - (Char* ('(:' | ':)') Char*))
/*Name   ::=[https://www.w3.org/TR/REC-xml/#NT-Name]*/
Name     ::= NameStartChar (NameChar)*
NameStartChar
         ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
|]
