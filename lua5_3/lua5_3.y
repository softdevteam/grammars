%start block
%%
block
    : statlistopt retstatopt;
statlist
    : statlist stat
    | stat
    ;
statlistopt
    : statlist
    |
    ;
stat
    : ";"
    | varlist "=" explist
    | functioncall
    | label
    | "BREAK"
    | "GOTO" "NAME"
    | "DO" block "END"
    | "WHILE" exp "DO" block "END"
    | "REPEAT" block "UNTIL" exp
    | "IF" exp "THEN" block elselistopt elseopt "END"
    | "FOR" "NAME" "=" exp "," explist "DO" block "END"
    | "FOR" namelist "IN" explist "DO" block "END"
    | "FUNCTION" funcname funcbody
    | "LOCAL" "FUNCTION" "NAME" funcbody
    | "LOCAL" namelist eqexplistopt
    ;
retstatopt
    : "RETURN" explistopt semicolonopt
    |
    ;
label
    : "::" "NAME" "::"
    ;
funcname
    : "NAME" funcnamelist
    | "NAME" funcnamelist ":" "NAME"
    ;
funcnamelist
    : funcnamelist "." "NAME"
    |
    ;
varlist
    : varlist "," var
    | var
    ;
var : "NAME"
    | prefixexp "[" exp "]"
    | prefixexp "." "NAME"
    ;
explist
    : explist "," exp
    | exp
    ;
explistopt
    : explist
    |
    ;
eqexplistopt
    : "=" explist
    |
    ;
elselistopt
    : elselist
    |
    ;
elselist
    : elselist "ELSEIF" exp "THEN" block
    | "ELSEIF" exp "THEN" block
    ;
elseopt
    : "ELSE" block
    |
    ;
semicolonopt
    : ";"
    |
    ;
namelist
    : namelist "," "NAME"
    | "NAME"
    ;
exp : "NIL"
    | "FALSE"
    | "TRUE"
    | "NUMERAL"
    | literalstring
    | "..."
    | functiondef
    | prefixexp
    | tableconstructor
    | exp binop exp
    | unop exp
    ;
prefixexp
    : var
    | functioncall
    | "(" exp ")"
    ;
functioncall
    : prefixexp args
    | prefixexp ":" "NAME" args
    ;
args: "(" explistopt ")"
    | tableconstructor
    | literalstring
    ;
functiondef
    : "FUNCTION" funcbody
    ;
funcbody
    : "(" parlist ")" block "END";
parlist
    : namelist "," "..."
    | namelist
    | "..."
    |
    ;
tableconstructor
    : "{" fieldlistopt "}";
fieldlistopt
    : fieldlist fieldsepopt
    |
    ;
fieldlist
    : fieldlist fieldsep field
    | field
    ;
field
    : "[" exp "]" "=" exp
    | "NAME" "=" exp
    | exp
    ;
fieldsep
    : ","
    | ";"
    ;
fieldsepopt
    : fieldsep
    |
    ;
binop
    : "AND"
    | "OR"
    | "<"
    | ">"
    | "<="
    | ">="
    | "~="
    | "=="
    | "|"
    | "~"
    | "&"
    | "<<"
    | ">>"
    | ".."
    | "+"
    | "-"
    | "*"
    | "/"
    | "//"
    | "^"
    | "%"
    ;
unop: "NOT"
    | "-"
    | "#"
    | "~"
    ;
literalstring
    : "SHORT_STR"
    | "LONG_STR"
    ;
