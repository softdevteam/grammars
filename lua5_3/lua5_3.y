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
// Lua has 12 precedence levels which we encode as exp[0-11] (where "exp0" is,
// for convenience, simply called "exp").
exp : exp "OR" exp1
    | exp1
    ;
exp1: exp1 "AND" exp2
    | exp2
    ;
exp2: exp2 "<" exp3
    | exp2 ">" exp3
    | exp2 "<=" exp3
    | exp2 ">=" exp3
    | exp2 "~=" exp3
    | exp2 "==" exp3
    | exp3
    ;
exp3: exp3 "|" exp4
    | exp4
    ;
exp4: exp4 "~" exp5
    | exp5
    ;
exp5: exp5 "&" exp6
    | exp6
    ;
exp6: exp6 "<<" exp7
    | exp6 ">>" exp7
    | exp7
    ;
exp7: exp8 ".." exp7
    | exp8
    ;
exp8: exp8 "+" exp9
    | exp8 "-" exp9
    | exp9
    ;
exp9: exp9 "*" exp10
    | exp9 "/" exp10
    | exp9 "//" exp10
    | exp9 "%" exp10
    | exp10
    ;
exp10
    : "NOT" exp10
    | "#" exp10
    | "-" exp10
    | "~" exp10
    | exp11
    ;
exp11
    : exp12 "^" exp10
    | exp12
    ;
exp12
    : "NIL"
    | "FALSE"
    | "TRUE"
    | "NUMERAL"
    | literalstring
    | "..."
    | functiondef
    | prefixexp
    | tableconstructor
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
literalstring
    : "SHORT_STR"
    | "LONG_STR"
    ;
