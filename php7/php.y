/*
   +----------------------------------------------------------------------+
   | Zend Engine                                                          |
   +----------------------------------------------------------------------+
   | Copyright (c) 1998-2018 Zend Technologies Ltd. (http://www.zend.com) |
   +----------------------------------------------------------------------+
   | This source file is subject to version 2.00 of the Zend license,     |
   | that is bundled with this package in the file LICENSE, and is        |
   | available through the world-wide-web at the following url:           |
   | http://www.zend.com/license/2_00.txt.                                |
   | If you did not receive a copy of the Zend license and are unable to  |
   | obtain it through the world-wide-web, please send a note to          |
   | license@zend.com so we can mail you a copy immediately.              |
   +----------------------------------------------------------------------+
   | Authors: Andi Gutmans <andi@php.net>                                 |
   |          Zeev Suraski <zeev@php.net>                                 |
   |          Nikita Popov <nikic@php.net>                                |
   +----------------------------------------------------------------------+
*/

%left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE
%left ','
%left T_LOGICAL_OR
%left T_LOGICAL_XOR
%left T_LOGICAL_AND
%right T_PRINT
%right T_YIELD
%right T_DOUBLE_ARROW
%right T_YIELD_FROM
%left '=' T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL
%left '?' ':'
%right T_COALESCE
%left T_BOOLEAN_OR
%left T_BOOLEAN_AND
%left '|'
%left '^'
%left '&'
%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP
%nonassoc '<' T_IS_SMALLER_OR_EQUAL '>' T_IS_GREATER_OR_EQUAL
%left T_SL T_SR
%left '+' '-' '.'
%left '*' '/' '%'
%right '!'
%nonassoc T_INSTANCEOF
%right '~' T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST '@'
%right T_POW
%right '['
%nonassoc T_NEW T_CLONE
%left T_ELSEIF
%left T_ELSE
%left T_ENDIF
%right T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC

%token T_LNUMBER
%token T_DNUMBER
%token T_STRING
%token T_VARIABLE
%token T_CONSTANT_ENCAPSED_STRING

%token T_INCLUDE
%token T_INCLUDE_ONCE
%token T_EVAL
%token T_REQUIRE
%token T_REQUIRE_ONCE
%token T_LOGICAL_OR
%token T_LOGICAL_XOR
%token T_LOGICAL_AND
%token T_PRINT
%token T_YIELD
%token T_YIELD_FROM
%token T_PLUS_EQUAL
%token T_MINUS_EQUAL
%token T_MUL_EQUAL
%token T_DIV_EQUAL
%token T_CONCAT_EQUAL
%token T_MOD_EQUAL
%token T_AND_EQUAL
%token T_OR_EQUAL
%token T_XOR_EQUAL
%token T_SL_EQUAL
%token T_SR_EQUAL
%token T_BOOLEAN_OR
%token T_BOOLEAN_AND
%token T_IS_EQUAL
%token T_IS_NOT_EQUAL
%token T_IS_IDENTICAL
%token T_IS_NOT_IDENTICAL
%token T_IS_SMALLER_OR_EQUAL
%token T_IS_GREATER_OR_EQUAL
%token T_SPACESHIP
%token T_SL
%token T_SR
%token T_INSTANCEOF
%token T_INC
%token T_DEC
%token T_INT_CAST
%token T_DOUBLE_CAST
%token T_STRING_CAST
%token T_ARRAY_CAST
%token T_OBJECT_CAST
%token T_BOOL_CAST
%token T_UNSET_CAST
%token T_NEW
%token T_CLONE
%token T_EXIT
%token T_IF
%token T_ELSEIF
%token T_ELSE
%token T_ENDIF
%token T_ECHO
%token T_DO
%token T_WHILE
%token T_ENDWHILE
%token T_FOR
%token T_ENDFOR
%token T_FOREACH
%token T_ENDFOREACH
%token T_DECLARE
%token T_ENDDECLARE
%token T_AS
%token T_SWITCH
%token T_ENDSWITCH
%token T_CASE
%token T_DEFAULT
%token T_BREAK
%token T_CONTINUE
%token T_GOTO
%token T_FUNCTION
%token T_CONST
%token T_RETURN
%token T_TRY
%token T_CATCH
%token T_FINALLY
%token T_THROW
%token T_USE
%token T_INSTEADOF
%token T_GLOBAL
%token T_STATIC
%token T_ABSTRACT
%token T_FINAL
%token T_PRIVATE
%token T_PROTECTED
%token T_PUBLIC
%token T_VAR
%token T_UNSET
%token T_ISSET
%token T_EMPTY
%token T_HALT_COMPILER
%token T_CLASS
%token T_TRAIT
%token T_INTERFACE
%token T_EXTENDS
%token T_IMPLEMENTS
%token T_OBJECT_OPERATOR
%token T_DOUBLE_ARROW
%token T_LIST
%token T_ARRAY
%token T_CALLABLE
%token T_LINE
%token T_FILE
%token T_DIR
%token T_CLASS_C
%token T_TRAIT_C
%token T_METHOD_C
%token T_FUNC_C
%token T_DOLLAR_OPEN_CURLY_BRACES
%token T_CURLY_OPEN
%token T_PAAMAYIM_NEKUDOTAYIM
%token T_NAMESPACE
%token T_NS_C
%token T_NS_SEPARATOR
%token T_ELLIPSIS
%token T_COALESCE
%token T_POW
%token T_POW_EQUAL

// Not supported by lrpar
// %left T_NOELSE
// %token T_NOELSE (used to resolve dangling else problem)
// %token T_INLINE_HTML (inlined HTML)
// %token T_ENCAPSED_AND_WHITESPACE (currently lexed into T_CONSTANT_ENCAPSED_STRING)
// %token T_STRING_VARNAME (strings with variables inside)
// %token T_NUM_STRING (string containing numbers?)
// %token T_START_HEREDOC (multiline strings)
// %token T_END_HEREDOC

// Not needed by the grammar
// %token T_COMMENT
// %token T_OPEN_TAG
// %token T_OPEN_TAG_WITH_ECHO
// %token T_DOC_COMMENT
// %token T_CLOSE_TAG

%% /* Rules */

start:
	top_statement_list
;

reserved_non_modifiers:
	  T_INCLUDE | T_INCLUDE_ONCE | T_EVAL | T_REQUIRE | T_REQUIRE_ONCE | T_LOGICAL_OR | T_LOGICAL_XOR | T_LOGICAL_AND
	| T_INSTANCEOF | T_NEW | T_CLONE | T_EXIT | T_IF | T_ELSEIF | T_ELSE | T_ENDIF | T_ECHO | T_DO | T_WHILE | T_ENDWHILE
	| T_FOR | T_ENDFOR | T_FOREACH | T_ENDFOREACH | T_DECLARE | T_ENDDECLARE | T_AS | T_TRY | T_CATCH | T_FINALLY
	| T_THROW | T_USE | T_INSTEADOF | T_GLOBAL | T_VAR | T_UNSET | T_ISSET | T_EMPTY | T_CONTINUE | T_GOTO
	| T_FUNCTION | T_CONST | T_RETURN | T_PRINT | T_YIELD | T_LIST | T_SWITCH | T_ENDSWITCH | T_CASE | T_DEFAULT | T_BREAK
	| T_ARRAY | T_CALLABLE | T_EXTENDS | T_IMPLEMENTS | T_NAMESPACE | T_TRAIT | T_INTERFACE | T_CLASS
	| T_CLASS_C | T_TRAIT_C | T_FUNC_C | T_METHOD_C | T_LINE | T_FILE | T_DIR | T_NS_C
;

semi_reserved:
	  reserved_non_modifiers
	| T_STATIC | T_ABSTRACT | T_FINAL | T_PRIVATE | T_PROTECTED | T_PUBLIC
;

identifier:
		T_STRING
	| 	semi_reserved
;

top_statement_list:
		top_statement_list top_statement
	|	/* empty */
;

namespace_name:
		T_STRING
	|	namespace_name T_NS_SEPARATOR T_STRING
;

name:
		namespace_name
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name
	|	T_NS_SEPARATOR namespace_name
;

top_statement:
		statement
	|	function_declaration_statement
	|	class_declaration_statement
	|	trait_declaration_statement
	|	interface_declaration_statement
	|	T_HALT_COMPILER '(' ')' ';'

	|	T_NAMESPACE namespace_name ';'

	|	T_NAMESPACE namespace_name
		'{' top_statement_list '}'

	|	T_NAMESPACE
		'{' top_statement_list '}'

	|	T_USE mixed_group_use_declaration ';'
	|	T_USE use_type group_use_declaration ';'
	|	T_USE use_declarations ';'
	|	T_USE use_type use_declarations ';'
	|	T_CONST const_list ';'
;

use_type:
	 	T_FUNCTION
	| 	T_CONST
;

group_use_declaration:
		namespace_name T_NS_SEPARATOR '{' unprefixed_use_declarations possible_comma '}'

	|	T_NS_SEPARATOR namespace_name T_NS_SEPARATOR '{' unprefixed_use_declarations possible_comma '}'

;

mixed_group_use_declaration:
		namespace_name T_NS_SEPARATOR '{' inline_use_declarations possible_comma '}'

	|	T_NS_SEPARATOR namespace_name T_NS_SEPARATOR '{' inline_use_declarations possible_comma '}'

;

possible_comma:
		/* empty */
	|	','
;

inline_use_declarations:
		inline_use_declarations ',' inline_use_declaration

	|	inline_use_declaration

;

unprefixed_use_declarations:
		unprefixed_use_declarations ',' unprefixed_use_declaration

	|	unprefixed_use_declaration

;

use_declarations:
		use_declarations ',' use_declaration

	|	use_declaration

;

inline_use_declaration:
		unprefixed_use_declaration
	|	use_type unprefixed_use_declaration
;

unprefixed_use_declaration:
		namespace_name

	|	namespace_name T_AS T_STRING

;

use_declaration:
		unprefixed_use_declaration
	|	T_NS_SEPARATOR unprefixed_use_declaration
;

const_list:
		const_list ',' const_decl
	|	const_decl
;

inner_statement_list:
		inner_statement_list inner_statement

	|	/* empty */

;


inner_statement:
		statement
	|	function_declaration_statement
	|	class_declaration_statement
	|	trait_declaration_statement
	|	interface_declaration_statement
	|	T_HALT_COMPILER '(' ')' ';'

;


statement:
		'{' inner_statement_list '}'
	|	if_stmt
	|	alt_if_stmt
	|	T_WHILE '(' expr ')' while_statement

	|	T_DO statement T_WHILE '(' expr ')' ';'

	|	T_FOR '(' for_exprs ';' for_exprs ';' for_exprs ')' for_statement

	|	T_SWITCH '(' expr ')' switch_case_list

	|	T_BREAK optional_expr ';'
	|	T_CONTINUE optional_expr ';'
	|	T_RETURN optional_expr ';'
	|	T_GLOBAL global_var_list ';'
	|	T_STATIC static_var_list ';'
	|	T_ECHO echo_expr_list ';'
//	|	T_INLINE_HTML (not supported yet)
	|	expr ';'
	|	T_UNSET '(' unset_variables possible_comma ')' ';'
	|	T_FOREACH '(' expr T_AS foreach_variable ')' foreach_statement

	|	T_FOREACH '(' expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ')'
		foreach_statement

	|	T_DECLARE '(' const_list ')'
		declare_statement
	|	';'	/* empty statement */
	|	T_TRY '{' inner_statement_list '}' catch_list finally_statement

	|	T_THROW expr ';'
	|	T_GOTO T_STRING ';'
	|	T_STRING ':'
;

catch_list:
		/* empty */

	|	catch_list T_CATCH '(' catch_name_list T_VARIABLE ')' '{' inner_statement_list '}'

;

catch_name_list:
		name
	|	catch_name_list '|' name
;

finally_statement:
		/* empty */
	|	T_FINALLY '{' inner_statement_list '}'
;

unset_variables:
		unset_variable
	|	unset_variables ',' unset_variable
;

unset_variable:
		variable
;

function_declaration_statement:
	function returns_ref T_STRING backup_doc_comment '(' parameter_list ')' return_type
	backup_fn_flags '{' inner_statement_list '}' backup_fn_flags

;

is_reference:
		/* empty */
	|	'&'
;

is_variadic:
		/* empty */
	|	T_ELLIPSIS
;

class_declaration_statement:
		class_modifiers T_CLASS
		T_STRING extends_from implements_list backup_doc_comment '{' class_statement_list '}'

	|	T_CLASS
		T_STRING extends_from implements_list backup_doc_comment '{' class_statement_list '}'

;

class_modifiers:
		class_modifier
	|	class_modifiers class_modifier
;

class_modifier:
		T_ABSTRACT
	|	T_FINAL
;

trait_declaration_statement:
		T_TRAIT
		T_STRING backup_doc_comment '{' class_statement_list '}'

;

interface_declaration_statement:
		T_INTERFACE
		T_STRING interface_extends_list backup_doc_comment '{' class_statement_list '}'

;

extends_from:
		/* empty */
	|	T_EXTENDS name
;

interface_extends_list:
		/* empty */
	|	T_EXTENDS name_list
;

implements_list:
		/* empty */
	|	T_IMPLEMENTS name_list
;

foreach_variable:
		variable
	|	'&' variable
	|	T_LIST '(' array_pair_list ')'
	|	'[' array_pair_list ']'
;

for_statement:
		statement
	|	':' inner_statement_list T_ENDFOR ';'
;

foreach_statement:
		statement
	|	':' inner_statement_list T_ENDFOREACH ';'
;

declare_statement:
		statement
	|	':' inner_statement_list T_ENDDECLARE ';'
;

switch_case_list:
		'{' case_list '}'
	|	'{' ';' case_list '}'
	|	':' case_list T_ENDSWITCH ';'
	|	':' ';' case_list T_ENDSWITCH ';'
;

case_list:
		/* empty */
	|	case_list T_CASE expr case_separator inner_statement_list

	|	case_list T_DEFAULT case_separator inner_statement_list

;

case_separator:
		':'
	|	';'
;


while_statement:
		statement
	|	':' inner_statement_list T_ENDWHILE ';'
;


if_stmt_without_else:
		T_IF '(' expr ')' statement

	|	if_stmt_without_else T_ELSEIF '(' expr ')' statement

;

if_stmt:
		if_stmt_without_else // %prec T_NOELSE
	|	if_stmt_without_else T_ELSE statement

;

alt_if_stmt_without_else:
		T_IF '(' expr ')' ':' inner_statement_list

	|	alt_if_stmt_without_else T_ELSEIF '(' expr ')' ':' inner_statement_list

;

alt_if_stmt:
		alt_if_stmt_without_else T_ENDIF ';'
	|	alt_if_stmt_without_else T_ELSE ':' inner_statement_list T_ENDIF ';'

;

parameter_list:
		non_empty_parameter_list
	|	/* empty */
;


non_empty_parameter_list:
		parameter

	|	non_empty_parameter_list ',' parameter

;

parameter:
		optional_type is_reference is_variadic T_VARIABLE

	|	optional_type is_reference is_variadic T_VARIABLE '=' expr

;


optional_type:
		/* empty */
	|	type_expr
;

type_expr:
		type
	|	'?' type
;

type:
		T_ARRAY
	|	T_CALLABLE
	|	name
;

return_type:
		/* empty */
	|	':' type_expr
;

argument_list:
		'(' ')'
	|	'(' non_empty_argument_list possible_comma ')'
;

non_empty_argument_list:
		argument

	|	non_empty_argument_list ',' argument

;

argument:
		expr
	|	T_ELLIPSIS expr
;

global_var_list:
		global_var_list ',' global_var
	|	global_var
;

global_var:
	simple_variable

;


static_var_list:
		static_var_list ',' static_var
	|	static_var
;

static_var:
		T_VARIABLE
	|	T_VARIABLE '=' expr
;


class_statement_list:
		class_statement_list class_statement

	|	/* empty */

;


class_statement:
		variable_modifiers property_list ';'

	|	method_modifiers T_CONST class_const_list ';'

	|	T_USE name_list trait_adaptations

	|	method_modifiers function returns_ref identifier backup_doc_comment '(' parameter_list ')'
		return_type backup_fn_flags method_body backup_fn_flags

;

name_list:
		name
	|	name_list ',' name
;

trait_adaptations:
		';'
	|	'{' '}'
	|	'{' trait_adaptation_list '}'
;

trait_adaptation_list:
		trait_adaptation

	|	trait_adaptation_list trait_adaptation

;

trait_adaptation:
		trait_precedence ';'
	|	trait_alias ';'
;

trait_precedence:
	absolute_trait_method_reference T_INSTEADOF name_list

;

trait_alias:
		trait_method_reference T_AS T_STRING

	|	trait_method_reference T_AS reserved_non_modifiers
	|	trait_method_reference T_AS member_modifier identifier

	|	trait_method_reference T_AS member_modifier

;

trait_method_reference:
		identifier

	|	absolute_trait_method_reference
;

absolute_trait_method_reference:
	name T_PAAMAYIM_NEKUDOTAYIM identifier

;

method_body:
		';' /* abstract method */
	|	'{' inner_statement_list '}'
;

variable_modifiers:
		non_empty_member_modifiers
	|	T_VAR
;

method_modifiers:
		/* empty */
	|	non_empty_member_modifiers
;

non_empty_member_modifiers:
		member_modifier
	|	non_empty_member_modifiers member_modifier
;

member_modifier:
		T_PUBLIC
	|	T_PROTECTED
	|	T_PRIVATE
	|	T_STATIC
	|	T_ABSTRACT
	|	T_FINAL
;

property_list:
		property_list ',' property
	|	property
;

property:
		T_VARIABLE backup_doc_comment

	|	T_VARIABLE '=' expr backup_doc_comment

;

class_const_list:
		class_const_list ',' class_const_decl
	|	class_const_decl
;

class_const_decl:
	identifier '=' expr backup_doc_comment
;

const_decl:
	T_STRING '=' expr backup_doc_comment
;

echo_expr_list:
		echo_expr_list ',' echo_expr
	|	echo_expr
;
echo_expr:
	expr
;

for_exprs:
		/* empty */
	|	non_empty_for_exprs
;

non_empty_for_exprs:
		non_empty_for_exprs ',' expr
	|	expr
;

anonymous_class:
        T_CLASS ctor_arguments
		extends_from implements_list backup_doc_comment '{' class_statement_list '}';

new_expr:
		T_NEW class_name_reference ctor_arguments

	|	T_NEW anonymous_class

;

expr:
		variable

	|	T_LIST '(' array_pair_list ')' '=' expr
	|	'[' array_pair_list ']' '=' expr
	|	variable '=' expr

	|	variable '=' '&' variable

	|	T_CLONE expr
	|	variable T_PLUS_EQUAL expr

	|	variable T_MINUS_EQUAL expr

	|	variable T_MUL_EQUAL expr

	|	variable T_POW_EQUAL expr

	|	variable T_DIV_EQUAL expr

	|	variable T_CONCAT_EQUAL expr

	|	variable T_MOD_EQUAL expr

	|	variable T_AND_EQUAL expr

	|	variable T_OR_EQUAL expr

	|	variable T_XOR_EQUAL expr

	|	variable T_SL_EQUAL expr

	|	variable T_SR_EQUAL expr

	|	variable T_INC
	|	T_INC variable
	|	variable T_DEC
	|	T_DEC variable
	|	expr T_BOOLEAN_OR expr

	|	expr T_BOOLEAN_AND expr

	|	expr T_LOGICAL_OR expr

	|	expr T_LOGICAL_AND expr

	|	expr T_LOGICAL_XOR expr

	|	expr '|' expr
	|	expr '&' expr
	|	expr '^' expr
	|	expr '.' expr
	|	expr '+' expr
	|	expr '-' expr
	|	expr '*' expr
	|	expr T_POW expr
	|	expr '/' expr
	|	expr '%' expr
	| 	expr T_SL expr
	|	expr T_SR expr
	|	'+' expr %prec T_INC
	|	'-' expr %prec T_INC
	|	'!' expr
	|	'~' expr
	|	expr T_IS_IDENTICAL expr

	|	expr T_IS_NOT_IDENTICAL expr

	|	expr T_IS_EQUAL expr

	|	expr T_IS_NOT_EQUAL expr

	|	expr '<' expr

	|	expr T_IS_SMALLER_OR_EQUAL expr

	|	expr '>' expr

	|	expr T_IS_GREATER_OR_EQUAL expr

	|	expr T_SPACESHIP expr

	|	expr T_INSTANCEOF class_name_reference

	|	'(' expr ')'
	|	new_expr
	|	expr '?' expr ':' expr

	|	expr '?' ':' expr

	|	expr T_COALESCE expr

	|	internal_functions_in_yacc
	|	T_INT_CAST expr
	|	T_DOUBLE_CAST expr
	|	T_STRING_CAST expr
	|	T_ARRAY_CAST expr
	|	T_OBJECT_CAST expr
	|	T_BOOL_CAST expr
	|	T_UNSET_CAST expr
	|	T_EXIT exit_expr
	|	'@' expr
	|	scalar
	|	'`' backticks_expr '`'
	|	T_PRINT expr
	|	T_YIELD
	|	T_YIELD expr
	|	T_YIELD expr T_DOUBLE_ARROW expr
	|	T_YIELD_FROM expr
	|	function returns_ref backup_doc_comment '(' parameter_list ')' lexical_vars return_type
		backup_fn_flags '{' inner_statement_list '}' backup_fn_flags
	|	T_STATIC function returns_ref backup_doc_comment '(' parameter_list ')' lexical_vars
		return_type backup_fn_flags '{' inner_statement_list '}' backup_fn_flags
;

function:
	T_FUNCTION
;

backup_doc_comment:
	/* empty */
;

backup_fn_flags:
	/* empty */
;

returns_ref:
		/* empty */
	|	'&'
;

lexical_vars:
		/* empty */
	|	T_USE '(' lexical_var_list ')'
;

lexical_var_list:
		lexical_var_list ',' lexical_var
	|	lexical_var
;

lexical_var:
		T_VARIABLE
	|	'&' T_VARIABLE
;

function_call:
		name argument_list

	|	class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list

	|	variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list

	|	callable_expr argument_list

;

class_name:
		T_STATIC
	|	name
;

class_name_reference:
		class_name
	|	new_variable
;

exit_expr:
		/* empty */
	|	'(' optional_expr ')'
;

backticks_expr:
		/* empty */

//	|	T_ENCAPSED_AND_WHITESPACE
	|	encaps_list
;


ctor_arguments:
		/* empty */
	|	argument_list
;


dereferencable_scalar:
		T_ARRAY '(' array_pair_list ')'
	|	'[' array_pair_list ']'
	|	T_CONSTANT_ENCAPSED_STRING
;

scalar:
		T_LNUMBER
	|	T_DNUMBER
	|	T_LINE
	|	T_FILE
	|	T_DIR
	|	T_TRAIT_C
	|	T_METHOD_C
	|	T_FUNC_C
	|	T_NS_C
	|	T_CLASS_C
//	|	T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC
//	|	T_START_HEREDOC T_END_HEREDOC

	|	'"' encaps_list '"'
//	|	T_START_HEREDOC encaps_list T_END_HEREDOC
	|	dereferencable_scalar
	|	constant
;

constant:
		name
	|	class_name T_PAAMAYIM_NEKUDOTAYIM identifier

	|	variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier

;

optional_expr:
		/* empty */
	|	expr
;

variable_class_name:
	dereferencable
;

dereferencable:
		variable
	|	'(' expr ')'
	|	dereferencable_scalar
;

callable_expr:
		callable_variable
	|	'(' expr ')'
	|	dereferencable_scalar
;

callable_variable:
		simple_variable

	|	dereferencable '[' optional_expr ']'

	|	constant '[' optional_expr ']'

	|	dereferencable '{' expr '}'

	|	dereferencable T_OBJECT_OPERATOR property_name argument_list

	|	function_call
;

variable:
		callable_variable

	|	static_member

	|	dereferencable T_OBJECT_OPERATOR property_name

;

simple_variable:
		T_VARIABLE
	|	'$' '{' expr '}'
	|	'$' simple_variable
;

static_member:
		class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable

	|	variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable

;

new_variable:
		simple_variable

	|	new_variable '[' optional_expr ']'

	|	new_variable '{' expr '}'

	|	new_variable T_OBJECT_OPERATOR property_name

	|	class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable

	|	new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable

;

member_name:
		identifier
	|	'{' expr '}'
	|	simple_variable
;

property_name:
		T_STRING
	|	'{' expr '}'
	|	simple_variable
;

array_pair_list:
		non_empty_array_pair_list
;

possible_array_pair:
		/* empty */
	|	array_pair
;

non_empty_array_pair_list:
		non_empty_array_pair_list ',' possible_array_pair

	|	possible_array_pair

;

array_pair:
		expr T_DOUBLE_ARROW expr

	|	expr

	|	expr T_DOUBLE_ARROW '&' variable

	|	'&' variable

	|	expr T_DOUBLE_ARROW T_LIST '(' array_pair_list ')'
	|	T_LIST '(' array_pair_list ')'
;

encaps_list:
		encaps_list encaps_var

//	|	encaps_list T_ENCAPSED_AND_WHITESPACE

	|	encaps_var

//	|	T_ENCAPSED_AND_WHITESPACE encaps_var

;

encaps_var:
		T_VARIABLE

	|	T_VARIABLE '[' encaps_var_offset ']'

	|	T_VARIABLE T_OBJECT_OPERATOR T_STRING

	|	T_DOLLAR_OPEN_CURLY_BRACES expr '}'

//	|	T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME '}'
//	|	T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME '[' expr ']' '}'

	|	T_CURLY_OPEN variable '}'
;

encaps_var_offset:
		T_STRING
//	|	T_NUM_STRING
//	|	'-' T_NUM_STRING
	|	T_VARIABLE
;


internal_functions_in_yacc:
		T_ISSET '(' isset_variables possible_comma ')'
	|	T_EMPTY '(' expr ')'
	|	T_INCLUDE expr

	|	T_INCLUDE_ONCE expr

	|	T_EVAL '(' expr ')'

	|	T_REQUIRE expr

	|	T_REQUIRE_ONCE expr

;

isset_variables:
		isset_variable
	|	isset_variables ',' isset_variable

;

isset_variable:
		expr
;
