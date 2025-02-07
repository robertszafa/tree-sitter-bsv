/**
 * @file A tree-sitter grammar for Bluespec System Verilog (BSV)
 * @author Robert Szafarczyk <robertszafa@gmail.com>
 * @license MIT
 */

/// This grammar implements the BSV language as described in:
/// - Bluespec System Verilog language Reference Guide: 
///     https://github.com/B-Lang-org/bsc/releases/latest/download/BSV_lang_ref_guide.pdf

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name : "bsv",

  rules : {
    sourceFile : $ => choice(
                    prec.left(seq(
                      'package', $.packageIde, ';', repeat($.exportDecl),
                        repeat($.importDecl),
                        repeat($.packageStmt),
                      'endpackage', optional(seq(':', $.packageIde))
                    )),
                    prec.left(seq(
                      repeat($.exportDecl),
                      repeat($.importDecl),
                      repeat($.packageStmt),
                    )),
                   ),

    packageIde : $ => $.identifier, // BSV lang ref guide requires upper case; the
                                     // actual bsc implementation does not care...

    exportDecl : $ => prec.left(seq(
      'export', $.exportItem, repeat(seq(',', $.exportItem)), ';'
    )),

    exportItem : $ => choice(prec.left(seq($.identifier, optional(seq('(', '..', ')')))),
                             prec.left(seq($.Identifier, optional(seq('(', '..', ')')))),
                             prec.left(seq($.packageIde, '::', '*'))),

    importDecl : $ => prec.left(
      seq('import', $.importItem, repeat(seq(',', $.importItem)), ';')
    ),

    importItem : $ => prec.left(seq($.packageIde, '::', '*')),

    packageStmt : $ => choice(
      $.interfaceDecl, 
      $.typeDef, 
      $.typeclassDef,
      $.typeclassInstanceDef,
      // $.externModuleImport, // TODO: Add this, sec. 15 from BSV lang ref
      $.externCImport, 
      $.varDecl, 
      $.functionDef,
      $.moduleDef
    ),

    // ================================================================
    // Statement: Interface Declarations

    interfaceDecl : $ => prec.left(seq(
      optional($.attributeInstance), 
      'interface', $.typeDefType, ';', 
        repeat($.interfaceMemberDecl),
      'endinterface', optional(seq(':', $.typeIde))
    )),

    interfaceMemberDecl : $ => choice($.methodProto, $.subinterfaceDecl),

    methodProto : $ => prec.left(seq(
      optional($.attributeInstances), 
      'method', $.type, $.identifier,
        optional(seq('(', $.methodProtoFormals, ')')), 
      ';')
    ),

    methodProtoFormals : $ => prec.left(seq(
      $.methodProtoFormal, repeat(seq(',', $.methodProtoFormal))
    )),
    methodProtoFormal : $ => prec.left(seq(
      optional($.attributeInstances), $.type, $.identifier,
    )),

    subinterfaceDecl : $ => prec.left(
      seq(optional($.attributeInstances), 'interface', $.type, $.identifier, ';'),
    ),

    expressionStmt : $ => choice(
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.moduleDef,
      $.beginEndStmt,
      $.If,
      $.Case,
      $.For,
      $.While,
    ),

    // ================================================================
    // Typedefs

    typeDef : $ => choice(
      $.typedefSynonym, 
      $.typedefEnum, 
      $.typedefStruct,
      $.typedefTaggedUnion
    ),

    typeDefType : $ => prec.left(seq(
      $.typeIde, optional($.typeFormals)
    )),
    typeFormals : $ => prec.left(seq(
      '#', '(', $.typeFormal, repeat(seq(',', $.typeFormal)), ')'
    )),
    typeFormal : $ => prec.left(seq(
      optional('numeric'), 'type', $.typeIde
    )),

    // Synonym typedefs
    typedefSynonym : $ => prec.left(seq(
      'typedef', $.type, $.typeDefType, ';'
    )),

    // Enum typedefs
    typedefEnum : $ => prec.left(seq(
      'typedef', 'enum', repeat($.typedefEnumElements), $.Identifier, optional($.derives), ';'
    )),
    typedefEnumElements : $ => prec.left(seq(
      $.typedefEnumElement, repeat(seq(',', $.typedefEnumElement))
    )),
    typedefEnumElement : $ => choice(
                           prec.left(seq($.Identifier, optional(seq('=', $.intLiteral)))),
                           prec.left(seq($.Identifier, '[', $.intLiteral, ']',
                                     optional(seq('=', $.intLiteral)))),
                           prec.left(seq($.Identifier, '[', $.intLiteral, ':', $.intLiteral, ']',
                                     optional(seq('=', $.intLiteral))))
    ),

    // Struct and taggedunion typedefs
    typedefStruct : $ => prec.left(seq(
      'typedef', 'struct', '{', repeat($.structMember), '}', $.typeDefType, optional($.derives), ';'
    )),

    typedefTaggedUnion : $ => prec.left(seq(
      'typedef', 'union', 'tagged', '{',
        repeat($.unionMember), 
      '}', $.typeDefType, optional($.derives), ';'
    )),

    structMember : $ => choice(prec.left(seq($.type, $.identifier, ';')),
                               prec.left(seq($.subUnion, $.identifier, ';'))),

    unionMember : $ => choice(prec.left(seq($.type, $.Identifier, ';')),
                              prec.left(seq($.subStruct, $.Identifier, ';')),
                              prec.left(seq($.subUnion, $.Identifier, ';')),
                              prec.left(seq('void', $.Identifier, ';'))),

    subStruct : $ => prec.left(seq(
      'struct', '{', repeat($.structMember), '}'
    )),
    subUnion : $ => prec.left(seq(
      'union', 'tagged', '{', repeat($.unionMember), '}'
    )),

    derives : $ => prec.left(seq(
      'deriving', '(', $.typeclassIde, repeat(seq(',', $.typeclassIde)), ')'
    )),

    // ================================================================
    // Variable declarations and initialization

    varDecl : $ => choice(
      prec.right(seq($.type, $.varInit, repeat(seq(',', $.varInit)), ';')),
      prec.left(seq($.type, $.identifier, '<-', $.expression, ';')),
      prec.left(seq('let', $.identifier, '=', $.expression, ';')),
      prec.left(seq('let', $.identifier, '<-', $.expression, ';')),
      prec.left(seq('match', $.pattern, '=', $.expression, ';'))
    ),

    varInit : $ => prec.left(seq(
      $.identifier, optional($.arrayDims), optional(seq('=', $.expression))
    )),
    arrayDims : $ => prec.left(seq(
      '[', $.expression, ']', repeat(seq('[', $.expression, ']'))
    )),

    // ================================================================
    // Typeclass definitions

    typeclassDef : $ => prec.left(seq(
      'typeclass', $.typeclassIde, $.typeFormals, optional($.provisos), 
        optional($.typedepends), ';', repeat($.overloadedDef), 
      'endtypeclass', optional(seq(':', $.typeclassIde))
    )),
    typeclassIde : $ => $.Identifier,

    typeFormals : $ => prec.left(seq(
      '#', '(', $.typeFormal, repeat(seq(',', $.typeFormal)), ')'
    )),
    typeFormal : $ => prec.left(seq(
      optional('numeric'), 'type', $.typeIde
    )),
    typedepends : $ => prec.left(seq(
      'dependencies', '(', $.typedepend, repeat(seq(',', $.typedepend)), ')'
    )),
    typedepend : $ => prec.left(seq(
      $.typelist, 'determines', $.typelist
    )),
    typelist : $ => choice(
      $.typeIde, 
      prec.left(seq('(', $.typeIde, repeat(seq(',', $.typeIde)), ')'))
    ),
    overloadedDef : $ => choice($.functionProto, $.moduleProto, $.varDecl),
    //
    // ================================================================
    // Typeclass instance definitions

    typeclassInstanceDef : $ => prec.left(seq(
      'instance', $.typeclassIde, '#', '(', $.type, repeat(seq(',', $.type)), ')',
      optional($.provisos), ';',
        repeat(choice(prec.left(seq($.varAssign, ';')),
                      $.functionDef, 
                      $.moduleDef)),
      'endinstance', optional(seq(':', $.typeclassIde))
    )),

    // ================================================================
    // Module Declarations

    moduleDef : $ => prec.left(seq(
      optional($.attributeInstances), 
      $.moduleProto,
        repeat($.moduleStmt), 
      'endmodule', optional(seq(':', $.identifier))
    )),

    moduleProto : $ => prec.left(seq(
      'module', optional(seq('[', $.type, ']')), $.identifier, 
      optional($.moduleFormalParams), '(', optional($.moduleFormalArgs), ')',
      optional($.provisos), ';'
    )),

    moduleFormalParams : $ => prec.left(seq(
      '#', '(', $.moduleFormalParam, repeat(seq(',', $.moduleFormalParam)), ')'
    )),

    moduleFormalParam : $ => prec.left(seq(
      optional($.attributeInstances), optional('parameter'), $.type, $.identifier
    )),

    moduleFormalArgs : $ => choice(
      prec.left(seq(optional($.attributeInstances), $.type)),
      prec.left(seq(optional($.attributeInstances), $.type, $.identifier,
                repeat(seq(',', optional($.attributeInstances), $.type, $.identifier))))
    ),

    moduleStmt : $ => choice(
      $.moduleInst, 
      $.methodDef, 
      $.subinterfaceDef,
      $.rule, 
      $.Stmt
    ),

    // ----------------
    // 5.4.1: Short form instantiation

    moduleInst : $ => prec.left(seq(
      optional($.attributeInstances),
      $.type, $.identifier, '<-', $.moduleApp, ';'
    )),

    moduleApp : $ => prec.left(seq(
      $.identifier, prec.left(optional(seq('(', $.moduleActualParamArg,
                               repeat(seq(',', $.moduleActualParamArg)), ')')))
    )),

    moduleActualParamArg : $ => choice(
      $.expression,
      prec.left(seq('clocked_by', $.expression)),
      prec.left(seq('reset_by', $.expression))
    ),

    // ----------------
    // 5.4.2: Long form instantiation
    // Obsolete (still exists in some very old BSV code).

    moduleInst2 : $ => prec.left(seq(
      optional($.attributeInstances), 
      $.type, $.identifier, '(', ')', ';', $.moduleApp2, $.identifier,
      optional(seq('(', $.moduleActualArgs, ')')), ';'
    )),
    moduleApp2 : $ => prec.left(seq(
      $.identifier, optional(seq('#', '(', $.moduleActualParam,
                              repeat(seq(',', $.moduleActualParam)), ')'))
    )),
    moduleActualParam : $ => $.expression,
    moduleActualArgs : $ => prec.left(seq(
      $.moduleActualArg, repeat(seq(',', $.moduleActualArg))
    )),
    moduleActualArg : $ => choice(
      $.expression, 
      prec.left(seq('clocked_by', $.expression)), 
      prec.left(seq('reset_by', $.expression))
    ),

    // ----------------
    // 5.5 Interface definition (definition of methods)

    methodDef : $ => prec.left(seq(
      'method', optional($.type), $.identifier,
        optional(seq('(', optional($.methodFormals), ')')),
        optional($.implicitCond), ';', 
        $.functionBody,
      'endmethod', optional(seq(':', $.identifier))
    )),
    methodFormals : $ => prec.left(seq($.methodFormal, repeat(seq(',', $.methodFormal)))),
    methodFormal : $ => prec.left(seq(optional($.type), $.identifier)),
    implicitCond : $ => prec.left(seq('if', '(', $.condPredicate, ')')),

    // 5.5.2 Definition of subinterfaces

    subinterfaceDef : $ => prec.left(seq(
      'interface', $.Identifier, $.identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optional(seq(':', $.identifier))
    )),
    interfaceStmt : $ => choice($.methodDef, $.subinterfaceDef),

    // 5.5.3 Definition of methods and subinterfaces by assignment

    methodDef : $ => prec.left(seq(
      'method', optional($.type), $.identifier, '(', $.methodFormals, ')', 
      optional($.implicitCond), '=', $.expression, ';'
    )),
    subinterfaceDef : $ => prec.left(seq(
      'interface', optional($.type), $.identifier, '=', $.expression, ';'
    )),

    // ----------------
    // 5.6 Rules in module definitions

    rule : $ => prec.left(seq(
      optional($.attributeInstances), 
      'rule', $.identifier, optional($.ruleCond), ';', 
        repeat($.Stmt), 
      'endrule', optional(seq(':', $.identifier))
    )),
    ruleCond : $ => prec.left(seq('(', $.condPredicate, ')')),

    // ================================================================
    // Function definition

    functionDef : $ => choice(
      prec.left(seq(
        optional($.attributeInstances), 
        $.functionProto,
          repeat($.functionBody), 
        'endfunction', optional(seq(':', $.identifier))
      )),
      prec.left(seq(
        $.functionProto, '=', $.expression, ';')
      )
    ),

    functionBody: $ => choice(
      $.actionBlock,
      $.actionValueBlock,
      prec.left(repeat1($.functionBodyStmt)),
    ),

    functionBodyStmt: $ => choice(
      $.returnStmt, 
      $.varDecl, 
      $.varAssign, 
      $.functionDef,
      $.moduleDef, 
      $.beginEndBlock, 
      $.If, 
      $.Case, 
      $.For, 
      $.While
    ),

    returnStmt: $ => seq('return', $.expression, ';'),

    functionProto : $ => prec.left(seq(
      'function', $.type, $.identifier,
      optional(seq('(', $.functionFormals, ')')),
      optional($.provisos), ';'
    )),
    functionFormals : $ => prec.left(seq(
      $.functionFormal, repeat(seq(',', $.functionFormal))
    )),
    functionFormal : $ => prec.left(seq($.type, $.identifier)),

    // ================================================================
    // Importing C functions

    externCImport : $ => prec.left(seq(
      'import', '"BDPI"',
      optional(seq($.identifier, '=')), 'function', $.type, 
      $.identifier, '(', optional($.CFuncArgs), ')', optional($.provisos), ';'
    )),
    CFuncArgs : $ => prec.left(seq(
      $.CFuncArg, repeat(seq(',', $.CFuncArg))
    )),
    CFuncArg : $ => prec.left(seq(
      $.type, optional($.identifier)
    )),

    // ================================================================
    // Variable assignment (for variables already declared)
    varAssign : $ => choice(
      prec.left(seq($.lValue, '=', $.expression, ';')),
      prec.left(seq($.identifier, '<-', $.expression, ';'))
    ),
    lValue : $ => choice(
      $.identifier, 
      prec.left(seq($.lValue, '.', $.identifier)),
      prec.left(seq($.lValue, '[', $.expression, ']')),
      prec.left(seq($.lValue, '[', $.expression, ':', $.expression, ']'))
    ),

    // ================================================================
    // Types

    type : $ => choice(
      $.typePrimary, 
      prec.left(seq(
        $.typePrimary, '(', $.type, repeat(prec.left(seq(',', $.type))), ')'
      ))
    ),

    typePrimary : $ => choice(
      prec.left(seq($.typeIde, optional(seq('#', '(', $.type, repeat(seq(',', $.type)), ')')))),
      $.typeNat,
      prec.left(seq('bit', optional(seq($.typeNat, ':', $.typeNat))))
    ),

    // To quote from BSV reference manual: Data types in BSV are case sensitive. 
    // The first character of a type is almost always uppercase, the
    // only exceptions being the types int and bit for compatibility with Verilog.
    typeIde : $ => $.identifier,
    typeNat : $ => $.decDigits,

    // ================================================================
    // Expressions

    expression : $ => choice(
      $.condExpr, 
      $.operatorExpr, 
      $.exprPrimary
    ),

    condExpr : $ => prec.right(seq(
      $.condPredicate, '?', $.expression, ':', $.expression
    )),

    operatorExpr : $ => choice(
      prec.right(seq($.unop, $.expression)),
      prec.left(seq($.expression, $.binop, $.expression))
    ),

    // The following operators are in order of decreasing precedence
    unop : $ => token(choice(
      prec(90, '+'),
      prec(90, '-'),
      prec(90, '!'),
      prec(90, '~'),
      prec(89, '&'),
      prec(88, '~&'),
      prec(87, '|'),
      prec(86, '~|'),
      prec(85, '^'),
      prec(84, '^~'),
      prec(84, '~^'),
    )),
    binop : $ => token(choice(
      prec(83, '*'),
      prec(83, '/'),
      prec(83, '%'),
      prec(82, '+'),
      prec(82, '-'),
      prec(81, '<<'),
      prec(81, '>>'),
      prec(80, '<='),
      prec(80, '>='),
      prec(80, '<'),
      prec(80, '>'),
      prec(79, '=='),
      prec(79, '!='),
      prec(78, '&'),
      prec(77, '^'),
      prec(76, '^~'),
      prec(76, '~^'),
      prec(75, '|'),
      prec(74, '&&'),
      prec(73, '||'),
    )),

    exprPrimary : $ => choice(
      prec.left(seq('(', $.expression, ')')),
      prec.left(seq($.exprPrimary, '.', $.identifier)),
      $.identifier,
      $.intLiteral,
      $.realLiteral,
      $.stringLiteral,
      '?',
      seq('valueOf', '(', $.type, ')'),
      seq('valueof', '(', $.type, ')'),
      $.bitConcat,
      $.bitSelect,
      $.functionCall,
      $.methodCall,
      $.typeAssertion,
      $.structExpr,
      $.taggedUnionExpr,
      $.interfaceExpr,
      $.rulesExpr,
      $.beginEndBlock,
      $.actionBlock,
      $.actionValueBlock,
      $.seqFsmStmt,
      $.parFsmStmt,
    ),

    bitConcat : $ => seq(
      '{', $.expression, repeat(seq(',', $.expression)), '}'
    ),
    bitSelect : $ => prec.left(seq(
      $.exprPrimary, '[', $.expression, optional(seq(':', $.expression)), ']'
    )),

    // functionCall ::= exprPrimary [ '(' [ expression { ',' expression } ] ')' ]
    //  NOTE: We make parens madatory.
    functionCall : $ => prec.left(seq(
      $.exprPrimary, 
      '(',
        optional(seq($.expression, repeat(seq(',', $.expression)))),
      ')'
    )),

    // methodCall ::= exprPrimary '.' identifier [ '(' [ expression { ',' expression } ] ')' ]
    //  NOTE: We make parens madatory.
    methodCall : $ => prec.left(seq(
      $.exprPrimary, '.', $.identifier,
      '(',
        optional(seq($.expression, repeat(seq(',', $.expression)))),
      ')',
    )),

    typeAssertion : $ => choice(
      prec.left(seq($.type, '’', $.bitConcat)),
      prec.left(seq($.type, '’', '(', $.expression, ')'))
    ),

    structExpr : $ => prec.left(seq(
      $.Identifier, '{', 
        $.memberBind,
        repeat(seq(',', $.memberBind)), 
      '}'
    )),
    taggedUnionExpr : $ => choice(
      prec.left(seq('tagged', $.Identifier, '{', $.memberBind,
                repeat(seq(',', $.memberBind)), '}')),
      prec.left(seq('tagged', $.Identifier, $.exprPrimary))
    ),
    memberBind : $ => prec.left(seq($.identifier, ':', $.expression)),

    interfaceExpr : $ => prec.left(seq(
      'interface', $.Identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optional(seq(':', $.Identifier))
    )),
    interfaceStmt : $ => choice($.methodDef, $.subinterfaceDef, $.expressionStmt),

    rulesExpr : $ => prec.left(seq(
      optional($.attributeInstances), 
      'rules', optional(seq(':', $.identifier)), 
        $.rulesStmt,
      'endrules', optional(seq(':', $.identifier))
      )),
    rulesStmt : $ => choice($.rule, $.expressionStmt),

    // ----------------
    // Blocks: begin-end, action-endaction, actionvalue-endactionvalue
    // and bodies of functions, methods and rules

    beginEndBlock : $ => prec.left(seq(
      'begin', optional(seq(':', $.identifier)),
        repeat($.Stmt), 
        $.expression, 
      'end', optional(seq(':', $.identifier))
    )),

    beginEndStmt : $ => prec.left(seq(
      'begin', optional(seq(':', $.identifier)),
        repeat($.Stmt), 
      'end', optional(seq(':', $.identifier))
    )),

    actionBlock : $ => prec.left(seq(
      'action', optional(seq(':', $.identifier)),
        repeat($.Stmt), 'endaction',
      optional(prec.left(seq(':', $.identifier)))
    )),

    actionValueBlock : $ => prec.left(seq(
      'actionvalue', optional(seq(':', $.identifier)),
        repeat($.Stmt), 'endactionvalue',
      optional(seq(':', $.identifier))
    )),

    regWrite : $ => prec.left(seq($.expression, '<=', $.expression)),

    // ----------------
    // General statements

    Stmt : $ => choice(
      $.expression, 
      $.varDecl, 
      $.varAssign, 
      $.functionDef,
      $.moduleDef, 
      $.beginEndBlock, 
      $.If, 
      $.Case, 
      $.For, 
      $.While
    ),

    

    // ----------------
    // Conditionals

    If : $ => prec.left(seq(
      'if', '(', $.condPredicate, ')', $.Stmt,
      optional(seq('else', $.Stmt))
    )),

    Case : $ => choice(
      prec.left(seq(
        'case', '(', $.expression, ')',
          repeat($.CaseItem),
          optional($.DefaultItem),
        'endcase',
      )),
      prec.left(seq(
        'case', '(', $.expression, ')', 'matches',
          repeat($.CasePatItem), 
          optional($.DefaultItem),
        'endcase'
      ))
    ),
    CaseItem : $ => prec.left(seq(
      $.expression, repeat(seq(',', $.expression)), ':', $.Stmt
    )),
    CasePatItem : $ => prec.left(seq(
      $.pattern, repeat(seq('&&&', $.expression)), ':', $.Stmt
    )),
    DefaultItem : $ => prec.left(seq('default', optional(':'), $.Stmt)),

    // ----------------
    // Static loops

    While : $ => prec.left(seq('while', '(', $.expression, ')', $.Stmt)),
    For : $ => prec.left(seq(
      'for', '(', $.forInit, ';', $.forTest, ';', $.forIncr, ')',
        $.Stmt
    )),
    forInit : $ => choice($.forOldInit, $.forNewInit),

    forOldInit : $ => prec.left(seq(
      $.simpleVarAssign, repeat(seq(',', $.simpleVarAssign))
    )),
    simpleVarAssign : $ => prec.left(seq($.identifier, '=', $.expression)),

    forNewInit : $ => prec.left(seq(
      $.type, $.identifier, '=', $.expression,
      repeat(seq(',', $.simpleVarDeclAssign))
    )),
    simpleVarDeclAssign : $ => prec.left(seq(
      optional($.type), $.identifier, '=', $.expression),
    ),

    forTest : $ => $.expression,
    forIncr : $ => prec.left(seq(
      $.varIncr, repeat(seq(',', $.varIncr))
    )),
    varIncr : $ => prec.left(seq(
      $.identifier, '=', $.expression
    )),

    // ================================================================
    // Cond predicates occur in method def implicit conditions, rule
    // conditions, if expressions, pattern-matching case expressions, ...

    condPredicate : $ => prec.left(
      seq($.exprOrCondPattern, repeat(prec.left(seq('&&&', $.exprOrCondPattern))))
    ),
    exprOrCondPattern : $ => choice($.expression,
                                    prec.left(seq($.expression, 'matches', $.pattern))),

    // ================================================================
    // Patterns occur in condPredicates

    pattern : $ => choice(
                prec.left(seq('.', $.identifier)),
                '.*',
                $.constantPattern,
                $.taggedUnionPattern,
                $.structPattern,
                $.tuplePattern,
    ),

    constantPattern : $ => choice(
      $.intLiteral, 
      $.realLiteral, 
      $.stringLiteral,
      $.Identifier
    ),

    taggedUnionPattern : $ => prec.left(seq(
      'tagged', $.Identifier, optional($.pattern)
    )),
    structPattern : $ => prec.left(seq(
      'tagged', $.Identifier, '{', $.identifier, ':', $.pattern,
      repeat(seq(',', $.identifier, ':', $.pattern)), '}'
    )),
    tuplePattern : $ => prec.left(seq(
      '{', $.pattern, repeat(seq(',', $.pattern)), '}'
    )),

    // ================================================================
    // Attributes

    attributeInstances : $ => prec.left(seq(
      $.attributeInstance, repeat($.attributeInstance)
    )),
    attributeInstance : $ => prec.left(seq(
      '(*', $.attrSpec, repeat(seq(',', $.attrSpec)), '*)'
    )),
    attrSpec : $ => prec.left(seq(
      $.attrName, optional(seq('=', $.expression))
    )),
    attrName : $ => choice($.identifier, $.Identifier),

    // ================================================================
    // Provisos

    provisos : $ => prec.left(seq(
      'provisos', '(', $.proviso, repeat(seq(',', $.proviso)), ')'
    )),
    proviso : $ => prec.left(seq(
      $.Identifier, '#', '(', $.type, repeat(seq(',', $.type)), ')'
    )),

    // ================================================================
    // StmtFSM

    fsmStmt : $ => choice(
      $.exprFsmStmt,
      $.seqFsmStmt,
      $.parFsmStmt,
      $.ifFsmStmt,
      $.whileFsmStmt,
      $.repeatFsmStmt,
      $.forFsmStmt,
      $.returnFsmStmt,
    ),

    exprFsmStmt : $ => choice(
      prec.left(seq($.regWrite, ';')), 
      prec.left(seq($.expression, ';'))
    ),

    seqFsmStmt : $ => prec.left(seq(
      'seq', 
        $.fsmStmt, 
        repeat($.fsmStmt), 
      'endseq'
    )),

    parFsmStmt : $ => prec.left(seq(
      'par', 
        $.fsmStmt, 
        repeat($.fsmStmt), 
      'endpar'
    )),

    ifFsmStmt : $ => prec.left(seq(
      'if', $.expression, $.fsmStmt,
      optional(seq('else', $.fsmStmt))
    )),

    returnFsmStmt : $ => prec.left(seq('return', ';')),

    whileFsmStmt : $ => prec.left(seq(
      'while', '(', $.expression, ')', $.loopBodyFsmStmt
    )),

    forFsmStmt : $ => prec.left(seq(
      'for', '(', $.fsmStmt, ';', $.expression, ';', $.fsmStmt, ')', 
        $.loopBodyFsmStmt
    )),

    repeatFsmStmt : $ => prec.left(seq(
      'repeat', '(', $.expression, ')', $.loopBodyFsmStmt
    )),

    loopBodyFsmStmt : $ => choice(
      $.fsmStmt, 
      prec.left(seq('break', ';')),
      prec.left(seq('continue', ';'))
    ),

    // ==================================================================
    // Integer, real literals

    intLiteral : $ => choice(
      "'0'", 
      "'1", 
      $.sizedIntLiteral, 
      $.unsizedIntLiteral
    ),
    sizedIntLiteral : $ => prec.left(seq($.bitWidth, $.baseLiteral)),
    unsizedIntLiteral : $ => choice(
      prec.left(seq(optional($.sign), $.baseLiteral)),
      prec.left(seq(optional($.sign), $.decNum))
    ),
    baseLiteral : $ => choice(
      prec.left(seq(choice("'d", "'D"), $.decDigitsUnderscore)),
      prec.left(seq(choice("'h", "'H"), $.hexDigitsUnderscore)),
      prec.left(seq(choice("'o", "'O"), $.octDigitsUnderscore)),
      prec.left(seq(choice("'b", "'B"), $.binDigitsUnderscore)),
    ),

    realLiteral : $ => choice(
      prec.left(seq($.decNum, optional(seq('.', $.decDigitsUnderscore)),
                    $.exp, optional($.sign), $.decDigitsUnderscore)),
      prec.left(seq($.decNum, '.', $.decDigitsUnderscore))
    ),

    decNum : $ => prec.left(seq($.decDigits, optional($.decDigitsUnderscore))),
    bitWidth : $ => $.decDigits,
    sign : $ => choice('+', '-'),
    exp : $ => choice('e', 'E'),
    decDigits : $ => /[0-9]+/,
    decDigitsUnderscore : $ => /[0-9_]+/,
    hexDigitsUnderscore : $ => /[0-9a-fA-F_]+/,
    octDigitsUnderscore : $ => /[0-7_]+/,
    binDigitsUnderscore : $ => /[0-1_]+/,

    stringLiteral : $ => /"([^"\\]|\\[\s\S])*"/,

    displayTaskName : $ => choice(
                        '$display', '$displayb', '$displayo',
                        '$displayh', '$write', '$writeb', '$writeo', '$writeh',
                        '$format', '$fopen', '$fclose', '$fdisplay',
                        '$fdisplayb', '$fdisplayo', '$fdisplayh', '$fwrite',
                        '$fwriteb', '$fwriteo', '$fwriteh', '$swrite',
                        '$swriteb', '$swriteo', '$swriteh', '$sformat',
                        '$swriteAV', '$swritebAV', '$swriteoAV', '$swritehAV',
                        '$sformatAV', '$fgetc', '$fungetc', '$fflush',
                        '$finish', '$stop', '$dumpvars', '$dumpon', '$dumpoff',
                        '$time', '$stime', '$realtobits', '$bitstoreal',
                        '$test$plusargs'),

    identifier : $ => token(/[a-zA-Z_][a-zA-Z0-9_]*/), // Identifier starting with any case.
    Identifier : $ => token(/[A-Z_][a-zA-Z0-9_]*/), // Identifier starting with upper case.

    comment : $ => token(seq('//', /.*/)),

  },

  extras : $ => [/\s/, $.comment],

  // tree-sitter has automatic keyword extraction
  word: $ => $.identifier,

  conflicts: $ => [
    [$.lValue, $.exprPrimary],
    [$.typeNat, $.decNum],
    [$.bitWidth, $.decNum],
    [$.Stmt, $.exprOrCondPattern],
    [$.exprPrimary, $.Stmt],
    [$.exprPrimary, $.fsmStmt],
    [$.operatorExpr, $.exprOrCondPattern],
    [$.exprOrCondPattern, $.ifFsmStmt],
    [$.condPredicate, $.condPredicate],
    [$.moduleApp, $.exprPrimary],
    [$.condExpr, $.exprOrCondPattern],
    [$.expression, $.exprPrimary, $.methodCall],
    [$.expression, $.functionCall],
    [$.CasePatItem, $.exprOrCondPattern],
    [$.typeIde, $.exprPrimary],
    [$.typeIde, $.exprPrimary, $.moduleApp],
    [$.typeIde, $.methodDef],
  ]
});
