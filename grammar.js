/**
 * @file A tree-sitter grammar for Bluespec System Verilog (BSV)
 * @author Robert Szafarczyk <robertszafa@gmail.com>
 * @license MIT
 */

/// This grammar implements the BSV language as described in:
/// - Bluespec System Verilog language Reference Guide: 
///     https://github.com/B-Lang-org/bsc/releases/latest/download/BSV_lang_ref_guide.pdf
/// - Simpliefied BSV grammar collected in single txt file by Rishiyur S. Nikhil:
///     https://github.com/rsnikhil/goParseBSV/blob/master/grammar.txt
///
/// Any mistakes are my own.

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

    packageIde : $ =>
                   $._Identifier, // BSV lang ref guide requires upper case; the
                                 // actual bsc implementation does not care...

    exportDecl : $ => prec.left(seq(
      'export', $.exportItem, repeat(seq(',', $.exportItem)), ';'
    )),

    exportItem : $ => choice(prec.left(seq($._identifier, optional(seq('(', '..', ')')))),
                             prec.left(seq($._Identifier, optional(seq('(', '..', ')')))),
                             prec.left(seq($.packageIde, '::', '*'))),

    importDecl : $ => prec.left(
      seq('import', $.importItem, repeat(seq(',', $.importItem)), ';')
    ),

    importItem : $ => prec.left(seq($.packageIde, '::', '*')),

    packageStmt : $ => choice($.interfaceDecl, $.typeDef, $.typeclassDef,
                              $.typeclassInstanceDef,
                              // $.externModuleImport, // TODO: Add this, sec. 15 from BSV lang ref
                              $.externCImport, $.varDecl, $.functionDef,
                              $.moduleDef),

    // ================================================================
    // Statement: Interface Declarations

    interfaceDecl : $ => prec.left(seq(
      optional($.attributeInstance), 
      'interface',
        $.typeDefType, ';', $.interfaceMemberDecl,
      'endinterface', optional(seq(':', $._typeIde))
    )),

    interfaceMemberDecl : $ => choice($.methodProto, $.subinterfaceDecl),

    methodProto : $ => prec.left(seq(
      optional($.attributeInstances), 'method', $.type, $._identifier,
      optional(seq('(', $.methodProtoFormals, ')')), ';')
    ),

    methodProtoFormals : $ => prec.left(
      seq($.methodProtoFormal, repeat(seq(',', $.methodProtoFormal)))
    ),
    methodProtoFormal : $ => prec.left(
      seq(optional($.attributeInstances), $.type, $._identifier),
    ),

    subinterfaceDecl : $ => prec.left(
      seq(optional($.attributeInstances), 'interface', $.type, $._identifier, ';'),
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

    typeDef : $ => choice($.typedefSynonym, $.typedefEnum, $.typedefStruct,
                          $.typedefTaggedUnion),

    typeDefType : $ => seq($._typeIde, optional($.typeFormals)),
    typeFormals : $ => seq('#', '(', $.typeFormal,
                           repeat(seq(',', $.typeFormal)), ')'),
    typeFormal : $ => seq(optional('numeric'), 'type', $._typeIde),

    // Synonym typedefs
    typedefSynonym : $ => seq('typedef', $.type, $.typeDefType, ';'),

    // Enum typedefs
    typedefEnum : $ => seq('typedef', 'enum', repeat($.typedefEnumElements),
                           $._Identifier, optional($.derives), ';'),
    typedefEnumElements : $ => seq($.typedefEnumElement,
                                   repeat(seq(',', $.typedefEnumElement))),
    typedefEnumElement : $ => choice(
                           seq($._Identifier, optional(seq('=', $.intLiteral))),
                           seq($._Identifier, '[', $.intLiteral, ']',
                               optional(seq('=', $.intLiteral))),
                           seq($._Identifier, '[', $.intLiteral, ':',
                               $.intLiteral, ']',
                               optional(seq('=', $.intLiteral)))),

    // Struct and taggedunion typedefs
    typedefStruct : $ => seq('typedef', 'struct', '{', repeat($.structMember),
                             '}', $.typeDefType, optional($.derives), ';'),

    typedefTaggedUnion : $ => seq('typedef', 'union', 'tagged', '{',
                                  repeat($.unionMember), '}', $.typeDefType,
                                  optional($.derives), ';'),

    structMember : $ => choice(seq($.type, $._identifier, ';'),
                               seq($.subUnion, $._identifier, ';')),

    unionMember : $ => choice(seq($.type, $._Identifier, ';'),
                              seq($.subStruct, $._Identifier, ';'),
                              seq($.subUnion, $._Identifier, ';'),
                              seq('void', $._Identifier, ';')),

    subStruct : $ => seq('struct', '{', repeat($.structMember), '}'),
    subUnion : $ => seq('union', 'tagged', '{', repeat($.unionMember), '}'),

    derives : $ => seq('deriving', '(', $.typeclassIde,
                       repeat(seq(',', $.typeclassIde)), ')'),

    // ================================================================
    // Variable declarations and initialization

    varDecl : $ => choice(
      prec.left(seq($.type, $.varInit, repeat(seq(',', $.varInit)), ';')),
      prec.left(seq($.type, $._identifier, '<-', $.expression, ';')),
      prec.left(seq('let', $._identifier, '=', $.expression, ';')),
      prec.left(seq('let', $._identifier, '<-', $.expression, ';')),
      prec.left(seq('match', $.pattern, '=', $.expression, ';'))
    ),

    varInit : $ => seq($._identifier, optional($.arrayDims),
                       optional(seq('=', $.expression))),
    arrayDims : $ => seq('[', $.expression, ']',
                         repeat(seq('[', $.expression, ']'))),

    // ================================================================
    // Typeclass definitions

    typeclassDef : $ => prec.left(seq(
      'typeclass', $.typeclassIde, $.typeFormals, optional($.provisos), 
        optional($.typedepends), ';', repeat($.overloadedDef), 
      'endtypeclass', optional(seq(':', $.typeclassIde))
    )),
    typeclassIde : $ => $._Identifier,

    typeFormals : $ => prec.left(seq(
      '#', '(', $.typeFormal, repeat(seq(',', $.typeFormal)), ')'
    )),
    typeFormal : $ => seq(optional('numeric'), 'type', $._typeIde),
    typedepends : $ => seq('dependencies', '(', $.typedepend,
                           repeat(seq(',', $.typedepend)), ')'),
    typedepend : $ => seq($.typelist, 'determines', $.typelist),
    typelist : $ => choice($._typeIde, seq('(', $._typeIde,
                                          repeat(seq(',', $._typeIde)), ')')),
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

    moduleDef : $ => seq(optional($.attributeInstances), $.moduleProto,
                         repeat($.moduleStmt), 'endmodule',
                         optional(seq(':', $._identifier))),

    moduleProto : $ => seq('module', optional(seq('[', $.type, ']')), $._identifier, 
                           optional($.moduleFormalParams), '(', optional($.moduleFormalArgs), ')',
                           optional($.provisos), ';'),

    moduleFormalParams : $ => seq('#', '(', $.moduleFormalParam,
                                  repeat(seq(',', $.moduleFormalParam)), ')'),

    moduleFormalParam : $ => seq(optional($.attributeInstances),
                                 optional('parameter'), $.type, $._identifier),

    moduleFormalArgs : $ => choice(
                         seq(optional($.attributeInstances), $.type),
                         seq(optional($.attributeInstances), $.type,
                             $._identifier,
                             repeat(seq(',', optional($.attributeInstances),
                                        $.type, $._identifier)))),

    moduleStmt : $ => choice($.moduleInst, $.methodDef, $.subinterfaceDef,
                             $.rule, $.Stmt),

    // ----------------
    // 5.4.1: Short form instantiation

    moduleInst : $ => seq(
      optional($.attributeInstances),
      $.type, $._identifier, '<-', $.moduleApp, ';'
    ),

    moduleApp : $ => prec.left(seq(
                  $._identifier,
                  prec.left(optional(seq('(', $.moduleActualParamArg,
                            repeat(seq(',', $.moduleActualParamArg)), ')')))
    )),

    moduleActualParamArg : $ => choice($.expression,
                                       seq('clocked_by', $.expression),
                                       seq('reset_by', $.expression)),

    // ----------------
    // 5.4.2: Long form instantiation
    // Obsolete (still exists in some very old BSV code).

    moduleInst2 : $ => seq(optional($.attributeInstances), $.type, $._identifier,
                           '(', ')', ';', $.moduleApp2, $._identifier,
                           optional(seq('(', $.moduleActualArgs, ')')), ';'),
    moduleApp2 : $ => seq(
                   $._identifier,
                   optional(seq('#', '(', $.moduleActualParam,
                                repeat(seq(',', $.moduleActualParam)), ')'))),
    moduleActualParam : $ => $.expression,
    moduleActualArgs : $ => seq($.moduleActualArg,
                                repeat(seq(',', $.moduleActualArg))),
    moduleActualArg : $ => choice($.expression, seq('clocked_by', $.expression),
                                  seq('reset_by', $.expression)),

    // ----------------
    // 5.5 Interface definition (definition of methods)

    methodDef : $ => prec.left(seq(
      'method', optional($.type), $._identifier,
        optional(seq('(', optional($.methodFormals), ')')),
        optional($.implicitCond), ';', 
        $.functionBody,
      'endmethod', optional(seq(':', $._identifier))
    )),
    methodFormals : $ => prec.left(seq($.methodFormal, repeat(seq(',', $.methodFormal)))),
    methodFormal : $ => prec.left(seq(optional($.type), $._identifier)),
    implicitCond : $ => prec.left(seq('if', '(', $.condPredicate, ')')),

    // 5.5.2 Definition of subinterfaces

    subinterfaceDef : $ => prec.left(seq(
      'interface', $._Identifier, $._identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optional(seq(':', $._identifier))
    )),
    interfaceStmt : $ => choice($.methodDef, $.subinterfaceDef),

    // 5.5.3 Definition of methods and subinterfaces by assignment

    methodDef : $ => prec.left(seq(
      'method', optional($.type), $._identifier, '(', $.methodFormals, ')', 
      optional($.implicitCond), '=', $.expression, ';'
    )),
    subinterfaceDef : $ => prec.left(seq(
      'interface', optional($.type), $._identifier, '=', $.expression, ';'
    )),

    // ----------------
    // 5.6 Rules in module definitions

    rule : $ => prec.left(seq(
      optional($.attributeInstances), 
      'rule', $._identifier, optional($.ruleCond), ';', 
        repeat($.Stmt), 
      'endrule', optional(seq(':', $._identifier))
    )),
    ruleCond : $ => prec.left(seq('(', $.condPredicate, ')')),

    // ================================================================
    // Function definition

    functionDef : $ =>
                    choice(seq(optional($.attributeInstances), $.functionProto,
                               repeat($.Stmt), 'endfunction',
                               optional(seq(':', $._identifier))),
                           seq($.functionProto, '=', $.expression, ';')),

    functionProto : $ => seq('function', $.type, $._identifier,
                             optional(seq('(', $.functionFormals, ')')),
                             optional($.provisos), ';'),
    functionFormals : $ => seq($.functionFormal,
                               repeat(seq(',', $.functionFormal))),
    functionFormal : $ => seq($.type, $._identifier),

    // ================================================================
    // Importing C functions

    externCImport : $ => seq('import', '"BDPI"',
                             optional(seq(
                                 $._identifier,
                                 '=',
                                 )),
                             'function', $.type, $._identifier, '(',
                             optional($.CFuncArgs), ')', optional($.provisos),
                             ';'),
    CFuncArgs : $ => seq($.CFuncArg, repeat(seq(',', $.CFuncArg))),
    CFuncArg : $ => seq($.type, optional($._identifier)),

    // ================================================================
    // Variable assignment (for variables already declared)
    varAssign : $ => choice(seq($.lValue, '=', $.expression, ';'),
                            seq($._identifier, '<-', $.expression, ';')),
    lValue : $ =>
               choice($._identifier, seq($.lValue, '.', $._identifier),
                      seq($.lValue, '[', $.expression, ']'),
                      seq($.lValue, '[', $.expression, ':', $.expression, ']')),

    // ================================================================
    // Types

    type : $ => choice(
      $._typePrimary, 
      prec.left(seq(
        $._typePrimary, '(', $.type, repeat(prec.left(seq(',', $.type))), ')'
      ))
    ),

    _typePrimary : $ => choice(
      prec.left(seq($._typeIde, optional(seq('#', '(', $.type, repeat(seq(',', $.type)), ')')))),
      $.typeNat,
      prec.left(seq('bit', optional(seq($.typeNat, ':', $.typeNat))))
    ),

    _typeIde : $ => choice($._Identifier, 'SizeOf'),
    typeNat : $ => $.decDigits,

    // ================================================================
    // Expressions

    expression : $ => choice($.condExpr, $.operatorExpr, $.exprPrimary),

    condExpr : $ => prec.left(seq($.condPredicate, '?', $.expression, ':', $.expression)),

    operatorExpr : $ => choice(prec.left(seq($.unop, $.expression)),
                               prec.left(seq($.expression, $.binop, $.expression))),

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

    exprPrimary : $ => prec.left(choice(
                    prec.left(seq('(', $.expression, ')')),
                    prec.left(seq($.exprPrimary, '.', $._identifier)),
                    $._identifier,
                    $.intLiteral,
                    $.realLiteral,
                    $.stringLiteral,
                    '?',
                    prec.left(seq('valueof', '(', $.type, ')')),
                    prec.left(seq('valueOf', '(', $.type, ')')),
                    prec.left(seq('return', $.expression, ';')),
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
    )),

    bitConcat : $ => seq('{', $.expression, repeat(seq(',', $.expression)), '}'),
    bitSelect : $ => seq($.exprPrimary, '[', $.expression,
                         optional(seq(':', $.expression)), ']'),

    // functionCall           ::= exprPrimary [ '(' [ expression { ','
    // expression } ] ')' ]
    functionCall : $ => prec.left(seq(
      $.exprPrimary,
      optional(prec.left(seq(
          '(',
          optional(prec.left(seq($.expression,
                        prec.left(repeat(seq(',', $.expression)))))),
          ')')))
    )),

    // methodCall             ::= exprPrimary '.' identifier [ '(' [
    // expression { ',' expression } ] ')' ]
    methodCall : $ => seq($.exprPrimary, '.', $._identifier,
                          optional(
                              seq('(',
                                  optional(seq($.expression,
                                               repeat(seq(',', $.expression)))),
                                  ')'))),

    typeAssertion : $ => choice(seq($.type, '’', $.bitConcat),
                                seq($.type, '’', '(', $.expression, ')')),

    structExpr : $ => seq($._Identifier, '{', $.memberBind,
                          repeat(seq(',', $.memberBind)), '}'),
    taggedUnionExpr : $ => choice(seq('tagged', $._Identifier, '{', $.memberBind,
                                      repeat(seq(',', $.memberBind)), '}'),
                                  seq('tagged', $._Identifier, $.exprPrimary)),
    memberBind : $ => seq($._identifier, ':', $.expression),

    interfaceExpr : $ => prec.left(
      seq('interface', $._Identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optional(seq(':', $._Identifier))
    )),
    interfaceStmt : $ =>
                      choice($.methodDef, $.subinterfaceDef, $.expressionStmt),

    rulesExpr : $ => prec.left(seq(
      optional($.attributeInstances), 
      'rules', optional(seq(':', $._identifier)), 
        $.rulesStmt,
      'endrules', optional(seq(':', $._identifier))
      )),
    rulesStmt : $ => choice($.rule, $.expressionStmt),

    // ----------------
    // Blocks: begin-end, action-endaction, actionvalue-endactionvalue
    // and bodies of functions, methods and rules

    beginEndBlock : $ => prec.left(seq(
      'begin', optional(seq(':', $._identifier)),
        repeat($.Stmt), 
        $.expression, 
      'end', optional(seq(':', $._identifier))
    )),

    beginEndStmt : $ => prec.left(seq(
      'begin', optional(seq(':', $._identifier)),
        repeat($.Stmt), 
      'end', optional(seq(':', $._identifier))
    )),

    actionBlock : $ => prec.left(seq(
      'action', optional(seq(':', $._identifier)),
        repeat($.Stmt), 'endaction',
      optional(prec.left(seq(':', $._identifier)))
    )),

    actionValueBlock : $ => prec.left(seq(
      'actionvalue', optional(seq(':', $._identifier)),
        repeat($.Stmt), 'endactionvalue',
      optional(seq(':', $._identifier))
    )),

    regWrite : $ => seq($.expression, '<=', $.expression),

    // ----------------
    // General statements

    Stmt : $ => choice($.expression, $.varDecl, $.varAssign, $.functionDef,
                       $.moduleDef, $.beginEndBlock, $.If, $.Case, $.For, $.While),

    // ----------------
    // Conditionals

    If : $ => prec.left(seq(
      'if', '(', $.condPredicate, ')', $.Stmt,
      optional(seq('else', $.Stmt))
    )),

    Case : $ => choice(seq(
                           'case',
                           '(',
                           $.expression,
                           ')',
                           repeat($.CaseItem),
                           optional($.DefaultItem),
                           'endcase',
                           ),
                       seq('case', '(', $.expression, ')', 'matches',
                           repeat($.CasePatItem), optional($.DefaultItem),
                           'endcase')),
    CaseItem : $ =>
                 seq($.expression, repeat(seq(',', $.expression)), ':', $.Stmt),
    CasePatItem : $ => prec.left(seq(
      $.pattern, repeat(seq('&&&', $.expression)), ':', $.Stmt
    )),
    DefaultItem : $ => seq('default', optional(':'), $.Stmt),

    // ----------------
    // Static loops

    While : $ => seq('while', '(', $.expression, ')', $.Stmt),
    For : $ => seq('for', '(', $.forInit, ';', $.forTest, ';', $.forIncr, ')',
                   $.Stmt),
    forInit : $ => choice($.forOldInit, $.forNewInit),

    forOldInit : $ =>
                   seq($.simpleVarAssign, repeat(seq(',', $.simpleVarAssign))),
    simpleVarAssign : $ => seq($._identifier, '=', $.expression),

    forNewInit : $ => seq($.type, $._identifier, '=', $.expression,
                          repeat(seq(',', $.simpleVarDeclAssign))),
    simpleVarDeclAssign : $ => seq(optional($.type), $._identifier, '=',
                                   $.expression),

    forTest : $ => $.expression,
    forIncr : $ => seq($.varIncr, repeat(seq(',', $.varIncr))),
    varIncr : $ => seq($._identifier, '=', $.expression),

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
                seq('.', $._identifier),
                '.*',
                $.constantPattern,
                $.taggedUnionPattern,
                $.structPattern,
                $.tuplePattern,
    ),

    constantPattern : $ => choice($.intLiteral, $.realLiteral, $.stringLiteral,
                                  $._Identifier),

    taggedUnionPattern : $ => seq('tagged', $._Identifier, optional($.pattern)),
    structPattern : $ => seq(
                      'tagged', $._Identifier, '{', $._identifier, ':', $.pattern,
                      repeat(seq(',', $._identifier, ':', $.pattern)), '}'
    ),
    tuplePattern : $ => seq('{', $.pattern, repeat(seq(',', $.pattern)), '}'),

    // ================================================================
    // Attributes

    attributeInstances : $ => seq($.attributeInstance,
                                  repeat($.attributeInstance)),
    attributeInstance : $ => seq('(*', $.attrSpec, repeat(seq(',', $.attrSpec)),
                                 '*)'),
    attrSpec : $ => seq($.attrName, optional(seq('=', $.expression))),
    attrName : $ => choice($._identifier, $._Identifier),

    _identifier : $ => /[a-z][a-zA-Z1-9_$]*/, // Identifier starting with lower
                                             // case.
    _Identifier : $ => /[A-Z][a-zA-Z1-9_$]*/, // Identifier starting with upper
                                             // case.

    comment : $ => token(seq('//', /.*/)),

    // ================================================================
    // Provisos

    provisos : $ => seq('provisos', '(', $.proviso, repeat(seq(',', $.proviso)),
                        ')'),
    proviso : $ => seq($._Identifier, '#', '(', $.type, repeat(seq(',', $.type)),
                       ')'),

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

    exprFsmStmt : $ => choice(seq($.regWrite, ';'), seq($.expression, ';')),

    seqFsmStmt : $ => seq('seq', $.fsmStmt, repeat($.fsmStmt), 'endseq'),

    parFsmStmt : $ => seq('par', $.fsmStmt, repeat($.fsmStmt), 'endpar'),

    ifFsmStmt : $ => prec.left(seq(
      'if', $.expression, $.fsmStmt,
      optional(seq('else', $.fsmStmt))
    )),

    returnFsmStmt : $ => seq('return', ';'),

    whileFsmStmt : $ => seq('while', '(', $.expression, ')', $.loopBodyFsmStmt),

    forFsmStmt : $ => seq('for', '(', $.fsmStmt, ';', $.expression, ';',
                          $.fsmStmt, ')', $.loopBodyFsmStmt),

    repeatFsmStmt : $ =>
                      seq('repeat', '(', $.expression, ')', $.loopBodyFsmStmt),

    loopBodyFsmStmt : $ => choice($.fsmStmt, seq('break', ';'),
                                  seq('continue', ';')),

    // ==================================================================
    // Integer, real literals

    intLiteral : $ => choice("'0'", "'1", $.sizedIntLiteral, $.unsizedIntLiteral),
    sizedIntLiteral : $ => seq($.bitWidth, $.baseLiteral),
    unsizedIntLiteral : $ => choice(seq(optional($.sign), $.baseLiteral),
                                    seq(optional($.sign), $.decNum)),
    baseLiteral : $ => choice(
                    seq(choice("'d", "'D"), $.decDigitsUnderscore),
                    seq(choice("'h", "'H"), $.hexDigitsUnderscore),
                    seq(choice("'o", "'O"), $.octDigitsUnderscore),
                    seq(choice("'b", "'B"), $.binDigitsUnderscore),
    ),

    realLiteral : $ => choice(
                    seq($.decNum, optional(seq('.', $.decDigitsUnderscore)),
                        $.exp, optional($.sign), $.decDigitsUnderscore),
                    seq($.decNum, '.', $.decDigitsUnderscore)),

    decNum : $ => seq($.decDigits, optional($.decDigitsUnderscore)),
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

  },

  extras : $ => [/\s/, $.comment],

  word: $ => $._identifier,

  conflicts: $ => [
    [$.lValue, $.exprPrimary],
    [$.typeNat, $.decNum],
    [$.bitWidth, $.decNum],
    [$.Stmt, $.exprOrCondPattern],
    // [$.Stmt, $.operatorExpr],
    [$.expression, $.functionCall],
    // [$.bitSelect, $.functionCall],
    // [$.exprPrimary, $.functionCall, $.methodCall],
    [$.exprPrimary, $.Stmt],
    [$.exprPrimary, $.fsmStmt],
    [$.unsizedIntLiteral, $.realLiteral],
    [$.operatorExpr, $.exprOrCondPattern],
    [$.functionCall, $.taggedUnionExpr],
    [$.bitSelect, $.functionCall, $.taggedUnionExpr],
    [$.exprPrimary, $.functionCall, $.methodCall, $.taggedUnionExpr],
    [$.exprOrCondPattern, $.ifFsmStmt],
    [$.exprPrimary, $.methodCall],
    [$.condPredicate, $.condPredicate],
    [$.moduleApp, $.exprPrimary],
    [$.condExpr, $.exprOrCondPattern],
    // [$.condExpr, $.operatorExpr],
    [$.expression, $.exprPrimary, $.functionCall, $.methodCall],
    [$.CasePatItem, $.exprOrCondPattern],



  ]
});
