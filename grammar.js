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

// Some useful functions

function repeatseq(...rules) {
  return repeat(prec.left(seq(...rules)));
}

function optseq(...rules) {
  return optional(prec.left(seq(...rules)));
}

// Functions producing contextualized rules.

function ctxtBeginEndStmt($, ctxtStmt) {
  return field('beginEndStmt', prec.left(seq(
    'begin', optseq(':', $.identifier),
    repeat(ctxtStmt), 
    'end', optseq(':', $.identifier)
  )));
}

function ctxtIf($, ctxtStmt) {
  return field('ifStmt', prec.left(seq(
    'if', '(', $.condPredicate, ')', ctxtStmt,
    optional(seq('else', ctxtStmt))
  )));
}

function ctxtCase($, ctxtStmt) {
  let CaseItem = () => prec.left(seq(
      $.expression, repeat(seq(',', $.expression)), ':', ctxtStmt
  ));
  let CasePatItem = () => prec.left(seq(
      $.pattern, repeat(seq('&&&', $.expression)), ':', ctxtStmt
  ));
  let DefaultItem = () => prec.left(seq(
    'default', optional(':'), ctxtStmt
  ));

  return choice(
    prec.left(seq(
      'case', '(', $.expression, ')',
        repeat(CaseItem()),
        optional(DefaultItem()),
      'endcase',
    )),
    prec.left(seq(
      'case', '(', $.expression, ')', 'matches',
        repeat(CasePatItem()), 
        optional(DefaultItem()),
      'endcase'
    ))
  );
}

    // ----------------
    // Static loops
function ctxtFor($, ctxtStmt) {
    let simpleVarAssign = () => prec.left(seq($.identifier, '=', $.expression));
    let simpleVarDeclAssign = () => prec.left(seq(
      optional($.type), $.identifier, '=', $.expression),
    );

    let forOldInit = () => prec.left(seq(
      simpleVarAssign(), repeat(seq(',', simpleVarAssign()))
    ));
    let forNewInit = () => prec.left(seq(
      $.type, $.identifier, '=', $.expression,
      repeat(seq(',', simpleVarDeclAssign()))
    ));
    let forInit = () => choice(forOldInit(), forNewInit());

    let varIncr = () => prec.left(seq(
      $.identifier, '=', $.expression
    ));
    let forTest = () => $.expression;
    let forIncr = () => prec.left(seq(
      varIncr(), repeat(seq(',', varIncr()))
    ));

  return prec.left(seq(
    'for', '(', forInit(), ';', forTest(), ';', forIncr(), ')',
      ctxtStmt
  ));
}

function ctxtWhile($, ctxtStmt) {
  return prec.left(seq('while', '(', $.expression, ')', ctxtStmt));
}


module.exports = grammar({
  name : "bsv",

  rules : {

    // =============================================================
    // sec 3 Packages and the outermost structure of a BSV design

    sourceFile: $ => choice(
      seq(
        'package', $.packageIde, ';', 
          repeat($.exportDecl),
          repeat($.importDecl),
          repeat($.packageStmt),
        'endpackage', optseq(':', $.packageIde)
      ),
      // NOTE: The actual bsc implementation allows files 
      // without a package declaration.
      seq(
        repeat($.exportDecl),
        repeat($.importDecl),
        repeat($.packageStmt),
      ),
    ),

    // NOTE: Ignore compiler directives for now. Assume pre-processed source.
    //       The directives use C syntax, so any C pre-processor works on these.
    //       Alternatively, we could use preprocessor rules from tree-sitter-c
    // compilerDirective: $ => choice(
    //   seq("'include", '"', $.filename, '"'),
    //   seq("'include", '<', $.filename, '>'),
    //   seq("'include", $.macroInvocation),
    //   seq("'line", $.decDigits, '"', $.filename, '"', $.level),
    //   seq("'define", $.macroName, optseq('(', $.macroFormals, ')'), $.macroText),
    // ),
    // level: $ => choice('0', '1', '2'),

    exportDecl: $ => prec.left(seq(
      'export', $.exportItem, repeatseq(',', $.exportItem), ';'
    )),
    exportItem: $ => choice(
      seq($.identifier, optseq('(', '..', ')')),
      seq($.Identifier, optseq('(', '..', ')')),
      seq($.packageIde, '::', '*')
    ),

    importDecl: $ => seq('import', $.importItem, repeatseq(',', $.importItem), ';'),
    importItem: $ => seq($.packageIde, '::', '*'),

    packageStmt: $ => choice(
      $.moduleDef,
      $.interfaceDecl, 
      $.typeDef, 
      $.varDecl, 
      // NOTE: Not allowed at package scope:
      // $.varAssign, 
      $.functionDef,
      $.typeclassDef,
      $.typeclassInstanceDef,
      // TODO: Add externModuleImport, sec. 15 from BSV lang ref
      // $.externModuleImport, 
      // NOTE: missing in the spec:
      $.externCImport, 
    ),

    //  NOTE: BSV lang ref guide requires upper case; 
    //  the actual bsc implementation does not care...
    packageIde: $ => $.identifier, 

    // ================================================================
    // sec 4 Types

    type: $ => choice(
      $.typePrimary, 
      prec.left(seq(
        $.typePrimary, '(', $.type, repeatseq(',', $.type), ')'
      ))
    ),

    typePrimary: $ => choice(
      seq($.typeIde, optseq('#', '(', $.type, repeatseq(',', $.type), ')')),
      $.typeNat,
      seq('bit', optseq($.typeNat, ':', $.typeNat))
    ),

    // NOTE: To quote from BSV reference manual: Data types in BSV are case 
    // sensitive. The first character of a type is almost always uppercase, the
    // only exceptions being the types int and bit for compatibility with Verilog.
    typeIde: $ => $.identifier,
    typeNat: $ => $.decDigits,

    // ================================================================
    // sec 5.2 Interface declaration

    interfaceDecl: $ => seq(
      optional($.attributeInstance), 
      'interface', $.typeDefType, ';', 
        repeat($.interfaceMemberDecl),
      'endinterface', optseq(':', $.typeIde)
    ),

    typeDefType: $ => seq(
      $.typeIde, optional($.typeFormals)
    ),
    typeFormals: $ => seq(
      '#', '(', $.typeFormal, repeatseq(',', $.typeFormal), ')'
    ),
    typeFormal: $ => seq(
      optional(choice('numeric', 'string')), 'type', $.typeIde
    ),

    interfaceMemberDecl: $ => choice($.methodProto, $.subinterfaceDecl),

    methodProto: $ => seq(
      optional($.attributeInstances), 
      'method', $.type, $.identifier, '(', optional($.methodProtoFormals), ')', ';'
    ),

    methodProtoFormals: $ => seq(
      $.methodProtoFormal, repeatseq(',', $.methodProtoFormal)
    ),
    methodProtoFormal: $ => seq(
      optional($.attributeInstances), $.type, $.identifier,
    ),

    // NOTE: Spec seems to be missing "identifier".
    subinterfaceDecl: $ => seq(
      optional($.attributeInstances), 'interface', $.type, $.identifier, ';'
      // optional($.attributeInstances), 'interface', $.typeDefType, $.identifier, ';'
    ),

    // ================================================================
    // sec 5.3 Module definition

    moduleDef: $ => seq(
      optional($.attributeInstances), 
      $.moduleProto,
        repeat($.moduleStmt), 
      'endmodule', optseq(':', $.identifier)
    ),

    moduleProto: $ => seq(
      'module', optseq('[', $.type, ']'), $.identifier,
      optional($.moduleFormalParams), '(', optional($.moduleFormalArgs), ')',
      optional($.provisos), ';'
    ),
    moduleFormalParams: $ => seq(
      '#', '(', $.moduleFormalParam, repeatseq(',', $.moduleFormalParam), ')'
    ),
    moduleFormalParam: $ => prec.left(seq(
      optional($.attributeInstances), optional('parameter'), $.type, $.identifier
    )),
    moduleFormalArgs: $ => choice(
      seq(optional($.attributeInstances), $.type),
      seq(optional($.attributeInstances), $.type, $.identifier,
          repeatseq(',', optional($.attributeInstances), $.type, $.identifier))
    ),

    moduleStmt: $ => choice(
      $.moduleInst,
      $.methodDef,
      $.subinterfaceDef,
      $.rule, 
      $.varDo,
      $.varDeclDo,
      $.functionCall,
      $.systemTaskStmt,
      seq('(', $.expression, ')'),
      $.returnStmt,
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.moduleDef,
      ctxtBeginEndStmt($, $.moduleStmt),
      ctxtIf($, $.moduleStmt),
      ctxtCase($, $.moduleStmt),
      ctxtFor($, $.moduleStmt),
      ctxtWhile($, $.moduleStmt),
    ),


    // ============================================================
    // sec 5.4 Module and interface instantiation

    // sec 5.4.1 Short form instantiation
    moduleInst: $ => seq(
      optional($.attributeInstances),
      $.type, $.identifier, '<-', $.moduleApp, ';'
    ),
    moduleApp: $ => seq(
      $.identifier, '(', 
        optseq($.moduleActualParamArg, repeatseq(',', $.moduleActualParamArg)),
      ')'
    ),
    moduleActualParamArg: $ => choice(
      $.expression,
      seq('clocked_by', $.expression),
      seq('reset_by', $.expression)
    ),

    // sec 5.4.2 Long form instantiation (depractaed)
    moduleInstLongForm: $ => seq(
      optional($.attributeInstances), 
      $.type, $.identifier, '(', ')', ';', 
      $.moduleApp2, $.identifier, '(', optional($.moduleActualArgs), ')', ';'
    ),
    moduleApp2: $ => seq(
      $.identifier, optseq('#', 
        '(', $.moduleActualParam,repeatseq(',', $.moduleActualParam)), ')'
    ),
    moduleActualParam: $ => $.expression,
    moduleActualArgs: $ => seq(
      $.moduleActualParamArg, repeatseq(',', $.moduleActualParamArg)
    ),

    // ============================================================
    // sec 5.5 Interface definition (definition of methods)

    // NOTE: Parens in method definition should be optional.
    //       It should also be possible to have empty parens.
    //       The spec is wrong here.
    methodDef: $ => choice(
      seq(
        'method', optional($.type), $.identifier,
          optseq('(', optional($.methodFormals), ')'),
          optional($.implicitCond), ';', 
            $.functionBody,
        'endmethod', optseq(':', $.identifier)
      ),
      // Short form (sec 5.5.3)
      seq(
        'method', optional($.type), $.identifier, 
        optseq('(', optional($.methodFormals), ')'),
        optional($.implicitCond), '=', $.expression, ';'
      ),
      // Action method (sec. 5.5.1)
      seq(
        'method', 'Action', $.identifier,
          optseq('(', optional($.methodFormals), ')'),
          optional($.implicitCond), ';', 
            repeat($.actionStmt),
        'endmethod', optseq(':', $.identifier),
      ),
      // ActionValue method (sec. 5.5.1)
      seq(
        'method', 'ActionValue', $.typeFormals, $.identifier,
          optseq('(', optional($.methodFormals), ')'),
          optional($.implicitCond), ';', 
            repeat($.actionValueStmt),
        'endmethod', optseq(':', $.identifier),
      ),
    ),

    methodFormals: $ => seq($.methodFormal, repeatseq(',', $.methodFormal)),
    methodFormal: $ => seq(optional($.type), $.identifier),
    implicitCond: $ => prec.right(seq('if', '(', $.condPredicate, ')')),
    
    // 5.5.2 Definition of subinterfaces
    subinterfaceDef: $ => choice(
      seq(
        'interface', $.Identifier, $.identifier, ';',
          repeat($.interfaceStmt), 
        'endinterface', optseq(':', $.identifier)
      ),
      // Short form (sec 5.5.3)
      seq(
        'interface', optional($.type), $.identifier, '=', $.expression, ';'
      ),
    ),
    interfaceStmt: $ => choice(
      $.methodDef, 
      $.subinterfaceDef,
      $.expressionStmt
    ),

    expressionStmt: $ => choice(
      $.varDecl,
      $.functionDef,
      $.moduleDef,
      ctxtBeginEndStmt($, $.expressionStmt),
      ctxtIf($, $.expressionStmt),
      ctxtCase($, $.expressionStmt),
      ctxtFor($, $.expressionStmt),
      ctxtWhile($, $.expressionStmt),
    ),

    // ============================================================
    // sec 5.6 Rules in module definitions

    rule: $ => prec.left(seq(
      optional($.attributeInstances), 
      'rule', $.identifier, optional($.ruleCond), ';', 
        repeat($.actionStmt), // rule body
      'endrule', optional(seq(':', $.identifier))
    )),
    ruleCond: $ => prec.left(seq('(', $.condPredicate, ')')),
    condPredicate: $ => prec.left(seq(
      $.exprOrCondPattern, repeatseq('&&&', $.exprOrCondPattern)
    )),
    exprOrCondPattern: $ => choice(
      $.expression,
      seq($.expression, 'matches', $.pattern)
    ),

    // ================================================================
    // Typedefs

    typeDef: $ => choice(
      $.typedefSynonym, 
      $.typedefEnum, 
      $.typedefStruct,
      $.typedefTaggedUnion
    ),

    typedefSynonym: $ => prec.left(seq(
      'typedef', $.type, $.typeDefType, ';'
    )),

    typedefEnum: $ => seq(
      'typedef', 'enum', '{', $.typedefEnumElements, '}', $.Identifier, 
        optional($.derives), ';'
    ),
    typedefEnumElements: $ => seq(
      $.typedefEnumElement, repeatseq(',', $.typedefEnumElement)
    ),
    typedefEnumElement: $ => choice(
      prec.left(seq($.Identifier, optseq('=', $.intLiteral))),
      prec.left(seq($.Identifier, '[', $.intLiteral, ']',
                  optseq('=', $.intLiteral))),
      prec.left(seq($.Identifier, '[', $.intLiteral, ':', $.intLiteral, ']',
                  optseq('=', $.intLiteral)))
    ),

    // sec 7.3 Structs and tagged unions
    typedefStruct: $ => prec.left(seq(
      'typedef', 'struct', '{', 
        repeat($.structMember), 
      '}', $.typeDefType, optional($.derives), ';'
    )),
    typedefTaggedUnion: $ => prec.left(seq(
      'typedef', 'union', 'tagged', '{',
        repeat($.unionMember), 
      '}', $.typeDefType, optional($.derives), ';'
    )),
    structMember: $ => choice(
      prec.left(seq($.type, $.identifier, ';')),
      prec.left(seq($.subUnion, $.identifier, ';'))
    ),
    unionMember: $ => choice(
      prec.left(seq($.type, $.Identifier, ';')),
      prec.left(seq($.subStruct, $.Identifier, ';')),
      prec.left(seq($.subUnion, $.Identifier, ';')),
      prec.left(seq('void', $.Identifier, ';'))
    ),
    subStruct: $ => prec.left(seq(
      'struct', '{', 
        repeat($.structMember), 
      '}'
    )),
    subUnion: $ => prec.left(seq(
      'union', 'tagged', '{', 
        repeat($.unionMember), 
      '}'
    )),


    // ============================================================
    // sec 8 Type classes (overloading groups) and provisos
    
    provisos: $ => prec.left(seq(
      'provisos', '(', $.proviso, repeatseq(',', $.proviso), ')'
    )),
    proviso: $ => prec.left(seq(
      $.Identifier, '#', '(', $.type, repeatseq(',', $.type), ')'
    )),

    typeclassDef: $ => prec.left(seq(
      'typeclass', $.typeclassIde, $.typeFormals, optional($.provisos), 
        optional($.typedepends), ';', 
          repeat($.overloadedDef), 
      'endtypeclass', optseq(':', $.typeclassIde)
    )),
    typeclassIde: $ => $.Identifier,
    typedepends: $ => prec.left(seq(
      'dependencies', '(', $.typedepend, repeatseq(',', $.typedepend), ')'
    )),
    typedepend: $ => prec.left(seq(
      $.typelist, 'determines', $.typelist
    )),
    typelist: $ => choice(
      $.typeIde, 
      prec.left(seq('(', $.typeIde, repeatseq(',', $.typeIde), ')'))
    ),
    overloadedDef: $ => choice(
      $.functionProto, 
      $.varDecl,
      $.moduleProto, // NOTE: missing moduleProto?
    ),

    // Instance declarations
    typeclassInstanceDef: $ => prec.left(seq(
      'instance', $.typeclassIde, '#', '(', $.type, repeatseq(',', $.type), ')',
        optional($.provisos), ';',
          repeat(choice(seq($.varAssign, ';'), $.functionDef, $.moduleDef)),
      'endinstance', optseq(':', $.typeclassIde)
    )),

    derives: $ => prec.left(seq(
      'deriving', '(', $.typeclassIde, repeat(seq(',', $.typeclassIde)), ')'
    )),

    // ================================================================
    // sec 9 Variable declarations and statements

    varDecl: $ => choice(
      prec.right(seq($.type, $.varInit, repeatseq(',', $.varInit), ';')),
      prec.left(seq('let', $.identifier, '=', $.expression, ';')),
    ),

    varInit: $ => prec.left(seq(
      $.identifier, optional($.arrayDims), optseq('=', $.expression)
    )),
    arrayDims: $ => prec.left(seq(
      '[', $.expression, ']', repeatseq('[', $.expression, ']')
    )),

    // Variable assignment 
    varAssign: $ => choice(
      prec.left(seq($.lValue, '=', $.expression, ';')),
      // if expression is the value returned by an actionvalue method:
      prec.left(seq('let', $.identifier, '<-', $.expression, ';')),
      // sec 11.4 pattern matching
      seq('match', $.pattern, '=', $.expression, ';')
    ),
    lValue: $ => choice(
      $.identifier, 
      prec.right(seq($.lValue, '.', $.identifier)),
      prec.right(seq($.lValue, '[', $.expression, ']')),
      prec.right(seq($.lValue, '[', $.expression, ':', $.expression, ']'))
    ),
    varDeclDo: $ => seq($.type, $.identifier, '<-', $.expression, ';'),
    varDo: $ => seq($.identifier, '<-', $.expression, ';'),
    
    regWrite: $ => choice(
      seq($.lValue, '<=', $.expression),
      seq('(', $.expression, ')', '<=', $.expression),
      seq($.lValue, $.arrayIndexes, '<=', $.expression),
      seq($.lValue, '[', $.expression, ':', $.expression, ']', '<=', $.expression),
      seq($.lValue, '.', $.identifier, '<=', $.expression),
    ),
    arrayIndexes: $ => repeat1(seq('[', $.expression, ']')),
    
    //
    // ================================================================
    // sec 9.8 Function definitions

    functionDef: $ => seq(
      optional($.attributeInstances), 
      $.functionProto,
        $.functionBody, 
      'endfunction', optseq(':', $.identifier)
    ),
    functionProto: $ => choice(
      seq('function', $.type, $.identifier,
          // NOTE: Optional parens, contrary to spec?
          optseq('(', optional($.functionFormals), ')'), optional($.provisos), ';'
      ),
      // Assign to expression (sec 9.8.1). 
      // Could also add optional(expression) to above choice arm.
      seq('function', $.type, $.identifier,
          optseq('(', optional($.functionFormals), ')'), optional($.provisos), 
          $.expression, ';'
      ),
    ),
    functionFormals: $ => prec.left(seq(
      $.functionFormal, repeatseq(',', $.functionFormal)
    )),
    functionFormal: $ => prec.left(seq($.type, $.identifier)),

    functionBody: $ => choice(
      $.actionBlock,
      $.actionValueBlock,
      // NOTE: Spec allows repeat0, but there is no way to have an empty body?
      repeat1($.functionBodyStmt),
    ),
    functionBodyStmt: $ => choice(
      $.returnStmt, 
      $.varDecl, 
      $.varAssign, 
      $.functionDef,
      $.moduleDef, 
      ctxtBeginEndStmt($, $.functionBodyStmt),
      ctxtIf($, $.functionBodyStmt),
      ctxtCase($, $.functionBodyStmt),
      ctxtFor($, $.functionBodyStmt),
      ctxtWhile($, $.functionBodyStmt),
    ),
    returnStmt: $ => seq('return', $.expression, ';'),


    // ================================================================
    // sec 10 Expressions

    expression: $ => choice(
      $.condExpr, 
      $.operatorExpr, 
      $.exprPrimary
    ),

    condExpr: $ => prec.right(seq(
      $.condPredicate, '?', $.expression, ':', $.expression
    )),

    operatorExpr: $ => choice(
      prec.right(seq($.unop, $.expression)),
      prec.left(seq($.expression, $.binop, $.expression))
    ),

    // The following operators are in order of decreasing precedence
    unop: $ => token(choice(
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
    binop: $ => token(choice(
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

    exprPrimary: $ => choice(
      $.identifier,
      $.intLiteral,
      $.realLiteral,
      $.stringLiteral,
      $.systemFunctionCall,
      field('dont_care', '?'),
      prec.left(seq('(', $.expression, ')')),
      seq('valueOf', '(', $.type, ')'),
      seq('valueof', '(', $.type, ')'),
      $.bitConcat,
      $.bitSelect,
      $.beginEndExpr,
      $.actionBlock,
      $.actionValueBlock,
      $.functionCall,
      $.methodCall,
      $.typeAssertion,
      $.structExpr,
      prec.left(seq($.exprPrimary, '.', $.identifier)),
      $.taggedUnionExpr,
      $.interfaceExpr,
      $.rulesExpr,
      $.seqFsmStmt,
      $.parFsmStmt,
    ),

    bitConcat: $ => seq(
      '{', $.expression, repeatseq(',', $.expression), '}'
    ),
    bitSelect: $ => prec.left(seq(
      $.exprPrimary, '[', $.expression, optseq(':', $.expression), ']'
    )),

    beginEndExpr: $ => prec.left(seq(
      'begin', optseq(':', $.identifier),
        repeat($.expressionStmt), 
        $.expression, 
      'end', optseq(':', $.identifier)
    )),

    actionBlock: $ => prec.left(seq(
      'action', optional(seq(':', $.identifier)),
        repeat($.actionStmt), 
      'endaction', optseq(':', $.identifier)
    )),
    actionStmt: $ => choice(
      $.regWrite,
      $.varDo,
      $.varDeclDo,
      $.functionCall,
      $.systemTaskStmt,
      seq('(', $.expression, ')'),
      $.actionBlock,
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.methodDef,
      ctxtBeginEndStmt($, $.actionStmt),
      ctxtIf($, $.actionStmt),
      ctxtCase($, $.actionStmt),
      ctxtFor($, $.actionStmt),
      ctxtWhile($, $.actionStmt),
    ),

    actionValueBlock: $ => prec.left(seq(
      'actionvalue', optional(seq(':', $.identifier)),
        repeat($.actionValueStmt), 
      'endactionvalue', optseq(':', $.identifier)
    )),
    actionValueStmt: $ => choice(
      $.regWrite,
      $.varDo,
      $.varDeclDo,
      $.functionCall,
      $.systemTaskStmt,
      seq('(', $.expression, ')'),
      $.returnStmt,
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.methodDef,
      ctxtBeginEndStmt($, $.actionValueStmt),
      ctxtIf($, $.actionValueStmt),
      ctxtCase($, $.actionValueStmt),
      ctxtFor($, $.actionValueStmt),
      ctxtWhile($, $.actionValueStmt),
    ),

    // NOTE: We make function and method call parens madatory, 
    //       contrary to spec, but same as bsc compiler implementation.
    functionCall: $ => prec.left(seq(
      $.exprPrimary, 
      '(',
        optseq($.expression, repeatseq(',', $.expression)),
      ')'
    )),
    methodCall: $ => prec.left(seq(
      $.exprPrimary, '.', $.identifier,
      '(',
        optseq($.expression, repeatseq(',', $.expression)),
      ')',
    )),

    typeAssertion: $ => choice(
      seq($.type, '’', $.bitConcat),
      seq($.type, '’', '(', $.expression, ')')
    ),

    structExpr: $ => seq(
      $.Identifier, '{', 
        $.memberBind, repeatseq(',', $.memberBind), 
      '}'
    ),
    memberBind: $ => seq($.identifier, ':', $.expression),

    taggedUnionExpr: $ => choice(
      seq('tagged', $.Identifier, '{', $.memberBind, repeatseq(',', $.memberBind), '}'),
      prec.left(seq('tagged', $.Identifier, $.exprPrimary))
    ),

    interfaceExpr: $ => prec.left(seq(
      'interface', $.Identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optseq(':', $.Identifier)
    )),
    interfaceStmt: $ => choice(
      $.methodDef, 
      $.subinterfaceDef, 
      $.expressionStmt
    ),

    rulesExpr: $ => prec.left(seq(
      optional($.attributeInstances), 
      'rules', optseq(':', $.identifier), 
        $.rulesStmt,
      'endrules', optseq(':', $.identifier)
      )),
    rulesStmt: $ => choice(
      $.rule, 
      $.expressionStmt
    ),

    // ================================================================
    // sec 11 Pattern matching

    pattern: $ => choice(
      prec.right(seq('.', $.identifier)),
      '.*',
      $.constantPattern,
      $.taggedUnionPattern,
      $.structPattern,
      $.tuplePattern,
    ),
    constantPattern: $ => choice(
      $.intLiteral, 
      $.realLiteral, 
      $.stringLiteral,
      $.Identifier
    ),
    taggedUnionPattern: $ => prec.left(seq(
      'tagged', $.Identifier, optional($.pattern)
    )),
    structPattern: $ => prec.left(seq(
      $.Identifier, '{', $.identifier, ':', $.pattern,
                         repeatseq(',', $.identifier, ':', $.pattern), '}'
    )),
    tuplePattern: $ => prec.left(seq(
      '{', $.pattern, repeatseq(',', $.pattern), '}'
    )),

    // ============================================================
    // sec 11.2 Case expressions with pattern matching

    caseExpr: $ => prec.left(seq(
      'case', '(', $.expression, ')', 'matches'.
        repeat($.caseExprItem),
      'endcase'
    )),
      
    caseExprItem: $ => choice(
      prec.left(seq(
        $.pattern, optseq('&&&', $.expression), ':', $.expression
      )),
      seq('default', optional(':'), $.expression)
    ),

    // ================================================================
    // sec 12 Finite State Machines

    fsmStmt: $ => choice(
      $.exprFsmStmt,
      $.seqFsmStmt,
      $.parFsmStmt,
      $.ifFsmStmt,
      $.whileFsmStmt,
      $.repeatFsmStmt,
      $.forFsmStmt,
      $.returnFsmStmt,
    ),
    exprFsmStmt: $ => choice(
      seq($.regWrite, ';'), 
      seq($.expression, ';')
    ),
    seqFsmStmt: $ => prec.left(seq(
      'seq', $.fsmStmt, repeat($.fsmStmt), 'endseq'
    )),
    parFsmStmt: $ => prec.left(seq(
      'par', $.fsmStmt, repeat($.fsmStmt), 'endpar'
    )),
    ifFsmStmt: $ => prec.left(seq(
      'if', $.expression, $.fsmStmt,
      optional(seq('else', $.fsmStmt))
    )),
    returnFsmStmt: $ => seq('return', ';'),
    whileFsmStmt: $ => prec.left(seq(
      'while', '(', $.expression, ')', $.loopBodyFsmStmt
    )),
    forFsmStmt: $ => prec.left(seq(
      'for', '(', $.fsmStmt, ';', $.expression, ';', $.fsmStmt, ')', 
        $.loopBodyFsmStmt
    )),
    repeatFsmStmt: $ => prec.left(seq(
      'repeat', '(', $.expression, ')', 
        $.loopBodyFsmStmt
    )),
    loopBodyFsmStmt: $ => choice(
      $.fsmStmt, 
      seq('break', ';'),
      seq('continue', ';')
    ),

    // ================================================================
    // sec 13 Important primitives
    // TODO: Not whole spec implemented here

    systemTaskStmt: $ => choice(
      seq($.systemTaskCall, ';'),
      // This should laos cover sec 13.8.4 Writing to a file, and other
      seq($.displayTaskName, '(', optseq($.expression, 
                                         repeatseq(',', $.expression)), ')', ';'),
      // $fclose ( fileIdentifier ) ;
      seq('$fclose', '(', $.identifier, ')'),
    ),
    displayTaskName: $ => choice('$display', '$displayb', '$displayo',
      '$displayh', '$write', '$writeb', '$writeo', '$writeh',
      '$format', '$fopen', '$fclose', '$fdisplay',
      '$fdisplayb', '$fdisplayo', '$fdisplayh', '$fwrite',
      '$fwriteb', '$fwriteo', '$fwriteh', '$swrite',
      '$swriteb', '$swriteo', '$swriteh', '$sformat',
      '$swriteAV', '$swritebAV', '$swriteoAV', '$swritehAV',
      '$sformatAV', '$fgetc', '$fungetc', '$fflush',
      '$finish', '$stop', '$dumpvars', '$dumpon', '$dumpoff',
      '$time', '$stime', '$realtobits', '$bitstoreal',
      '$test$plusargs'
    ),
    systemTaskCall: $ => choice(
      seq('$format', '(', optseq($.expression, repeatseq(',', $.expression)), ')'),
      // $fopen ( fileName [ , fileType ] )
      seq('$fopen', '(', optseq($.identifier, repeatseq(',', $.identifier)), ')'),
    ),
    systemFunctionCall: $ => choice('$time', '$stime'),

    // ================================================================
    // sec 14 Guiding the compiler with attributes

    attributeInstances: $ => prec.left(seq(
      $.attributeInstance, repeat($.attributeInstance)
    )),
    attributeInstance: $ => prec.left(seq(
      '(*', $.attrSpec, repeatseq(',', $.attrSpec), '*)'
    )),
    attrSpec: $ => seq($.attrName, optseq('=', $.expression)),
    // Could be maybe more specific here, and list all the possible names
    attrName: $ => choice($.identifier, $.Identifier), 
    

    // ============================================================
    // TODO: sec 15 Embedding RTL in a BSV design
    // externModuleImport ::= import "BVI" [ identifier = ] moduleProto
    //                         { moduleStmt }
    //                         { importBVIStmt }
    //                        endmodule [ : identifier ]


    // ============================================================
    // sec 16 Embedding C in a BSV Design

    externCImport: $ => seq(
      'import', '"BDPI"', optseq($.identifier, '='), 'function', $.type, 
      $.identifier, '(', optional($.CFuncArgs), ')', optional($.provisos), ';'
    ),
    CFuncArgs: $ => prec.left(seq(
      $.CFuncArg, repeat(seq(',', $.CFuncArg))
    )),
    CFuncArg: $ => prec.left(seq(
      $.type, optional($.identifier)
    )),


    // =============================================================
    // sec 2 Lexical elements

    intLiteral: $ => choice(
      "'0'", 
      "'1", 
      $.sizedIntLiteral, 
      $.unsizedIntLiteral
    ),
    sizedIntLiteral: $ => seq($.bitWidth, $.baseLiteral),
    unsizedIntLiteral: $ => choice(
      seq(optional($.sign), $.baseLiteral),
      seq(optional($.sign), $.decNum)
    ),
    baseLiteral: $ => choice(
      seq(choice("'d", "'D"), $.decDigitsUnderscore),
      seq(choice("'h", "'H"), $.hexDigitsUnderscore),
      seq(choice("'o", "'O"), $.octDigitsUnderscore),
      seq(choice("'b", "'B"), $.binDigitsUnderscore),
    ),
    decNum: $ => seq($.decDigits, optional($.decDigitsUnderscore)),
    bitWidth: $ => $.decDigits,
    sign: $ => choice('+', '-'),
    // NOTE: The below rules require at least one digit, 
    //       not at least 0 like in the spec.
    decDigits: $ => /[0-9]+/,
    decDigitsUnderscore: $ => /[0-9_]+/,
    hexDigitsUnderscore: $ => /[0-9a-fA-F_]+/,
    octDigitsUnderscore: $ => /[0-7_]+/,
    binDigitsUnderscore: $ => /[0-1_]+/,

    stringLiteral: $ => /"([^"\\]|\\[\s\S])*"/,

    realLiteral: $ => choice(
      seq($.decNum, optseq('.', $.decDigitsUnderscore), $.exp, 
                    optional($.sign), $.decDigitsUnderscore),
      seq($.decNum, '.', $.decDigitsUnderscore)
    ),
    exp: $ => choice('e', 'E'),

    identifier: $ => token(/[a-zA-Z_][a-zA-Z0-9_]*/), // Identifier starting with any case.
    Identifier: $ => token(/[A-Z_][a-zA-Z0-9_]*/), // Identifier starting with upper case.

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    // from: https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    comment: $ => token(choice(
      seq('//', /.*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      ),
    )),

  },

  extras: $ => [/\s/, $.comment],

  // tree-sitter has automatic keyword extraction
  word: $ => $.identifier,

  conflicts: $ => [
    [$.typeNat, $.decNum],
    [$.typeNat, $.decNum],
    [$.bitWidth, $.decNum],
    [$.lValue, $.exprPrimary],
    [$.typeIde, $.exprPrimary],
    [$.typeIde, $.methodDef],
    [$.exprOrCondPattern, $.ifFsmStmt],
    [$.condPredicate, $.condPredicate],
    [$.condExpr, $.exprOrCondPattern],
    [$.expression, $.functionCall],
    [$.operatorExpr, $.exprOrCondPattern],
    [$.exprPrimary, $.fsmStmt],
    [$.exprPrimary, $.moduleStmt],
    [$.systemTaskStmt, $.displayTaskName],
    [$.displayTaskName, $.systemTaskCall],
    [$.displayTaskName, $.systemFunctionCall],
    [$.unsizedIntLiteral, $.realLiteral],
    [$.moduleStmt, $.expressionStmt],
    [$.exprPrimary, $.actionStmt],
    [$.exprPrimary, $.actionValueStmt],
    [$.expressionStmt, $.actionStmt],
    [$.expressionStmt, $.actionValueStmt],
    [$.exprOrCondPattern, $.regWrite],
    [$.lValue, $.arrayIndexes],

    [$.exprOrCondPattern, $.exprOrCondPattern],
    [$.expression, $.exprPrimary, $.methodCall],
    [$.typeIde, $.exprPrimary, $.moduleApp],
  ]
});
