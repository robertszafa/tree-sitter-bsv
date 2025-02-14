/**
 * @file A tree-sitter grammar for Bluespec System Verilog (BSV)
 * @author Robert Szafarczyk <robertszafa@gmail.com>
 * @license MIT
 */

/// This grammar implements the BSV language as described in:
/// - Bluespec System Verilog language Reference Guide: 
///     https://github.com/B-Lang-org/bsc/releases/latest/download/BSV_lang_ref_guide.pdf
/// When we deviate from the spec, it is only to match the behaviour of the bsc compiler implemetation.
/// These cases are explained in 'NOTE' comments.

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
      $.expression, repeatseq(',', $.expression), ':', ctxtStmt
  ));

  let CasePatItem = () => prec.left(seq(
      $.pattern, repeatseq('&&&', $.expression), ':', ctxtStmt
  ));

  let DefaultItem = () => prec.left(seq(
    'default', optional(':'), ctxtStmt
  ));

  return field("case_expr", choice(
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
  ));
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
    let forIncr = () => prec.left(seq(
      varIncr(), repeatseq(',', varIncr())
    ));

    let forTest = () => $.expression;

  return field("forStmt", prec.left(seq(
    'for', '(', forInit(), ';', forTest(), ';', forIncr(), ')',
      ctxtStmt
  )));
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
      // NOTE: Contrary to spec, the actual bsc implementation allows files 
      //       without a package declaration.
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
      // NOTE: Only the below "varAssign" rule choice is valid at packageStmt scope,
      //       contrary to the spec.
      prec.left(seq($.lValue, '=', $.expression, ';')),
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

    type: $ => seq(
      // NOTE: bsc allows namespaced types, e.g., Prelude::List.
      repeatseq($.identifier, '::'), 
      choice(
        $.typePrimary, 
        prec.left(seq(
          $.typePrimary, '(', $.type, repeatseq(',', $.type), ')'
        ))
      )
    ),

    typePrimary: $ => choice(
      seq($.typeIde, optseq('#', '(', $.type, repeatseq(',', $.type), ')')),
      $.typeNat,
      seq('bit', optseq('[', $.typeNat, ':', $.typeNat, ']'))
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
      // NOTE 1: The spec has a type for "typeFormal::= [ numeric | string ] type typeIde"
      //         The "type" string should be optional: we can have "numeric type typePrimary"
      //         or "type typePrimary". Also allow "parameter".
      // NOTE 2: The spec wrongly does not allow nested types, do s/typeIde/typePrimary
      optional(choice('numeric', 'string', 'parameter')), optional('type'), $.typePrimary
    ),

    interfaceMemberDecl: $ => choice($.methodProto, $.subinterfaceDecl),

    methodProto: $ => seq(
      optional($.attributeInstances), 
      'method', $.type, $.identifier, optseq('(', optional($.methodProtoFormals), ')'), ';'
    ),

    methodProtoFormals: $ => seq(
      $.methodProtoFormal, repeatseq(',', $.methodProtoFormal)
    ),
    // NOTE: A function can be a method parameter.
    methodProtoFormal: $ => choice(
      seq(optional($.attributeInstances), $.type, $.identifier),
      $.functionProto
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
      // NOTE: The spec {module, function, action, actionValue, etc}Stmt rules 
      //       are missing semicolons in some places.
      prec.left(seq($.functionCall, ';')),
      $.systemTaskStmt,
      prec.left(seq('(', $.expression, ')', ';')),
      $.returnStmt,
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.moduleDef,
      // NOTE: The spec does not say this, but bsc allows naked condExprs.
      seq($.condExpr, ';'),
      ctxtBeginEndStmt($, $.moduleStmt),
      ctxtIf($, $.moduleStmt),
      ctxtCase($, $.moduleStmt),
      ctxtFor($, $.moduleStmt),
      ctxtWhile($, $.moduleStmt),
    ),


    // ============================================================
    // sec 5.4 Module and interface instantiation

    // sec 5.4.1 Short form instantiation
    moduleInst: $ => choice(
      prec.left(seq(
        optional($.attributeInstances),
        // NOTE: Contrary to spec, we make the type optional, since
        //       we could be assigning to an already declared Vector for example.
        //       We also do s/identifier/lValue to allow asigning to array indexes,
        //       or to allow things like concurrent registers.
        optional($.type), $.lValue, '<-', $.moduleApp, ';'
      )),
      // Long Form
      prec.left(seq(
        optional($.attributeInstances), 
        $.type, $.identifier, '(', ')', ';', 
        $.moduleApp2, $.identifier, '(', optional($.moduleActualArgs), ')', ';'
      ),
      )
    ),
    moduleApp: $ => prec.left(seq(
      // NOTE: Parens are optional in the bsc implementation.
      $.identifier, optseq('(', 
        optseq($.moduleActualParamArg, repeatseq(',', $.moduleActualParamArg)),
      ')')
    )),
    moduleActualParamArg: $ => choice(
      $.expression,
      seq('clocked_by', $.expression),
      seq('reset_by', $.expression)
    ),

    // sec 5.4.2 Long form instantiation (depractaed)
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
      prec.left(seq($.expression, 'matches', $.pattern)),
      // NOTE: Contrary to spec, a pattern and an expression can be in parens.
      prec.left(seq($.expression, 'matches', '(', $.pattern, ')')),
      prec.left(seq('(', $.expression, ')')),
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
        // NOTE: The spec allows an empty body, but bsc does not.
        repeat1($.unionMember), 
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
      // NOTE: We removed the semicolon from the functionProto rule
      //       to allow functionProtos to appear in method/function arguments.
      seq($.functionProto, ';'),
      $.varDecl,
      $.moduleProto, // NOTE: missing moduleProto?
    ),

    // Instance declarations
    typeclassInstanceDef: $ => prec.left(seq(
      'instance', $.typeclassIde, '#', '(', $.type, repeatseq(',', $.type), ')',
        optional($.provisos), ';',
          // NOTE: Spec has an erronous semicolon after varAssign.
          repeat(choice($.varAssign, $.functionDef, $.moduleDef)),
      'endinstance', optseq(':', $.typeclassIde)
    )),

    derives: $ => prec.left(seq(
      'deriving', '(', $.typeclassIde, repeat(seq(',', $.typeclassIde)), ')'
    )),

    // ================================================================
    // sec 9 Variable declarations and statements

    varDecl: $ => choice(
      prec.left(seq($.type, $.varInit, repeatseq(',', $.varInit), ';')),
      prec.left(seq('let', $.identifier, '=', $.expression, ';')),
      // NOTE: The spec is missing the below rule to allow declarations like:
      //       Reg#(Maybe#(t)) data();
      prec.left(seq($.type, $.identifier, 
        '(', 
          optseq($.expression, repeatseq(',', $.expression)),
        ')', ';')
      ),
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
      prec.left(seq('match', $.pattern, '=', $.expression, ';'))
    ),
    lValue: $ => choice(
      $.identifier, 
      // NOTE: The spec is missing this.
      $.tupleBind,
      prec.right(seq($.lValue, '.', $.identifier)),
      prec.right(seq($.lValue, '[', $.expression, ']')),
      prec.right(seq($.lValue, '[', $.expression, ':', $.expression, ']'))
    ),
    varDeclDo: $ => prec.left(seq($.type, $.identifier, '<-', $.expression, ';')),
    varDo: $ => prec.left(seq($.identifier, '<-', $.expression, ';')),

    // NOTE: The spec does not have this rule, but it is supported by bsc.
    //       This should work: {b, i, .*} <- mkSub;
    tupleBind: $ => prec.left(seq(
      '{', choice($.identifier, '.*'), repeatseq(',', choice($.identifier, '.*')), '}'
    )),
    
    regWrite: $ => choice(
      // NOTE: Spec is missing semicolon. regWrite could be without one, 
      //       relying on super rules to add one, but this is not the approach
      //       taken with other rules in the spec, e.g., varDo.
      prec.left(seq($.lValue, '<=', $.expression, ';')),
      prec.left(seq('(', $.expression, ')', '<=', $.expression, ';')),
      prec.left(seq($.lValue, $.arrayIndexes, '<=', $.expression, ';')),
      prec.left(seq($.lValue, '[', $.expression, ':', $.expression, ']', '<=', $.expression, ';')),
      prec.left(seq($.lValue, '.', $.identifier, '<=', $.expression, ';')),
    ),
    arrayIndexes: $ => repeat1(seq('[', $.expression, ']')),
    
    //
    // ================================================================
    // sec 9.8 Function definitions

    functionDef: $ => prec.left(seq(
      optional($.attributeInstances), 
      // NOTE: We removed the semicolon from the functionProto rule
      //       to allow functionProtos to appear in method/function arguments.
      $.functionProto, ';',
        $.functionBody, 
      'endfunction', optseq(':', $.identifier)
    )),

    // NOTE: Contrary to the spec, our rule does not have a semicolon, requiring
    //       any super rules to handle them. This is to enable function prototypes
    //       to appear in function/method arguments.
    //       Also, we have optional parens, contrary to spec?
    functionProto: $ => prec.left(seq(
      'function', $.type, choice($.identifier, seq('\\', $.binop)),
          optseq('(', optional($.functionFormals), ')'), 
          optional($.provisos), optional($.expression)
    )),
    functionFormals: $ => prec.left(seq(
      $.functionFormal, repeatseq(',', $.functionFormal)
    )),
    functionFormal: $ => choice(
      // NOTE: A function can take another function as parameter.
      prec.left(seq($.type, $.identifier)),
      $.functionProto
    ),

    functionBody: $ => choice(
      $.actionBlock,
      $.actionValueBlock,
      // NOTE: Spec allows repeat0, but there is no way to have an empty body?
      repeat1($.functionBodyStmt),
      // NOTE: BSV allows returning function value by assigning to function name.
      //       The spec doesn't provide the below explicit rule to allow this in general.
      field('functionNameAssign', 
        prec.left(seq($.identifier, '=', $.functionBodyStmt, ';'))),
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
      // NOTE: The spec is missing the power binop.
      prec(83, '**'),
      prec(82, '*'),
      prec(82, '/'),
      prec(82, '%'),
      prec(81, '+'),
      prec(81, '-'),
      prec(80, '<<'),
      prec(80, '>>'),
      prec(79, '<='),
      prec(79, '>='),
      prec(79, '<'),
      prec(79, '>'),
      prec(78, '=='),
      prec(78, '!='),
      prec(77, '&'),
      prec(76, '^'),
      prec(75, '^~'),
      prec(75, '~^'),
      prec(74, '|'),
      prec(73, '&&'),
      prec(72, '||'),
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
      'action', optseq(':', $.identifier),
        repeat($.actionStmt), 
      'endaction', optseq(':', $.identifier)
    )),
    actionStmt: $ => choice(
      $.regWrite,
      $.varDo,
      $.varDeclDo,
      prec.left(seq($.functionCall, ';')),
      prec.left(seq($.methodCall, ';')),
      $.systemTaskStmt,
      prec.left(seq('(', $.expression, ')', ';')),
      $.actionBlock,
      $.varDecl,
      $.varAssign,
      $.functionDef,
      $.methodDef,
      // NOTE: The bsc implementation provides a "noAction" statement. 
      field('noAction', 'noAction'),
      ctxtBeginEndStmt($, $.actionStmt),
      ctxtIf($, $.actionStmt),
      ctxtCase($, $.actionStmt),
      ctxtFor($, $.actionStmt),
      ctxtWhile($, $.actionStmt),
    ),

    actionValueBlock: $ => prec.left(seq(
      'actionvalue', optseq(':', $.identifier),
        repeat($.actionValueStmt), 
      'endactionvalue', optseq(':', $.identifier)
    )),
    actionValueStmt: $ => choice(
      $.regWrite,
      $.varDo,
      $.varDeclDo,
      prec.left(seq($.functionCall, ';')),
      prec.left(seq($.methodCall, ';')),
      $.systemTaskStmt,
      prec.left(seq('(', $.expression, ')', ';')),
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

    // NOTE: Make parenthesis madatory in function calls, otherwise
    //       we run into infinite left recursion problems. The bsc implementation
    //       does allow funcCall without parens. We will also accept such "lVal = funcCall;"
    //       syntax, but our parser will produce an "identifier" node for "funcCall".
    //       Tools upstream can then detect if the identifier refers to a function.
    //
    //       We also set a higher precedence for method calls to parse 
    //       "identifierA.identifierB" as a methodCall.
    functionCall: $ => prec.left(40, seq(
      $.exprPrimary, 
      '(', 
        optseq($.expression, repeatseq(',', $.expression)),
      ')'
    )),
    // We keep the parens as optional in method calls, as the GLR can recover from
    // the left recursion due to the ".identifier" part that follows.
    methodCall: $ => prec.left(50, seq(
      $.exprPrimary, '.', $.identifier,
      optseq('(', 
        optseq($.expression, repeatseq(',', $.expression)),
      ')'),
    )),

    typeAssertion: $ => choice(
      seq($.type, '’', $.bitConcat),
      seq($.type, '’', '(', $.expression, ')')
    ),

    structExpr: $ => prec.left(seq(
      // $.Identifier, '{', // TODO: Why does upper case not work here?
      $.identifier, '{', 
        $.memberBind, repeatseq(',', $.memberBind), 
      '}'
    )),
    memberBind: $ => prec.left(seq($.identifier, ':', $.expression)),

    taggedUnionExpr: $ => choice(
      // NOTE: Contrary to spec, but to match the bsc implementation, we make curly brackets optional if no binds.
      prec.left(seq('tagged', $.Identifier, '{', $.memberBind, repeatseq(',', $.memberBind), '}')),
      prec.left(seq('tagged', $.Identifier, $.exprPrimary)),
      prec.left(seq('tagged', $.Identifier)),
    ),

    interfaceExpr: $ => prec.left(seq(
      'interface', $.Identifier, ';',
        repeat($.interfaceStmt), 
      'endinterface', optseq(':', $.Identifier)
    )),

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
      $.boolLiteral,
      $.Identifier,
    ),
    taggedUnionPattern: $ => choice(
      prec.left(seq(
        'tagged', $.Identifier, optional($.pattern)
      )),
      // NOTE: The spec is missing the case of a struct union member.
      prec.left(seq(
        'tagged', $.structPattern
      )),
    ),
    structPattern: $ => prec.left(seq(
      $.Identifier, '{', optseq($.identifier, ':', $.pattern,
                                repeatseq(',', $.identifier, ':', $.pattern)), '}'
    )),
    tuplePattern: $ => prec.left(seq(
      '{', $.pattern, repeatseq(',', $.pattern), '}'
    )),
    
    // ================================================================
    // sec 12 Finite State Machines

    fsmStmt: $ => choice(
      // NOTE: We did s/exprFsmStmt/actionStmt. The exprFsmStmt rule 
      //       has an choice of "expression ;", which, for example, 
      //       would require to put a semicolon after an action block.
      $.actionStmt,
      $.seqFsmStmt,
      $.parFsmStmt,
      $.ifFsmStmt,
      $.whileFsmStmt,
      $.repeatFsmStmt,
      $.forFsmStmt,
      $.returnFsmStmt,
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
      prec.left(seq($.systemTaskCall, ';')),
      // This should laos cover sec 13.8.4 Writing to a file, and other
      prec.left(seq($.displayTaskName, optseq('(', optseq($.expression, 
                                              repeatseq(',', $.expression)), ')'), ';'
      )),
      // $fclose ( fileIdentifier ) ;
      seq('$fclose', '(', $.identifier, ')', ';'),
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

    boolLiteral: $ => choice("True", "False"),

    realLiteral: $ => choice(
      seq($.decNum, optseq('.', $.decDigitsUnderscore), $.exp, 
                    optional($.sign), $.decDigitsUnderscore),
      seq($.decNum, '.', $.decDigitsUnderscore)
    ),
    exp: $ => choice('e', 'E'),

    // \p{L} matches any character in the unicode category. \p{Lu} for upper case unicode letter.
    // For Unicode in regex, see https://www.regular-expressions.info/unicode.html
    identifier: $ => token(/[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*/), // Identifier starting with any case.
    Identifier: $ => token(/[A-Z_\p{Lu}][a-zA-Z0-9_\p{L}]*/), // Identifier starting with upper case.

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
    [$.operatorExpr, $.exprOrCondPattern],
    [$.exprPrimary, $.fsmStmt],
    [$.systemTaskStmt, $.displayTaskName],
    [$.displayTaskName, $.systemTaskCall],
    [$.displayTaskName, $.systemFunctionCall],
    [$.unsizedIntLiteral, $.realLiteral],
    [$.moduleStmt, $.expressionStmt],
    [$.exprPrimary, $.actionStmt],
    [$.expressionStmt, $.actionStmt],
    [$.expressionStmt, $.actionValueStmt],
    [$.lValue, $.arrayIndexes],

    [$.exprOrCondPattern, $.exprOrCondPattern],
    [$.exprPrimary, $.structExpr],
    [$.exprOrCondPattern, $.exprPrimary],
    [$.lValue, $.actionStmt],
    [$.actionStmt, $.fsmStmt],
    [$.lValue, $.varDo],
    [$.varInit, $.lValue],
    [$.lValue, $.varDeclDo],
    [$.typeIde, $.structExpr],
    [$.tupleBind, $.exprPrimary],
    [$.lValue, $.functionBody],

  ]

});
