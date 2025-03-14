/**
 * @file A tree-sitter grammar for Bluespec System Verilog (BSV)
 * @author Robert Szafarczyk <robertszafa@gmail.com>
 * @license MIT
 */

/// This grammar implements the BSV language as described in:
/// - Bluespec System Verilog language Reference Guide: 
///     github.com/B-Lang-org/bsc/releases/latest/download/BSV_lang_ref_guide.pdf
/// When we deviate from, or add to, the spec, it is only to match the behaviour
/// of the bsc compiler implemetation: github.com/B-Lang-org/bsc
/// These deviations are explained in 'NOTE' comments.
///
/// One deviation made for another reason is dropping the distinction between
/// lower case "identifier" and upper case "Identifier". This decision was made
/// because the tree-sitter automatic keyword extraction feature requires that 
/// keywords be described by just one grammar rule.
/// Tools using the parser should check if for example a typeIde is upper case.

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
  // Note that we allow an optional 'return' in the items, since bsc allows this.
  let CaseItem = () => prec.left(seq(
      $.expression, repeatseq(',', $.expression), ':', optional('return'), ctxtStmt
  ));

  let CasePatItem = () => prec.left(seq(
      $.pattern, repeatseq('&&&', $.expression), ':', optional('return'), ctxtStmt
  ));

  let DefaultItem = () => prec.left(seq(
    'default', optional(':'), optional('return'), ctxtStmt
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
    let forInit = () => field('forInit', choice(forOldInit(), forNewInit()));

    let varIncr = () => field('varIncr', choice(
      prec.left(seq($.identifier, '=', $.expression)),
      prec.left(seq($.type, $.identifier)),
    ));
    let forIncr = () => field('forIncr', prec.left(seq(
      varIncr(), repeatseq(',', varIncr())
    )));

    let forTest = () => field('forTest', $.expression);

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
      prec.left(seq($.lValue, '=', $.rValue)),
      $.functionDef,
      $.typeclassDef,
      $.typeclassInstanceDef,
      // TODO: Add externModuleImport, sec. 15 from BSV lang ref
      $.externModuleImport, 
      // NOTE: externCImport was missing in the spec:
      $.externCImport, 
    ),

    //  NOTE: BSV lang ref guide requires upper case; 
    //  the actual bsc implementation does not care...
    packageIde: $ => $.identifier, 

    // ================================================================
    // sec 4 Types

    type: $ => prec.left(seq(
      // NOTE: bsc allows namespaced types, e.g., Prelude::List.
      optseq($.identifier, '::'), 
      choice(
        $.typePrimary, 
        // NOTE: A type can have optional parentheses around.
        prec.left(seq(
          '(', $.typePrimary, ')'
        )),
        prec.left(seq(
          $.typePrimary, '(', $.type, repeatseq(',', $.type), ')'
        ))
      )
    )),

    typePrimary: $ => prec.right(choice(
      prec.right(seq($.typeIde, optseq('#', '(', $.type, repeatseq(',', $.type), ')'))),
      $.typeNat,
      //  Only [N:0] bit vectors supported
      seq('bit', optseq('[', $.typeNat, ':', '0', ']')),
      $.functionType,
      // NOTE: A type can don't care in bsc.
      '?',
    )),

    functionType: $ => prec.right(seq(
      'function', $.type, choice($.identifier, seq('\\', $.binop)),
        optseq('(', optional($.functionFormals), ')')
    )),
    // NOTE: subFunctions use type inference in bsc, which means the return and 
    //       argument types can be ommited.
    subFunctionType: $ => prec.right(seq(
      'function', optional($.type), $.identifier,
        optseq('(', optional($.subFunctionFormals), ')')
    )),

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
      // NOTE: We do not allow varDo/varDeclDo here. Their syntax is covered by moduleInst.
      // $.varDo, $.varDeclDo,
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
    // NOTE: We do not provide rules for sec 5.4.2 Long form instantiation (depractaed),
    //       because this syntax is covered by the varDecl rule.
    moduleInst: $ => prec.left(seq(
      optional($.attributeInstances),
      // NOTE: Contrary to spec, we make the type optional, since
      //       we could be assigning to an already declared Vector for example.
      //       We also do s/identifier/lValue to allow asigning to array indexes,
      //       or to allow things like concurrent registers.
      optional($.type), $.lValue, '<-', $.moduleApp, ';'
    )),
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
        optional($.implicitCond), '=', $.rValue
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
        'interface', $.identifier, $.identifier, ';',
          repeat($.interfaceStmt), 
        'endinterface', optseq(':', $.identifier)
      ),
      // Short form (sec 5.5.3)
      seq(
        'interface', optional($.type), $.identifier, '=', $.rValue
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
      'typedef', 'enum', '{', $.typedefEnumElements, '}', $.identifier, 
        optional($.derives), ';'
    ),
    typedefEnumElements: $ => seq(
      $.typedefEnumElement, repeatseq(',', $.typedefEnumElement)
    ),
    typedefEnumElement: $ => choice(
      prec.left(seq($.identifier, optseq('=', $.intLiteral))),
      prec.left(seq($.identifier, '[', $.intLiteral, ']',
                  optseq('=', $.intLiteral))),
      prec.left(seq($.identifier, '[', $.intLiteral, ':', $.intLiteral, ']',
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
      prec.left(seq($.type, $.identifier, ';')),
      prec.left(seq($.subStruct, $.identifier, ';')),
      prec.left(seq($.subUnion, $.identifier, ';')),
      prec.left(seq('void', $.identifier, ';'))
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
      $.identifier, '#', '(', $.type, repeatseq(',', $.type), ')'
    )),

    typeclassDef: $ => prec.left(seq(
      'typeclass', $.typeclassIde, $.typeFormals, optional($.provisos), 
        optional($.typedepends), ';', 
          repeat($.overloadedDef), 
      'endtypeclass', optseq(':', $.typeclassIde)
    )),
    typeclassIde: $ => $.identifier,
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
    overloadedDef: $ => prec.right(choice(
      // NOTE: We removed the semicolon from the functionProto rule
      //       to allow functionProtos to appear in method/function arguments.
      //       We also allow functionDefs here, contrary to spec, but same as bsc.
      $.functionDef,
      prec.left(seq($.functionProto, ';')),
      // NOTE: A typeclass can contain module definitions and prototypes. 
      //       This is contrary to spec but same as bsc.
      $.moduleDef, 
      $.moduleProto, 
      $.varDecl,
    )),

    // Instance declarations
    typeclassInstanceDef: $ => prec.left(seq(
      // NOTE: This is missing from the spec, but bsc allows attributes here.
      optional($.attributeInstances),
      'instance', $.typeclassIde, '#', '(', $.type, repeatseq(',', $.type), ')',
        optional($.provisos), ';',
          // NOTE: Spec has an erronous semicolon after varAssign.
          //       We also use subFunctionDef rule, because bsc has type
          //       inference inside instance blocks.
          repeat(choice($.varAssign, $.subFunctionDef, $.moduleDef)),
      'endinstance', optseq(':', $.typeclassIde)
    )),

    derives: $ => prec.left(seq(
      'deriving', '(', $.typeclassIde, repeat(seq(',', $.typeclassIde)), ')'
    )),

    // ================================================================
    // sec 9 Variable declarations and statements
    // NOTE: We changed the rules significantly compared to the spec to
    //       closer match the behaviour of the bsc compiler (e.g., bsc allows
    //       lValues to be returned from a function, and has a tuple bind syntax
    //       missing from the spec), and to work around small ommisions in the spec
    //       (e.g., the spec forgets to require a semicolon at the end of a register
    //       write).
    //       The biggest change we do is to add an rValue rule. We also move some of the
    //       token matching from other rules to the lValue rule, where this makes sense.

    varDecl: $ => prec.right(choice(
      prec.left(seq($.type, $.varInit, repeatseq(',', $.varInit), ';')),
      prec.left(seq('let', $.identifier, '=', $.rValue)),
      // NOTE: The spec is missing the below rule to allow declarations like:
      //       Reg#(Maybe#(t)) data();
      prec.left(seq($.type, $.identifier, 
        '(', 
          optseq($.expression, repeatseq(',', $.expression)),
        ')', ';')
      ),
    )),
    varInit: $ => prec.left(seq(
      // NOTE: Contrary to the spec, we allow an lValue here, not just an identifier.
      //       The lValue arleady takes care of optional arrayIndexes.
      $.lValue, optseq('=', $.expression)
    )),

    // Variable assignment. Note that we use rValue instead of expression.
    varAssign: $ => choice(
      prec.left(seq($.lValue, '=', $.rValue)),
      // if expression is the value returned by an actionvalue method:
      prec.left(seq('let', $.identifier, '<-', $.rValue)),
      // sec 11.4 pattern matching
      prec.left(seq('match', $.pattern, '=', $.rValue))
    ),
    varDeclDo: $ => prec.left(seq($.type, $.identifier, '<-', $.rValue)),
    varDo: $ => prec.left(seq($.identifier, '<-', $.rValue)),
    
    // NOTE: No alternatives, since the lValue rule already takes care of the 
    //       different ways a register lValue can be accessed.
    regWrite: $ => prec.left(seq($.lValue, '<=', $.rValue)),

    lValue: $ => prec.left(choice(
      $.identifier, 
      // NOTE: The spec is missing tupleBind, and a function call that can return an lValue.
      $.tupleBind,
      // NOTE: We use identifier for function name in lValues, instead of exprPrimary.
      field('lValueFunctionCall', prec.left(seq(
        $.identifier, '(', optseq($.expression, repeatseq(',', $.expression)), ')'
      ))),
      prec.left(seq($.lValue, '.', $.identifier)),
      prec.left(seq($.lValue, $.arrayIndexes)),
      prec.left(seq($.lValue, '[', $.expression, ':', $.expression, ']')),
      prec.left(seq('(', $.expression, ')')),
    )),
    arrayIndexes: $ => prec.right(repeat1(seq('[', $.expression, ']'))),

    // NOTE: This rule does not exist in the language spec, but we add it here
    //       because we also allow a caseExpr to appear here, which the spec 
    //       misses. We use rValue instead of seq(expression, ';').
    rValue: $ => choice(
      seq($.expression, ';'),
      seq(ctxtCase($, seq($.expression, ';')), ';'),
    ),

    // NOTE: The spec does not have this rule, but it is supported by bsc.
    //       This should work: {b, i, .*} <- mkSub;
    tupleBind: $ => prec.left(seq(
      '{', choice($.identifier, '.*'), repeatseq(',', choice($.identifier, '.*')), '}'
    )),
    
    //
    // ================================================================
    // sec 9.8 Function definitions

    functionDef: $ => choice(
      prec.left(seq(
        optional($.attributeInstances), 
        // NOTE: We removed the semicolon from the functionProto rule
        //       to allow functionProtos to appear in method/function arguments.
        $.functionProto, ';',
          $.functionBody, 
        'endfunction', optseq(':', $.identifier)
      )),
      // NOTE: A function def, similarly to method def, can have a short form.
      prec.left(seq(
        optional($.attributeInstances), 
        $.functionProto, '=', $.rValue
      )),
    ),

    // NOTE: Contrary to the spec, our rule does not have a semicolon, requiring
    //       any super rules to handle them. This is to enable function prototypes
    //       to appear in function/method arguments.
    //       Also, we have optional parens, contrary to spec?
    functionProto: $ => prec.left(seq(
      $.functionType, optional($.provisos), optional($.expression)
    )),
    functionFormals: $ => prec.left(seq(
      $.functionFormal, repeatseq(',', $.functionFormal)
    )),
    functionFormal: $ => choice(
      prec.left(seq($.type, $.identifier)),
      // NOTE: A function can take another function as parameter.
      $.functionProto
    ),

    // NOTE: subFunctions use type inference in bsc, which means the return and 
    //       argument types can be ommited. Hence separate rules for sub functions.
    subFunctionDef: $ => choice(
      prec.left(seq(
        $.subFunctionProto, ';',
          $.functionBody, 
        'endfunction', optseq(':', $.identifier)
      )),
      prec.left(seq(
        $.subFunctionProto, '=', $.rValue
      )),
    ),
    subFunctionProto: $ => prec.left(seq(
      $.subFunctionType, optional($.provisos), optional($.expression)
    )),
    subFunctionFormals: $ => prec.left(seq(
      $.subFunctionFormal, repeatseq(',', $.subFunctionFormal)
    )),
    subFunctionFormal: $ => prec.right(choice(
      // NOTE: In subfunctions bsc can do type inference of arguments/return types.
      $.identifier,
      prec.left(seq($.type, $.identifier)),
      // NOTE: A function can take another function as parameter.
      $.functionProto
    )),

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
      // NOTE: We created a a new subFunctionDef rule, because the bsc compiler
      //       treats function definitions inside other functions differently.
      $.subFunctionDef,
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
      // NOTE: An identifier can be qualified.
      prec.left(seq($.identifier, '::', $.identifier)),
      $.identifier,
      $.intLiteral,
      $.realLiteral,
      $.stringLiteral,
      // NOTE: boolLiteral seems to be missing from the spec in this rule. 
      $.boolLiteral,
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
      // NOTE: Apparrently, bsc allows type hints with the below syntax.
      prec.right(seq($.type, "'", $.expression)),
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
      choice($.exprPrimary, '\\∘'),
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
      $.identifier, '{', 
        $.memberBind, repeatseq(',', $.memberBind), 
      '}'
    )),
    memberBind: $ => prec.left(seq($.identifier, ':', $.expression)),

    taggedUnionExpr: $ => choice(
      // NOTE: Contrary to spec, but to match the bsc implementation, we make curly brackets optional if no binds.
      prec.left(seq('tagged', $.identifier, '{', $.memberBind, repeatseq(',', $.memberBind), '}')),
      prec.left(seq('tagged', $.identifier, $.exprPrimary)),
      prec.left(seq('tagged', $.identifier)),
    ),

    interfaceExpr: $ => prec.left(seq(
      // NOTE: We make the semicolon after the interface identifier optional.
      //       This is contrary to spec, but matches the behaviour of the bsc compiler.
      'interface', $.identifier, optional(';'),
        repeat($.interfaceStmt), 
      'endinterface', optseq(':', $.identifier)
    )),

    rulesExpr: $ => prec.left(seq(
      optional($.attributeInstances), 
      'rules', optseq(':', $.identifier), 
        // NOTE: The spec wrongly allows only one rule.
        repeat1($.rulesStmt),
      'endrules', optseq(':', $.identifier)
      )),
    rulesStmt: $ => choice(
      $.rule, 
      $.expressionStmt
    ),

    // ================================================================
    // sec 11 Pattern matching

    // NOTE: We allow optional parens around a pattern.
    pattern: $ => prec.left(choice(
      $._pattern,
      seq('(', $._pattern, ')')
    )),
    _pattern: $ => choice(
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
      $.identifier,
    ),
    taggedUnionPattern: $ => choice(
      prec.left(seq(
        'tagged', $.identifier, optional($.pattern)
      )),
      // NOTE: The spec is missing the case of a struct union member.
      prec.left(seq(
        'tagged', $.structPattern
      )),
    ),
    structPattern: $ => prec.left(seq(
      $.identifier, '{', optseq($.identifier, ':', $.pattern,
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

    attributeInstances: $ => prec.left(repeat1($.attributeInstance)),
    attributeInstance: $ => prec.right(seq(
      '(*', $.attrSpec, repeatseq(',', $.attrSpec), '*)'
    )),
    // Could be maybe more specific here than just "identifier"; maybe list all the possible attr names.
    attrSpec: $ => prec.left(seq($.identifier, optseq('=', $.expression))),
    

    // ============================================================
    // sec 15 Embedding RTL in a BSV design
    externModuleImport: $ => seq(
      'import', '"BVI"', optseq($.identifier, '='), $.moduleProto,
        repeat($.moduleStmt),
        repeat($.importBVIStmt),
      'endmodule', optseq(':', $.identifier)
      ),

    importBVIStmt: $ => choice(
      $.parameterBVIStmt,
      $.methodBVIStmt,
      $.portBVIStmt,
      $.inputClockBVIStmt,
      $.defaultClockBVIStmt,
      $.outputClockBVIStmt,
      $.inputResetBVIStmt,
      $.defaultResetBVIStmt,
      // NOTE: This is missing from the spec.
      // $.noResetBVIStmt,
      $.outputResetBVIStmt,
      $.ancestorBVIStmt,
      $.sameFamilyBVIStmt,
      $.scheduleBVIStmt,
      $.pathBVIStmt,
      $.interfaceBVIStmt,
      $.inoutBVIStmt,
    ),

    parameterBVIStmt: $ => seq('parameter', $.identifier, '=', $.expression, ';'),

    methodBVIStmt: $ => seq(
      'method', optional($.portId), $.identifier, optseq('(', optseq($.portId, repeatseq(',', $.portId)), ')'),
        optseq('enable', '(', $.portId, ')'),
        optseq('ready', '(', $.portId, ')'),
        optseq('clocked_by', '(', $.clockId, ')'),
        optseq('reset_by', '(', $.resetId, ')'),
      ';'
    ),

    portBVIStmt: $ => seq(
      'port', $.identifier, 
        optseq('clocked_by', '(', $.clockId, ')'),
        optseq('reset_by', '(', $.resetId, ')'), '=', $.expression, ';'
    ),

    inputClockBVIStmt: $ => seq(
      'input_clock', optional($.identifier), '(', optional($.portsDef), ')', 
      choice('=', '<-'), $.expression, ';'
    ),
    portsDef: $ => seq(
      $.portId, optseq(',', optional($.attributeInstances), $.portId)
    ),

    defaultClockBVIStmt: $ => prec.right(choice(
      seq('default_clock', $.identifier, ';'),
      seq('default_clock', optional($.identifier), 
          optseq('(', $.portsDef, ')'), optseq(choice('=', '<-'), $.expression), ';')
    )),

    outputClockBVIStmt: $ => seq('output_clock', $.identifier, '(', optional($.portsDef), ')', ';'),

    inputResetBVIStmt: $ => seq(
      'input_reset', optional($.identifier), optseq('(', $.portId, ')'), 
                     optseq('clocked_by', '(', $.clockId, ')'), 
      choice('=', '<-'), $.expression, ';'
    ),
    
    defaultResetBVIStmt: $ => choice(
      seq('default_reset', $.identifier, ';'),
      seq('default_reset', optional($.identifier), optseq('(', $.portId, ')'), 
          optseq('clocked_by', '(', $.clockId, ')'), optseq('=', $.expression), ';')
    ),

    outputResetBVIStmt: $ => seq(
      'output_reset', $.identifier, 
                      optseq('(', $.portId, ')'), 
                      optseq('clocked_by', '(', $.clockId, ')'), ';'
    ),

    ancestorBVIStmt: $ => seq('ancestor', '(', $.clockId, ',', $.clockId, ')', ';'),
    sameFamilyBVIStmt: $ => seq('same_family', '(', $.clockId, ',', $.clockId, ')', ';'),

    scheduleBVIStmt: $ => seq(
      'schedule', '(', $.identifier, repeatseq(',', $.identifier), ')', $.operatorId,
      '(', $.identifier, repeatseq(',', $.identifier), ')', ';'
    ),
    operatorId: $ => choice('CF', 'SB', 'SBR', 'C'),
    
    pathBVIStmt: $ => seq('path', '(', $.portId, ',', $.portId, ')', ';'),

    interfaceBVIStmt: $ => seq(
      'interface', $.typeDefType, ';',
        repeat($.interfaceBVIMembDecl),
      'endinterface', optseq(':', $.typeIde)
    ),

    interfaceBVIMembDecl: $ => choice(
      $.methodBVIStmt, 
      seq($.interfaceBVIStmt, ';')
    ),

    inoutBVIStmt: $ => choice(
      seq('inout', $.portId, optseq('clocked_by', '(', $.clockId, ')'),
          optseq('reset_by', '(', $.resetId, ')'), '=', $.expression, ';'),
      seq('ifc_inout', $.identifier, '(', $.inoutId, ')', optseq('clocked_by', '(', $.clockId, ')'),
          optseq('reset_by', '(', $.resetId, ')'), ';')
    ),

    portId: $ => $.identifier,
    clockId: $ => $.identifier,
    resetId: $ => $.identifier,
    inoutId: $ => $.identifier,

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
  
    // Identifier starting with any case. See top-of-file comment on why we do not have upper case Identifier.
    // \p{L} matches any character in the unicode category. \p{Lu} for upper case unicode letter.
    // For Unicode in regex, see https://www.regular-expressions.info/unicode.html
    identifier: $ => token(/[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*/), 

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

    [$.exprOrCondPattern, $.exprOrCondPattern],
    [$.exprPrimary, $.structExpr],
    [$.exprOrCondPattern, $.exprPrimary],
    [$.actionStmt, $.fsmStmt],
    [$.typeIde, $.structExpr],
    [$.tupleBind, $.exprPrimary],
    [$.moduleStmt, $.expression],
    [$.moduleStmt, $.exprPrimary],
    [$.exprPrimary, $.actionValueStmt],
    [$.expressionStmt, $.functionBodyStmt],
    [$.typeIde, $.subinterfaceDef],
    [$.typeIde, $.subinterfaceDef, $.interfaceExpr],
    [$.typeIde, $.interfaceExpr],
    [$.subFunctionType, $.typeIde],
    [$.functionType, $.subFunctionType],
    [$.functionFormal, $.subFunctionFormal],
    [$.typePrimary, $.exprPrimary],
    [$.expression, $.bitSelect],
    [$.typeIde, $.lValue],
    [$.typeIde, $.lValue, $.exprPrimary],
    [$.typeIde, $.methodDef, $.methodBVIStmt],
    [$.typeIde, $.portId],
    [$.methodFormal, $.portId],
    [$.type, $.typeFormal],

  ]

});
