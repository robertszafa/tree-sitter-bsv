'''
This script checks if the parse errors caught by bsc are also caught by our parser.
We run our parser on the bsv files from the bsc github repo:
    https://github.com/B-Lang-org/bsc/tree/main/testsuite/bsc.syntax/bsv05
'''

import os

TESTSUITE_PATH = dir_path = os.path.dirname(os.path.realpath(__file__))

def check_file_passes(fname):
    TMP_FILE = f'{TESTSUITE_PATH}/.parse.out'
    os.system(f'cd {TESTSUITE_PATH}/../.. && tree-sitter parse {TESTSUITE_PATH}/{fname} > {TMP_FILE} 2>&1')

    with open(TMP_FILE, 'r') as f:
        out_lines = f.readlines()
        for line in out_lines:
            if 'ERROR' in line or 'MISSING' in line:
                return False

    os.system(f'rm -f {TMP_FILE}')

    return True

def check_should_pass(fname):
    """
    Run the basc compiler and grep for "Error:"
    """
    TMP_FILE = f'{TESTSUITE_PATH}/.parse.out'
    os.system(f'cd {TESTSUITE_PATH}/../.. && bsc {TESTSUITE_PATH}/{fname} > {TMP_FILE} 2>&1')

    with open(TMP_FILE, 'r') as f:
        out_lines = f.readlines()
        for line in out_lines:
            if 'Error: ' in line:
                return False

    os.system(f'rm -f {TMP_FILE}')

    return True


files_with_parse_error = [
    'TypedefTaggedUnionEmpty.bsv',
    'ImportSome.bsv',
    'ImperativeModuleBodyEnd.bsv',
    'InterfaceProvisos.bsv',
    'UnterminatedBlockComment.bsv',
    'ImperativeModuleInstanceTuple.bsv',
    'ImperativeFunctionCaseEarlyDefaultEq.bsv',
    'Latin1Code.bsv',
    'NumberMixed.bsv',
    'ImperativeModuleInterfaceSubinterfaceNoTag.bsv',
    'NumberRepeatedX.bsv',
    'NonOptionalMethodTypes.bsv',
    'TypedefStructStruct.bsv',
    'ImperativeFunctionBeginEndMissingReturn.bsv',
    'ImperativeActionFunctionReturn.bsv',
    'ImperativeFunctionForUndeclaredVar.bsv',
    'FunctionNoType.bsv',
    'EmptyExprBlock.bsv',
    'RuleEmptyConditionParens.bsv',
    'EmptyExprBlock.bsv',
    'RuleEmptyConditionParens.bsv',
    'ImportNothingNothing.bsv',
]

# These files have type errors, or other semantic logic errors catched by bsc. 
# But the syntax respects the BSV grammar.
bad_files_without_parse_error = [
    'BitConcatBad.bsv',
    'IntTypeBad.bsv',
    # We do not check upper case of types:
    'UTF8BadCons1.bsv',
    'UTF8BadCons2.bsv',
    # The error checking for the below are context sensitive
    # This also includes imports of files that bsc cannot find.
    'ImportNothingNothing.bsv', 
    'Counter2_Reg.bsv',
    'ImperativeFunctionUndeclEq.bsv',
    'ImperativeTupleEq_Unbound.bsv',
    'ImperativeTupleEq_WrongType.bsv',
    'TypeclassDefaultImport.bsv',
    'Shifter_pipe_elastic.bsv',
    'Counter2_CReg.bsv',
    'ImperativeFunctionUnassignedUse.bsv',
    'InterfaceArgs.bsv',
    'Shifter_pipe_rigid.bsv',
    'Shifter_iterative.bsv',
    'ExportAllImport.bsv',
    'Latin1LineComment.bsv',
    'ImperativeActionFunctionNameAssign.bsv',
    'ImperativeTupleDecl.bsv',
    'ImportNothingNothing.bsv',
    'ImperativeFunctionDeclEqRecursive.bsv',
    'Counter2.bsv',
    'Shifter.bsv',
    'ImperativeActionUndeclEq.bsv',
    'BitConcatBad.bsv',
    'Bubblesort.bsv',
    'FunctionBrokenEnd.bsv',
    'IntTypeBad.bsv',
    'Latin1MultilineComment.bsv',
    'ImperativeActionDeclEqRecursive.bsv',
    'PackageEmptyWrongTail.bsv',
    'Testbench.bsv',
    'ExportSomeImport.bsv',
    'ImperativeTupleBind_WrongType.bsv',
    'ImperativeActionUnassignedUse.bsv',
]

if __name__ == "__main__":
    num_ran = 0
    num_passed = 0
    for f in os.listdir(TESTSUITE_PATH):
        if ".bsv" in f:
            if f in bad_files_without_parse_error:
                # These are not parse errors, but type errors, wrong input errors, etc.
                continue 
            should_pass = check_should_pass(f)
            # should_pass = (all([file_bad_substr.lower() not in f.lower() for file_bad_substr in bad_file_substrings]) and \
            #                all([bad_file.lower() != f.lower() for bad_file in files_with_parse_error])) or \
            #               any([correct_parse_file.lower() in f.lower() for correct_parse_file in bad_files_without_parse_error])
            num_ran += 1
            did_pass = check_file_passes(f)
            if (did_pass and should_pass) or (not did_pass and not should_pass):
                num_passed += 1
            else:
                if should_pass:
                    print(f"Error in correct file: {f}")
                else:
                    print(f"File parsed correctly, but should fail: {f}")

    print(f"num tests  = {num_ran}")
    print(f"num passed = {num_passed}")

