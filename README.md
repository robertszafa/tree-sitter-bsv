# tree-sitter-bsv

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammar for [Bluespec System Verilog (BSV)](https://github.com/B-Lang-org/bsc/tree/main).


### Notes

We mostly used the [BSV language reference manual](https://github.com/B-Lang-org/bsc/releases/latest/download/BSV_lang_ref_guide.pdf) as reference for the grammar rules. Where we deviate or add additional grammar rules this is to match the behaviour of the bsc compiler, which not always enforces the spec, or to fix errors in the spec (there are a few). The deviations are explained in `NOTE` comments in the [grammar.js](https://github.com/robertszafa/tree-sitter-bsv/blob/main/grammar.js) file.

