package tree_sitter_bsv_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_bsv "github.com/robertszafa/tree-sitter-bsv/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_bsv.Language())
	if language == nil {
		t.Errorf("Error loading Bluespec System Verilog grammar")
	}
}
