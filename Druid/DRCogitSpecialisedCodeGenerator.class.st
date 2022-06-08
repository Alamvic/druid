Class {
	#name : #DRCogitSpecialisedCodeGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #'as yet unclassified' }
DRCogitSpecialisedCodeGenerator >> generatePreambleForCFG: aDRControlFlowGraph [

	| preamble |
	preamble := DruidTestRTLCompiler lookupSelector: #preambleForSmallInteger.

	generatorMethodBuilder addStatement: preamble ast body copy.

	super generatePreambleForCFG: aDRControlFlowGraph
]
