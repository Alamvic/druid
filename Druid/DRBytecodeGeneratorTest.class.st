Class {
	#name : #DRBytecodeGeneratorTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #'as yet unclassified' }
DRBytecodeGeneratorTest >> generateMethodFor: aDRControlFlowGraph selector: selector [
	
	| generator |
	DRRegisterCollapse new applyTo: aDRControlFlowGraph.
	generator := DRBytecodeGenerator new.
	generator methodName: selector.
	generator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ generator targetAST
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testBasicControlFlow [

	| cfg method |
	cfg := DRControlFlowGraph new.

	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].

	method := self generateMethodFor: cfg selector: #m.
	1 halt
]
