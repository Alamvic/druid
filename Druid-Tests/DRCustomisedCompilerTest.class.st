Class {
	#name : #DRCustomisedCompilerTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCustomisedCompilerTest >> setUp [

	super setUp.
	self configureInterpreter
]

{ #category : #tests }
DRCustomisedCompilerTest >> testArrayConditionalCheck [

	| cfg specialisationCheck |
	"
	Now that we have the specialisation in the CFG, it is self-affected by its own constraints.
	So, specialisation is clear because of optimisations of dead paths.
	Anyway, should be nice to have both possiblities:
	  - To include the checks in the IR -> compile a safe specialised version 
	  - To not include the checks -> compile an unsafe specialised version 
	"
	self skip.

	compilerCompiler specialisation: DRArrayPrimitiveSpecialisation new.
	cfg := self generateDruidIRFor: #primitiveAt.
	specialisationCheck := cfg allConditionalJumps first.

	self fail: 'Assertion missings'
	"
	Check receiver is not immediate, AND
	Check format
	"
]

{ #category : #tests }
DRCustomisedCompilerTest >> testArrayInitialConstraints [

	| cfg |
	compilerCompiler specialisation: DRArrayPrimitiveSpecialisation new.
	cfg := self generateDruidIRFor: #primitiveAt.

	self assert: cfg initialContrainst reg name equals: 'ReceiverResultReg'.
	self
		assertConstraint: cfg initialContrainst constraint
		includes: (DROopConstraint withType: Array)
]

{ #category : #tests }
DRCustomisedCompilerTest >> testSmallIntegerConditionalCheck [

	| cfg specialisationCheck |
	cfg := self generateDruidIRFor: #primitiveAdd.
	specialisationCheck := cfg allConditionalJumps first.

	self assert: specialisationCheck condition isEqualsThanComparison.
	self assert: specialisationCheck operand1 simpleConstantFold equals: 1.
	self assert: specialisationCheck operand2 expression equals: 'receiverTags'
]

{ #category : #tests }
DRCustomisedCompilerTest >> testSmallIntegerInitialConstraints [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveAdd.

	self assert: cfg initialContrainst reg name equals: 'ReceiverResultReg'.
	self
		assertConstraint: cfg initialContrainst constraint
		includes: (DRAnyMaskConstraint withValue: 1)
]
