Class {
	#name : #DRSpecialisationCompilerTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSpecialisationCompilerTest >> setUp [

	super setUp.
	self configureInterpreter
]

{ #category : #tests }
DRSpecialisationCompilerTest >> testArrayConditionalCheck [

	| cfg specialisationCheck |
	compilerCompiler specialisation: DRArrayPrimitiveSpecialisation new.
	cfg := self generateDruidIRFor: #primitiveAt.
	specialisationCheck := cfg allConditionalJumps first: 2.

	"Check for immediate"
	self assert: specialisationCheck first operand1 name equals: 'ReceiverResultReg'.
	self assert: specialisationCheck first operand2 value equals: 2r111.

	"Check for Array format"
	self assert: specialisationCheck second operand1 class equals: DRBitAnd. "Hard assertion over expressions tree"
	self assert: specialisationCheck second operand2 value equals: 2.

]

{ #category : #tests }
DRSpecialisationCompilerTest >> testArrayInitialConstraints [

	| cfg firstBlockAfterChecks edgeAfterChecks |
	compilerCompiler specialisation: DRArrayPrimitiveSpecialisation new.
	cfg := self generateDruidIRFor: #primitiveAt.

	firstBlockAfterChecks := cfg reversePostOrderBlocks at: 4. "initial + check immendiate + check format -> primitive"
	edgeAfterChecks := self anyIncomingEdge: cfg for: firstBlockAfterChecks.

	"Receiver oop is not an immediate"
	self
		assertConstraintIn: edgeAfterChecks
		for: 'ReceiverResultReg'
		includes: (DRNoneMaskConstraint withValue: 2r111).
	"Format is =2"
	self
		assertConstraintIn: edgeAfterChecks
		for: 'R3'
		includes: (DREqualsConstraint withValue: 2)
]

{ #category : #tests }
DRSpecialisationCompilerTest >> testSmallIntegerConditionalCheck [

	| cfg specialisationCheck |
	cfg := self generateDruidIRFor: #primitiveAdd.
	specialisationCheck := cfg allConditionalJumps first.

	self assert: specialisationCheck condition isEqualsThanComparison.
	self assert: specialisationCheck operand1 simpleConstantFold equals: 1.
	self assert: specialisationCheck operand2 expression equals: 'receiverTags'
]

{ #category : #tests }
DRSpecialisationCompilerTest >> testSmallIntegerInitialConstraints [

	| cfg firstBlockAfterChecks edgeAfterChecks |
	cfg := self generateDruidIRFor: #primitiveAdd.

	firstBlockAfterChecks := cfg reversePostOrderBlocks at: 3. "initial + check integer -> primitive"
	edgeAfterChecks := self anyIncomingEdge: cfg for: firstBlockAfterChecks.

	self
		assertConstraintIn: edgeAfterChecks
		for: 'ReceiverResultReg'
		includes: (DREqualsConstraint withValue: 2r1)
]
