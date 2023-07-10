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

	| cfg expectedConstraint |
	cfg := self generateDruidIRFor: #primitiveAdd.
	expectedConstraint := DRSmallIntegerPrimitiveSpecialisation new initialConstraint.
	
	self assertConstraint: cfg initialContrainst includes: expectedConstraint
]
