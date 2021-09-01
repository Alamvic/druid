Class {
	#name : #DRMultiply,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRMultiply >> acceptVisitor: aVisitor [
	
	^ aVisitor visitMultiply: self
]

{ #category : #printing }
DRMultiply >> opcode [
	
	^ 'MUL'
]

{ #category : #visiting }
DRMultiply >> sccpLatticeValueFor: sccp [

	| operand1LatticeValue operand2LatticeValue operand1IsConstant operand2IsConstant nonConstantLattices |
	operand1LatticeValue := sccp latticeValue: self operand1.
	operand2LatticeValue := sccp latticeValue: self operand2.
	
	nonConstantLattices := { sccp top. sccp bottom }.
	operand1IsConstant := (nonConstantLattices includes: operand1LatticeValue) not and: [
		operand1LatticeValue isConstant ].

	operand2IsConstant := (nonConstantLattices includes: operand2LatticeValue) not and: [ 
		operand2LatticeValue isConstant ].

	"If one of the operands is zero, the multiplication result is zero regardless the other operand"
	(operand1IsConstant and: [ operand1LatticeValue value = 0 ]) ifTrue: [ ^ operand1LatticeValue ].
	(operand2IsConstant and: [ operand2LatticeValue value = 0 ]) ifTrue: [ ^ operand2LatticeValue ].
	
	((operand1IsConstant and: [ operand2IsConstant ])) ifTrue: [ 
		^ operand1LatticeValue value * operand2LatticeValue value ].
	
	(operand1LatticeValue = sccp bottom or: [ operand2LatticeValue = sccp bottom ]) ifTrue: [ 
		^ sccp bottom ].
	^ sccp top
]
