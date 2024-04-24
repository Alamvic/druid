Class {
	#name : #DRDivision,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRDivision >> acceptVisitor: aVisitor [

	^ aVisitor visitDivision: self
]

{ #category : #testing }
DRDivision >> isDivision [
	
	^ true
]

{ #category : #printing }
DRDivision >> opcode [
	
	^ 'DIV'
]

{ #category : #accessing }
DRDivision >> operands: aCollection [

	super operands: aCollection asOrderedCollection
]

{ #category : #SCCP }
DRDivision >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand // operand2
]
