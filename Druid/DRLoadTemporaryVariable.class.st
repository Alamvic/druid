Class {
	#name : #DRLoadTemporaryVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitTemporaryVariable: self
]

{ #category : #initialization }
DRLoadTemporaryVariable >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : #testing }
DRLoadTemporaryVariable >> isLoadTemporaryVariable [

	^ true
]

{ #category : #printing }
DRLoadTemporaryVariable >> opcode [
	
	^ 'LoadTemp'
]

{ #category : #'cogit-generation' }
DRLoadTemporaryVariable >> rtlOperandQualifier [
	
	^ result rtlOperandQualifier
]

{ #category : #SCCP }
DRLoadTemporaryVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #accessing }
DRLoadTemporaryVariable >> variableName [

	^ self operand1 value
]
