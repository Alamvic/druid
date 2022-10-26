Class {
	#name : #DRReturn,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRReturn >> acceptVisitor: aVisitor [
	
	^ aVisitor visitReturn: self
]

{ #category : #initialization }
DRReturn >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : #visiting }
DRReturn >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRReturn >> isReturn [

	^ true
]

{ #category : #printing }
DRReturn >> opcode [
	
	^ 'Return'
]

{ #category : #SCCP }
DRReturn >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
