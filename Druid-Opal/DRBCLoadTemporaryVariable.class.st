Class {
	#name : 'DRBCLoadTemporaryVariable',
	#superclass : 'DRBCTemporaryVariableInstruction',
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBCLoadTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadTemporaryVariable: self
]

{ #category : 'initialization' }
DRBCLoadTemporaryVariable >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : 'testing' }
DRBCLoadTemporaryVariable >> isLoadTemporaryVariable [

	^ true
]

{ #category : 'printing' }
DRBCLoadTemporaryVariable >> opcode [
	
	^ 'LoadTemp'
]

{ #category : 'SCCP' }
DRBCLoadTemporaryVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'types' }
DRBCLoadTemporaryVariable >> typeWithAlreadySeen: aCollection [

	^ DRUndefinedType new
]
