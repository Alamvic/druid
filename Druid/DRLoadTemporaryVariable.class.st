Class {
	#name : 'DRLoadTemporaryVariable',
	#superclass : 'DRTemporaryVariableInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitTemporaryVariable: self
]

{ #category : 'accessing' }
DRLoadTemporaryVariable >> address [
	^ self operand1
]

{ #category : 'initialization' }
DRLoadTemporaryVariable >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : 'testing' }
DRLoadTemporaryVariable >> isLoadTemporaryVariable [

	^ true
]

{ #category : 'printing' }
DRLoadTemporaryVariable >> opcode [
	
	^ 'LoadTemp'
]

{ #category : 'SCCP' }
DRLoadTemporaryVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'types' }
DRLoadTemporaryVariable >> type [

	^ DRUndefinedType new
]
