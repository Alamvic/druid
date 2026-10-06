Class {
	#name : 'DRCGLoadTemporaryVariable',
	#superclass : 'DRCGTemporaryVariableInstruction',
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRCGLoadTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadTemporaryVariable: self
]

{ #category : 'initialization' }
DRCGLoadTemporaryVariable >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : 'testing' }
DRCGLoadTemporaryVariable >> isLoadTemporaryVariable [

	^ true
]

{ #category : 'printing' }
DRCGLoadTemporaryVariable >> opcode [
	
	^ 'LoadTemp'
]

{ #category : 'SCCP' }
DRCGLoadTemporaryVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : 'types' }
DRCGLoadTemporaryVariable >> typeWithAlreadySeen: aCollection [

	^ DRUndefinedType new
]
