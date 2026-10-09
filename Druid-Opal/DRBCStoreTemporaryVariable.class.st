Class {
	#name : 'DRBCStoreTemporaryVariable',
	#superclass : 'DRBCTemporaryVariableInstruction',
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'as yet unclassified' }
DRBCStoreTemporaryVariable class >> loadInstructionKind [

	^ DRBCLoadTemporaryVariable 
]

{ #category : 'visiting' }
DRBCStoreTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreTempVar: self
]

{ #category : 'testing' }
DRBCStoreTemporaryVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRBCStoreTemporaryVariable >> isStore [ 

	^ true
]

{ #category : 'testing' }
DRBCStoreTemporaryVariable >> isStoreTemporaryVariable [

	^ true
]

{ #category : 'printing' }
DRBCStoreTemporaryVariable >> opcode [
	
	^ 'StoreTemp'
]

{ #category : 'SCCP' }
DRBCStoreTemporaryVariable >> sccpLatticeValueFor: sccp [ 
	
	^ sccp bottom  "latticeValue: operands second"
]

{ #category : 'accessing' }
DRBCStoreTemporaryVariable >> storeValue [

	^ self operand1
]

{ #category : 'types' }
DRBCStoreTemporaryVariable >> typeWithAlreadySeen: aCollection [

	self error: 'Not implemented yet'
]
