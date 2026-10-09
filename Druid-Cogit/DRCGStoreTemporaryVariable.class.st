Class {
	#name : 'DRCGStoreTemporaryVariable',
	#superclass : 'DRCGTemporaryVariableInstruction',
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'as yet unclassified' }
DRCGStoreTemporaryVariable class >> loadInstructionKind [

	^ DRCGLoadTemporaryVariable 
]

{ #category : 'visiting' }
DRCGStoreTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreTempVar: self
]

{ #category : 'testing' }
DRCGStoreTemporaryVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRCGStoreTemporaryVariable >> isStore [ 

	^ true
]

{ #category : 'testing' }
DRCGStoreTemporaryVariable >> isStoreTemporaryVariable [

	^ true
]

{ #category : 'printing' }
DRCGStoreTemporaryVariable >> opcode [
	
	^ 'StoreTemp'
]

{ #category : 'SCCP' }
DRCGStoreTemporaryVariable >> sccpLatticeValueFor: sccp [ 
	
	^ sccp bottom  "latticeValue: operands second"
]

{ #category : 'accessing' }
DRCGStoreTemporaryVariable >> storeValue [

	^ self operand2
]

{ #category : 'types' }
DRCGStoreTemporaryVariable >> typeWithAlreadySeen: aCollection [

	self error: 'Not implemented yet'
]
