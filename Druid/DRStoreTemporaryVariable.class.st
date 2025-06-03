Class {
	#name : 'DRStoreTemporaryVariable',
	#superclass : 'DRInstruction',
	#instVars : [
		'vectorName'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRStoreTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreTempVar: self
]

{ #category : 'accessing' }
DRStoreTemporaryVariable >> address [
	
	^ self operand1
]

{ #category : 'testing' }
DRStoreTemporaryVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRStoreTemporaryVariable >> isStore [ 

	^ true
]

{ #category : 'testing' }
DRStoreTemporaryVariable >> isStoreTemporaryVariable [

	^ true
]

{ #category : 'testing' }
DRStoreTemporaryVariable >> isTempVectorTemp [ 

	^ false
]

{ #category : 'printing' }
DRStoreTemporaryVariable >> opcode [
	
	^ 'StoreTemp'
]

{ #category : 'SCCP' }
DRStoreTemporaryVariable >> sccpLatticeValueFor: sccp [ 
	
	^ sccp bottom  "latticeValue: operands second"
]

{ #category : 'accessing' }
DRStoreTemporaryVariable >> variableName [

	^ self operand1 value
]

{ #category : 'accessing' }
DRStoreTemporaryVariable >> vectorName [

	^ vectorName
]

{ #category : 'accessing' }
DRStoreTemporaryVariable >> vectorName: anObject [

	vectorName := anObject
]
