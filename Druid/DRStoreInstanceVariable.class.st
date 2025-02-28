Class {
	#name : 'DRStoreInstanceVariable',
	#superclass : 'DRInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRStoreInstanceVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreInstVar: self
]

{ #category : 'testing' }
DRStoreInstanceVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRStoreInstanceVariable >> isStore [ 

	^ true
]

{ #category : 'printing' }
DRStoreInstanceVariable >> opcode [
	
	^ 'StoreInstVar'
]

{ #category : 'SCCP' }
DRStoreInstanceVariable >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
