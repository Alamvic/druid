Class {
	#name : #DRStoreTemporaryVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRStoreTemporaryVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitStoreTempVar: self
]

{ #category : #testing }
DRStoreTemporaryVariable >> isMandatoryInstruction [ 

	^ true
]

{ #category : #printing }
DRStoreTemporaryVariable >> opcode [
	
	^ 'StoreTemp'
]

{ #category : #SCCP }
DRStoreTemporaryVariable >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
