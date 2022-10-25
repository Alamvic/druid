Class {
	#name : #DRStoreTemporaryVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #printing }
DRStoreTemporaryVariable >> opcode [
	
	^ 'StoreTemp'
]
