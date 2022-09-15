Class {
	#name : #DRMod,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #printing }
DRMod >> acceptVisitor: aVisitor [

	^ aVisitor visitMod: self
]

{ #category : #printing }
DRMod >> isModulo [
	
	^ true
]

{ #category : #printing }
DRMod >> opcode [
	
	^ 'MOD'
]

{ #category : #types }
DRMod >> type [

	^ DRIntegerType new
]
