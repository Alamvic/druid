Class {
	#name : #DRLoadTemporaryVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadTemporaryVariable >> acceptVisitor: aVisitor [

	aVisitor visitTemporaryVariable: self
]

{ #category : #printing }
DRLoadTemporaryVariable >> opcode [
	
	^ 'LoadTemp'
]

{ #category : #SCCP }
DRLoadTemporaryVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #types }
DRLoadTemporaryVariable >> type [

	^ DRUndefinedType new
]
