Class {
	#name : #DRLoadInstVar,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadInstVar >> acceptVisitor: aVisitor [

	aVisitor visitLoadInstVar: self
]

{ #category : #accessing }
DRLoadInstVar >> index [

	^ self originAST variable index
]

{ #category : #printing }
DRLoadInstVar >> opcode [
	
	^ 'LoadInstVar'
]

{ #category : #SCCP }
DRLoadInstVar >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #types }
DRLoadInstVar >> type [

	^ DRUndefinedType new
]
