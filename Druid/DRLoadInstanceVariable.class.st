Class {
	#name : #DRLoadInstanceVariable,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadInstanceVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadInstVar: self
]

{ #category : #accessing }
DRLoadInstanceVariable >> index [

	^ self originAST variable index
]

{ #category : #testing }
DRLoadInstanceVariable >> isLoadInstanceVariable [

	^ true
]

{ #category : #printing }
DRLoadInstanceVariable >> opcode [
	
	^ 'LoadInstVar'
]

{ #category : #SCCP }
DRLoadInstanceVariable >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #types }
DRLoadInstanceVariable >> type [

	^ DRUndefinedType new
]
