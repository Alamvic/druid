Class {
	#name : #DRNoop,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNoop >> acceptVisitor: aVisitor [

	^ aVisitor visitNoop: self
]

{ #category : #initialization }
DRNoop >> initialize [

	super initialize.
	result := DRNoRegister new
]

{ #category : #SCCP }
DRNoop >> isNoop [

	^ true
]

{ #category : #printing }
DRNoop >> opcode [
	
	^ 'Noop'
]

{ #category : #visiting }
DRNoop >> removeFromCFG [

	"If I am the ending instruction of a block, keep me around"
	self basicBlock endInstruction = self
		ifTrue: [ ^ self ].
	
	super removeFromCFG
]

{ #category : #SCCP }
DRNoop >> sccpLatticeValueFor: sccp [ 
	
	^ sccp top
]
