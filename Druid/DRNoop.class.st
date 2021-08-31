Class {
	#name : #DRNoop,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNoop >> acceptVisitor: aVisitor [

	^ aVisitor visitNoop: self
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

	"Do nothing for now"
]

{ #category : #SCCP }
DRNoop >> sccpEvaluateFor: sccp [ 
	
	^ sccp top
]
