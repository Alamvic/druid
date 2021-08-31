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

{ #category : #SCCP }
DRNoop >> sccpEvaluateFor: sccp [ 
	
	^ sccp top
]
