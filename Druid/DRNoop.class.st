Class {
	#name : #DRNoop,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNoop >> acceptVisitor: aVisitor [

	^ aVisitor visitNoop: self
]

{ #category : #printing }
DRNoop >> opcode [
	
	^ 'Noop'
]

{ #category : #SCCP }
DRNoop >> sccpEvaluateFor: sccp [ 
	
	^ sccp top
]
