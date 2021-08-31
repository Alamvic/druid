Class {
	#name : #DRReturn,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRReturn >> acceptVisitor: aVisitor [
	
	^ aVisitor visitReturn: self
]

{ #category : #visiting }
DRReturn >> isValidEndInstruction [

	^ true
]

{ #category : #printing }
DRReturn >> opcode [
	
	^ 'Return'
]

{ #category : #SCCP }
DRReturn >> sccpEvaluateFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
