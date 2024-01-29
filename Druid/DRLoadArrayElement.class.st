Class {
	#name : #DRLoadArrayElement,
	#superclass : #DRInstruction,
	#instVars : [
		'executionState'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLoadArrayElement >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadArrayElement: self
]

{ #category : #'as yet unclassified' }
DRLoadArrayElement >> arrayOperand [

	^ self operands first
]

{ #category : #accessing }
DRLoadArrayElement >> executionState [
	^ executionState
]

{ #category : #accessing }
DRLoadArrayElement >> executionState: aDRExecutionState [ 
	
	executionState := aDRExecutionState
]

{ #category : #'as yet unclassified' }
DRLoadArrayElement >> indexOperand [

	^ self operands second
]

{ #category : #testing }
DRLoadArrayElement >> isLoadArrayElement [
	
	^ true
]

{ #category : #printing }
DRLoadArrayElement >> opcode [
	
	^ 'LoadArrayElement'
]

{ #category : #SCCP }
DRLoadArrayElement >> sccpLatticeValueFor: sccp [

	^ sccp bottom
]

{ #category : #types }
DRLoadArrayElement >> type [

	^ DRUndefinedType new
]
