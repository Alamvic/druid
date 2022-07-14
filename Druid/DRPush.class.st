Class {
	#name : #DRPush,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #visiting }
DRPush >> acceptVisitor: aVisitor [

	^ aVisitor visitPush: self
]

{ #category : #testing }
DRPush >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRPush >> opcode [
	
	^ 'Push'
]

{ #category : #SCCP }
DRPush >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
