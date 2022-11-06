Class {
	#name : #DRPop,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #visiting }
DRPop >> acceptVisitor: aVisitor [

	^ aVisitor visitPop: self
]

{ #category : #testing }
DRPop >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRPop >> opcode [
	
	^ 'Pop'
]

{ #category : #SCCP }
DRPop >> sccpLatticeValueFor: sccp [ 
	
	"We know that we know nothing about what is in the stack"
	^ sccp bottom
]

{ #category : #SCCP }
DRPop >> size [
	"Popped memory size"
	
	^ 8 "bytes"
]
