Class {
	#name : #DRBytecodeJump,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRBytecodeJump >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBytecodeJump: self
]

{ #category : #testing }
DRBytecodeJump >> isMandatoryInstruction [

	^ true
]

{ #category : #SCCP }
DRBytecodeJump >> opcode [
	
	^ 'BytecodeJump'
]

{ #category : #SCCP }
DRBytecodeJump >> sccpLatticeValueFor: sccp [ 
	
	"We know that we know nothing about what is in the stack"
	^ sccp bottom
]
