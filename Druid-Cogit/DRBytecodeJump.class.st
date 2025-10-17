Class {
	#name : 'DRBytecodeJump',
	#superclass : 'DRInstruction',
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBytecodeJump >> acceptVisitor: aVisitor [
	
	^ aVisitor visitBytecodeJump: self
]

{ #category : 'testing' }
DRBytecodeJump >> isBytecodeJump [

	^ true
]

{ #category : 'testing' }
DRBytecodeJump >> isMandatoryInstruction [

	^ true
]

{ #category : 'testing' }
DRBytecodeJump >> needsStackFlush [

	"We need to guarantee that if we arrive to the target from this path, everything is in memory"
	^ true
]

{ #category : 'SCCP' }
DRBytecodeJump >> opcode [
	
	^ 'BytecodeJump'
]

{ #category : 'SCCP' }
DRBytecodeJump >> sccpLatticeValueFor: sccp [ 
	
	"We know that we know nothing about what is in the stack"
	^ sccp bottom
]
