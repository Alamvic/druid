Class {
	#name : #DRPopMany,
	#superclass : #DRStackInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRPopMany >> acceptVisitor: aVisitor [ 
	
	aVisitor visitPopMany: self
]

{ #category : #testing }
DRPopMany >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRPopMany >> isPop [

	^ true
]

{ #category : #accessing }
DRPopMany >> numberOfPoppedElements [
	
	^ self operand1 value
]

{ #category : #printing }
DRPopMany >> opcode [
	
	^ 'PopN'
]

{ #category : #SCCP }
DRPopMany >> sccpLatticeValueFor: sccp [ 
	
	"We know that we know nothing about what is in the stack"
	^ sccp bottom
]
