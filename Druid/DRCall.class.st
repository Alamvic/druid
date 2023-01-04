Class {
	#name : #DRCall,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRCall >> acceptVisitor: aVisitor [

	^ aVisitor visitCall: self
]

{ #category : #testing }
DRCall >> isMandatoryInstruction [

	^ true
]

{ #category : #printing }
DRCall >> opcode [
	
	^ 'Call'
]

{ #category : #accessing }
DRCall >> operands [

	^ #()
]

{ #category : #SCCP }
DRCall >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
