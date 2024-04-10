Class {
	#name : #DRFlushStack,
	#superclass : #DRInstruction,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRFlushStack >> acceptVisitor: aVisitor [ 

	^ aVisitor visitFlushStack: self
]

{ #category : #testing }
DRFlushStack >> isFlushStack [

	^ true
]

{ #category : #visiting }
DRFlushStack >> isMandatoryInstruction [

	^ true
]

{ #category : #SCCP }
DRFlushStack >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
