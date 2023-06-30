Class {
	#name : #DRCall,
	#superclass : #DRInstruction,
	#instVars : [
		'mapped'
	],
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #visiting }
DRCall >> acceptVisitor: aVisitor [

	^ aVisitor visitCall: self
]

{ #category : #properties }
DRCall >> beMapped [
	
	mapped := true
]

{ #category : #testing }
DRCall >> initialize [

	super initialize.
	mapped := false
]

{ #category : #testing }
DRCall >> isMandatoryInstruction [

	^ true
]

{ #category : #testing }
DRCall >> isMapped [
	
	^ mapped
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
