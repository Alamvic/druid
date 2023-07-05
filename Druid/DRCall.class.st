Class {
	#name : #DRCall,
	#superclass : #DRInstruction,
	#instVars : [
		'mapped',
		'functionName'
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

{ #category : #accessing }
DRCall >> functionName [
	^ functionName
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

{ #category : #printing }
DRCall >> operands: aCollection [

	functionName := aCollection first.
	super operands: aCollection allButFirst
]

{ #category : #SCCP }
DRCall >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]
