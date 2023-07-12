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

{ #category : #accessing }
DRCall >> functionName: anObject [

	functionName := anObject
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

{ #category : #accessing }
DRCall >> mapped: anObject [

	mapped := anObject
]

{ #category : #testing }
DRCall >> needsStackFlush [

	"We need to guarantee that if we arrive to the runtime from this path, everything is in memory"
	^ true
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
