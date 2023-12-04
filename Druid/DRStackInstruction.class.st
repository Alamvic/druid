Class {
	#name : #DRStackInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'stackDependency'
	],
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #testing }
DRStackInstruction >> isStackInstruction [

	^ true
]

{ #category : #accessing }
DRStackInstruction >> stackDependency [

	^ stackDependency
]

{ #category : #accessing }
DRStackInstruction >> stackDependency: anObject [

	stackDependency := anObject
]
