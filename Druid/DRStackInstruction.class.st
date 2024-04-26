Class {
	#name : #DRStackInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'stackDependents',
		'stackDependencies'
	],
	#category : #'Druid-IR'
}

{ #category : #adding }
DRStackInstruction >> addStackDependent: aStackInstruction [ 

	stackDependents add: aStackInstruction
]

{ #category : #dependencies }
DRStackInstruction >> clearStackDependencies [

	stackDependencies := OrderedCollection new.
	stackDependents := OrderedCollection new
]

{ #category : #testing }
DRStackInstruction >> initialize [

	super initialize.

	self clearStackDependencies
]

{ #category : #testing }
DRStackInstruction >> isStackInstruction [

	^ true
]

{ #category : #removing }
DRStackInstruction >> removeStackDependency: aStackDependency [
	
	stackDependencies := stackDependencies copyWithout: aStackDependency
]

{ #category : #removing }
DRStackInstruction >> removeStackDependent: aStackDependent [ 
	
	stackDependents := stackDependents copyWithout: aStackDependent
]

{ #category : #dependencies }
DRStackInstruction >> shiftStackAccessBy: anInteger [ 
	
	"Delegate to my stack dependents"
	stackDependents do: [ :e | e shiftStackAccessBy: anInteger ].
]

{ #category : #accessing }
DRStackInstruction >> stackDependencies [

	^ stackDependencies
]

{ #category : #accessing }
DRStackInstruction >> stackDependencies: anObject [

	stackDependencies := anObject.
	stackDependencies do: [ :e | e addStackDependent: self ]
]

{ #category : #accessing }
DRStackInstruction >> stackDependents [
	^ stackDependents
]

{ #category : #validation }
DRStackInstruction >> validate [

	super validate
]
