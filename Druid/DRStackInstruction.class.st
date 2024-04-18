Class {
	#name : #DRStackInstruction,
	#superclass : #DRInstruction,
	#instVars : [
		'stackDependency',
		'stackDependents'
	],
	#category : #'Druid-IR'
}

{ #category : #adding }
DRStackInstruction >> addStackDependent: aStackInstruction [ 

	stackDependents add: aStackInstruction
]

{ #category : #testing }
DRStackInstruction >> initialize [

	super initialize.
	stackDependents := OrderedCollection new
]

{ #category : #testing }
DRStackInstruction >> isStackInstruction [

	^ true
]

{ #category : #removing }
DRStackInstruction >> removeStackDependency: aStackDependency [
	
	stackDependency := stackDependency copyWithout: aStackDependency
]

{ #category : #removing }
DRStackInstruction >> removeStackDependent: aStackDependent [ 
	
	stackDependents := stackDependents copyWithout: aStackDependent
]

{ #category : #'as yet unclassified' }
DRStackInstruction >> shiftStackAccessBy: anInteger [ 
	
	"Delegate to my stack dependents"
	stackDependents do: [ :e | e shiftStackAccessBy: anInteger ].
]

{ #category : #accessing }
DRStackInstruction >> stackDependency [

	^ stackDependency
]

{ #category : #accessing }
DRStackInstruction >> stackDependency: anObject [

	stackDependency := anObject.
	stackDependency do: [ :e | e addStackDependent: self ]
]

{ #category : #accessing }
DRStackInstruction >> stackDependents [
	^ stackDependents
]

{ #category : #validation }
DRStackInstruction >> validate [

	super validate
]
