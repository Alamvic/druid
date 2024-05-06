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

	stackDependencies := Set new.
	stackDependents := Set new
]

{ #category : #validation }
DRStackInstruction >> dataDependencies [

	^ super dependencies
]

{ #category : #accessing }
DRStackInstruction >> dependencies [

	^ super dependencies , self stackDependencies
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

{ #category : #accessing }
DRStackInstruction >> stackEffectDependencies [
	"Return the closest stack effect dependencies following the stack dependencies"

	^ self stackDependencies flatCollect: [ :e |
		  e isStackEffect
			  ifTrue: [ { e } ]
			  ifFalse: [ e stackEffectDependencies ] ]
]

{ #category : #validation }
DRStackInstruction >> validate [

	super validate
]

{ #category : #validation }
DRStackInstruction >> validateDependenciesAreUsedByMyself [

	self dataDependencies do: [ :op | 
		(op hasUser: self) ifFalse: [ 
			DRError signal:
				'Dependency-user mismatch between: ' , self asString , ' and: '
				, op asString ] ]
]
