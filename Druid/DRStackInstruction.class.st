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

{ #category : #'as yet unclassified' }
DRStackInstruction >> definingStackInstruction [

	"Return the stack instruction defining the stack slot I'm popping"
	^ self stackSlotDefiningAtDepth: -1 ifNone: [ nil ]
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

{ #category : #accessing }
DRStackInstruction >> replaceDependency: anOperand by: anotherOperand [

	anOperand = anotherOperand ifTrue: [ ^ self ].

	(self stackDependencies asArray includes: anOperand) ifTrue: [
		self stackDependencies: (self stackDependencies asArray copyWithout: anOperand).
		self stackDependencies add: anotherOperand.
		^ self ].

	super replaceDependency: anOperand by: anotherOperand
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

	stackDependencies := anObject asSet.
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

{ #category : #'as yet unclassified' }
DRStackInstruction >> stackSlotDefiningAtDepth: anInteger ifNone: aBlock [
	"The only stack instructions defining stack slots are the push instructions"

	| definingSlots |
	definingSlots := self stackSlotsDefiningAtDepth: anInteger.
	definingSlots ifEmpty: [ ^ aBlock value ].
	^ definingSlots unique
]

{ #category : #'as yet unclassified' }
DRStackInstruction >> stackSlotsDefiningAtDepth: anInteger [ 
	"The only stack instructions defining stack slots are the push instructions"

	| definingSlots |
	definingSlots := self stackDependencies flatCollect: [ :s |
		  s stackSlotsDefiningAtDepth: anInteger + self stackDelta ].
	^ definingSlots
]

{ #category : #validation }
DRStackInstruction >> validateDependenciesAreUsedByMyself [

	self dataDependencies do: [ :op | 
		(op hasUser: self) ifFalse: [ 
			DRError signal:
				'Dependency-user mismatch between: ' , self asString , ' and: '
				, op asString ] ]
]
