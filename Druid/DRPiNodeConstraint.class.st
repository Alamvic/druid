Class {
	#name : 'DRPiNodeConstraint',
	#superclass : 'Object',
	#instVars : [
		'otherValue'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'initialization' }
DRPiNodeConstraint class >> withValue: aDRValue [

	^ self new initializeWithValue: aDRValue
]

{ #category : 'adding' }
DRPiNodeConstraint >> addUserOfValues: aDRPiNode [
	
	self asDependencyList do: [ :dep | dep addUser: aDRPiNode ].
]

{ #category : 'accessing' }
DRPiNodeConstraint >> asConstantConstraint [

	^ self
		  ifConstantDo: [ :val |
		  self constantConstraintClass withValue: val ]
		  ifNot: [ nil ]
]

{ #category : 'converting' }
DRPiNodeConstraint >> asDependencyList [

	^ self ifConstantDo: [ {  } ] ifNot: #asDependencyList
]

{ #category : 'accessing' }
DRPiNodeConstraint >> asZ3ConstraintOnVar: aZ3Variable [

	self subclassResponsibility 
]

{ #category : 'accessing' }
DRPiNodeConstraint >> asZ3UnsignedConstraintOnVar: aZ3Variable [

	^ self asZ3ConstraintOnVar: aZ3Variable 
]

{ #category : 'accessing' }
DRPiNodeConstraint >> binaryConstraint: constraint on: aZ3Variable [

	^ aZ3Variable
		  perform: constraint
		  with:
		  (self ifConstantDo: #yourself ifNot: #name)
]

{ #category : 'accessing' }
DRPiNodeConstraint >> constantConstraintClass [

	^ self subclassResponsibility
]

{ #category : 'reversing' }
DRPiNodeConstraint >> distanceForBuildingABCDGraph [

	^ nil
]

{ #category : 'adding' }
DRPiNodeConstraint >> ifConstantDo: trueBlock ifNot: falseBlock [

	| val |
	val := otherValue asDRValue.
	(val isConstant or: [val isCopy]) ifTrue: [
		val := val simpleConstantFold value.
	].

	(val isNumber or: val isBooleanValue)
		ifTrue: [ ^ trueBlock cull: val ]
		ifFalse: [ ^ falseBlock cull: otherValue ]
]

{ #category : 'initialization' }
DRPiNodeConstraint >> initializeWithValue: aDRValue [

	otherValue := aDRValue
]

{ #category : 'accessing' }
DRPiNodeConstraint >> minABCDDistanceToBeSatisfiable [

	^ nil
]

{ #category : 'arithmetic' }
DRPiNodeConstraint >> negated [

	self subclassResponsibility
]

{ #category : 'accessing' }
DRPiNodeConstraint >> otherValue [

	^ otherValue
]

{ #category : 'printing' }
DRPiNodeConstraint >> prettyPrint [

	^ self className , '(' , self otherValue asDRValue prettyPrint , ')'
]

{ #category : 'removing' }
DRPiNodeConstraint >> removeUserOfValues: aDRPiNode [

	self asDependencyList do: [ :dep |
		(dep ~= aDRPiNode operand) ifTrue: [ dep removeUser: aDRPiNode ] ]
]

{ #category : 'replacing' }
DRPiNodeConstraint >> replaceOperand: anOperand by: anotherOperand [ 
	(otherValue = anOperand) ifTrue: [ otherValue := anotherOperand ]
]

{ #category : 'initialization' }
DRPiNodeConstraint >> reversed [

	self subclassResponsibility
]
