Class {
	#name : 'DRConditionalConstraint',
	#superclass : 'Object',
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'testing' }
DRConditionalConstraint >> addUserOfValues: aDRInstruction [

	self asDependencyList do: [ :dep | dep addUser: aDRInstruction ]
]

{ #category : 'accessing' }
DRConditionalConstraint >> asDependencyList [

	self subclassResponsibility
]

{ #category : 'as yet unclassified' }
DRConditionalConstraint >> distanceForBuildingABCDGraph [

	^ nil
]

{ #category : 'arithmetic' }
DRConditionalConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : 'arithmetic' }
DRConditionalConstraint >> hasAllValuesLessThan: otherConstantValue [

	self subclassResponsibility
]

{ #category : 'testing' }
DRConditionalConstraint >> hasAllValuesWithMask: aMask [

	self subclassResponsibility
]

{ #category : 'testing' }
DRConditionalConstraint >> hasAllValuesWithoutMask: aMask [

	self subclassResponsibility
]

{ #category : 'testing' }
DRConditionalConstraint >> hasNotType: aDRType [

	^ self subclassResponsibility
]

{ #category : 'arithmetic' }
DRConditionalConstraint >> hasObjectValue: objectValue [

	^ false
]

{ #category : 'testing' }
DRConditionalConstraint >> hasType: aDRType [

	^ self subclassResponsibility
]

{ #category : 'arithmetic' }
DRConditionalConstraint >> hasValue: otherConstantValue [

	self subclassResponsibility
]

{ #category : 'testing' }
DRConditionalConstraint >> includes: otherConstraint [

	self subclassResponsibility
]

{ #category : 'merging' }
DRConditionalConstraint >> intersectionWith: otherConditionalConstraint [

	(self includes: otherConditionalConstraint) ifTrue: [ 
		^ otherConditionalConstraint ].
	(otherConditionalConstraint includes: self) ifTrue: [ 
		^ self ].
	(otherConditionalConstraint negated includes: self) ifTrue: [ 
		^ DREmptyConstraint new ].
	
	^ DRIntersectionConstraint constraints: { self. otherConditionalConstraint }
]

{ #category : 'testing' }
DRConditionalConstraint >> isAny [

	^ false
]

{ #category : 'testing' }
DRConditionalConstraint >> isEmpty [

	^ false
]

{ #category : 'testing' }
DRConditionalConstraint >> isUnion [

	^ false
]

{ #category : 'as yet unclassified' }
DRConditionalConstraint >> minABCDDistanceToBeSatisfiable [

	^ nil
]

{ #category : 'arithmetic' }
DRConditionalConstraint >> negated [

	self subclassResponsibility
]

{ #category : 'printing' }
DRConditionalConstraint >> opCode [

	self subclassResponsibility
]

{ #category : 'printing' }
DRConditionalConstraint >> printOn: aStream [

	aStream nextPutAll: self opCode
]

{ #category : 'testing' }
DRConditionalConstraint >> removeUserOfValues: aDRInstruction [

	self asDependencyList do: [ :dep | dep removeUser: aDRInstruction ]
]

{ #category : 'replacing' }
DRConditionalConstraint >> replaceOperand: anOperand by: anotherOperand [
	self subclassResponsibility
]
