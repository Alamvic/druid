Class {
	#name : 'DRSimpleConstraint',
	#superclass : 'DRConditionalConstraint',
	#instVars : [
		'constantValue'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'initialization' }
DRSimpleConstraint class >> withValue: aConstantValue [

	^ self new
		  constantValue: aConstantValue value;
		  yourself
]

{ #category : 'comparing' }
DRSimpleConstraint >> = otherConstraint [

	^ self class = otherConstraint class and: [ 
		  self constantValue = otherConstraint constantValue ]
]

{ #category : 'accessing' }
DRSimpleConstraint >> asDependencyList [

	^ (constantValue isKindOf: DRInstruction)
			ifTrue: [ constantValue asDependencyList ]
			ifFalse: [ {} ].
]

{ #category : 'accessing' }
DRSimpleConstraint >> constantValue [

	^ constantValue
]

{ #category : 'accessing' }
DRSimpleConstraint >> constantValue: anObject [

	constantValue := anObject
]

{ #category : 'testing' }
DRSimpleConstraint >> hasType: aDRClassType [

	^ false
]

{ #category : 'comparing' }
DRSimpleConstraint >> hash [
	"Hash implemented because #= was implemented"

	^ self class hash bitXor: self constantValue hash
]

{ #category : 'testing' }
DRSimpleConstraint >> includes: otherConstraint [

	^ self negated isIncludedIn: otherConstraint negated.
]

{ #category : 'arithmetic' }
DRSimpleConstraint >> isIncludedIn: otherConstraint [

	self subclassResponsibility 
]

{ #category : 'printing' }
DRSimpleConstraint >> prettyPrint [
	^ self className, '(', self constantValue asDRValue prettyPrint, ')'.
]

{ #category : 'printing' }
DRSimpleConstraint >> printOn: aStream [

	aStream
		nextPutAll: self opCode;
		nextPutAll: ' ';
		print: constantValue
]

{ #category : 'replacing' }
DRSimpleConstraint >> replaceOperand: anOperand by: anotherOperand [

	(constantValue = anOperand) ifTrue: [ constantValue := anotherOperand ]
]
