Class {
	#name : #DRUnionConstraint,
	#superclass : #DRComposedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #'as yet unclassified' }
DRUnionConstraint class >> less: minValue greater: maxValue [

	^ self constraints: { 
			  (DRLessConstraint withValue: minValue).
			  (DRGreaterConstraint withValue: maxValue) }
]

{ #category : #arithmetic }
DRUnionConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ constraints anySatisfy: [ :c | c hasAllValuesGreaterThan: otherConstantValue ]
]

{ #category : #arithmetic }
DRUnionConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ constraints anySatisfy: [ :c | c hasAllValuesLessThan: otherConstantValue ]
]

{ #category : #testing }
DRUnionConstraint >> hasAllValuesWithMask: aMask [

	"ATENTION: This implementation does not work mixing Masked & Ranged constraints"

	^ constraints anySatisfy: [ :c | c hasAllValuesWithMask: aMask ]
]

{ #category : #arithmetic }
DRUnionConstraint >> hasAllValuesWithoutMask: aMask [

	"ATENTION: This implementation does not work mixing Masked & Ranged constraints"

	^ constraints anySatisfy: [ :c | c hasAllValuesWithoutMask: aMask ]
]

{ #category : #arithmetic }
DRUnionConstraint >> hasValue: otherConstantValue [

	^ constraints anySatisfy: [ :c | c hasValue: otherConstantValue ]
]

{ #category : #arithmetic }
DRUnionConstraint >> includes: otherConstraint [

	otherConstraint isUnion ifTrue: [ 
		^ otherConstraint constraints allSatisfy: [ :c | self includes: c ] ].

	^ constraints anySatisfy: [ :c | c includes: otherConstraint ]
]

{ #category : #testing }
DRUnionConstraint >> isUnion [ 

	^ true
]

{ #category : #arithmetic }
DRUnionConstraint >> negated [

	^ DRIntersectionConstraint constraints: (constraints collect: [ :c | c negated ])
]

{ #category : #printing }
DRUnionConstraint >> opCode [

	^ 'OR'
]
