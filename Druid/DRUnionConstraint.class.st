Class {
	#name : #DRUnionConstraint,
	#superclass : #DRComposedConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #'as yet unclassified' }
DRUnionConstraint class >> less: minValue greater: maxValue [

	^ self constraints: { 
			  (DRLessConstraint withValue: minValue).
			  (DRGreaterConstraint withValue: maxValue) }
]

{ #category : #arithmetic }
DRUnionConstraint >> includes: otherConstraint [

	otherConstraint isUnion ifTrue: [ 
		^ otherConstraint constraints allSatisfy: [ :c | self includes: c ] ].

	^ constraints anySatisfy: [ :c | c includes: otherConstraint ]
]

{ #category : #arithmetic }
DRUnionConstraint >> includesValue: otherConstantValue [

	^ constraints anySatisfy: [ :c | c includesValue: otherConstantValue ]
]

{ #category : #arithmetic }
DRUnionConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ constraints anySatisfy: [ :c | c includesValuesGreaterThan: otherConstantValue ]
]

{ #category : #arithmetic }
DRUnionConstraint >> includesValuesLessThan: otherConstantValue [

	^ constraints anySatisfy: [ :c | c includesValuesLessThan: otherConstantValue ]
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
