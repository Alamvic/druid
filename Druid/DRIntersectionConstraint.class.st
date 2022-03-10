Class {
	#name : #DRIntersectionConstraint,
	#superclass : #DRComposedConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRIntersectionConstraint class >> between: minValue and: maxValue [

	^ self constraints: { 
			  (DRGreaterConstraint withValue: minValue).
			  (DRLessConstraint withValue: maxValue) }
]

{ #category : #testing }
DRIntersectionConstraint >> includes: otherConstraint [

	^ constraints allSatisfy: [ :c | c includes: otherConstraint ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> includesValue: otherConstantValue [

	^ constraints allSatisfy: [ :c | c includesValue: otherConstantValue ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ constraints allSatisfy: [ :c | c includesValuesGreaterThan: otherConstantValue ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> includesValuesLessThan: otherConstantValue [

	^ constraints allSatisfy: [ :c | c includesValuesLessThan: otherConstantValue ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> negated [

	^ DRUnionConstraint constraints: (constraints collect: [ :c | c negated ])
]

{ #category : #printing }
DRIntersectionConstraint >> opCode [

	^ 'AND'
]
