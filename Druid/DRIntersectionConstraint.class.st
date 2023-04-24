Class {
	#name : #DRIntersectionConstraint,
	#superclass : #DRComposedConstraint,
	#category : #'Druid-IR-Paths'
}

{ #category : #accessing }
DRIntersectionConstraint class >> between: minValue and: maxValue [

	^ self constraints: { 
			  (DRGreaterConstraint withValue: minValue).
			  (DRLessConstraint withValue: maxValue) }
]

{ #category : #arithmetic }
DRIntersectionConstraint >> hasAllValuesGreaterThan: otherConstantValue [

	^ constraints allSatisfy: [ :c | c hasAllValuesGreaterThan: otherConstantValue ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> hasAllValuesLessThan: otherConstantValue [

	^ constraints allSatisfy: [ :c | c hasAllValuesLessThan: otherConstantValue ]
]

{ #category : #testing }
DRIntersectionConstraint >> hasAllValuesWithMask: aMask [

	"ATENTION: This implementation does not work mixing Masked & Ranged constraints"

	^ constraints allSatisfy: [ :c | c hasAllValuesWithMask: aMask ]
]

{ #category : #testing }
DRIntersectionConstraint >> hasAllValuesWithoutMask: aMask [

	"ATENTION: This implementation does not work mixing Masked & Ranged constraints"

	^ constraints allSatisfy: [ :c | c hasAllValuesWithoutMask: aMask ]
]

{ #category : #testing }
DRIntersectionConstraint >> hasNotType: aDRClassType [

	^ constraints allSatisfy: [ :c | c hasNotType: aDRClassType ]
]

{ #category : #testing }
DRIntersectionConstraint >> hasType: aDRClassType [

	^ constraints allSatisfy: [ :c | c hasType: aDRClassType ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> hasValue: otherConstantValue [

	^ constraints allSatisfy: [ :c | c hasValue: otherConstantValue ]
]

{ #category : #testing }
DRIntersectionConstraint >> includes: otherConstraint [

	^ constraints allSatisfy: [ :c | c includes: otherConstraint ]
]

{ #category : #arithmetic }
DRIntersectionConstraint >> negated [

	^ DRUnionConstraint constraints: (constraints collect: [ :c | c negated ])
]

{ #category : #printing }
DRIntersectionConstraint >> opCode [

	^ 'AND'
]
