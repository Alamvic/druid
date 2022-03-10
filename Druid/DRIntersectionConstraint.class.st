Class {
	#name : #DRIntersectionConstraint,
	#superclass : #DRConditionalConstraint,
	#instVars : [
		'constraints'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRIntersectionConstraint class >> between: minValue and: maxValue [

	^ self constraints: { 
			  (DRGreaterConstraint withValue: minValue).
			  (DRLessConstraint withValue: maxValue) }
]

{ #category : #accessing }
DRIntersectionConstraint class >> constraints: constraints [

	^ self new
		  constraints: constraints;
		  yourself
]

{ #category : #accessing }
DRIntersectionConstraint >> constraints [

	^ constraints
]

{ #category : #accessing }
DRIntersectionConstraint >> constraints: anObject [

	constraints := anObject
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
DRIntersectionConstraint >> isIncludedIn: otherConstraint [

	^ constraints allSatisfy: [ :c | c isIncludedIn: otherConstraint ]
]

{ #category : #printing }
DRIntersectionConstraint >> opCode [

	^ 'AND'
]

{ #category : #printing }
DRIntersectionConstraint >> printOn: aStream [

	aStream
		nextPutAll: '(';
		print: constraints first;
		nextPutAll: self opCode;
		print: constraints second;
		nextPutAll: ')'

]
