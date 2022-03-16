Class {
	#name : #DRFullConstraint,
	#superclass : #DRConditionalConstraint,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRFullConstraint >> includes: otherConstraint [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValue: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValuesGreaterThan: otherConstantValue [

	^ true
]

{ #category : #arithmetic }
DRFullConstraint >> includesValuesLessThan: otherConstantValue [

	^ true
]
