Class {
	#name : #DRComposedConstraint,
	#superclass : #DRConditionalConstraint,
	#instVars : [
		'constraints'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRComposedConstraint class >> constraints: constraints [

	^ self new
		  constraints: constraints;
		  yourself
]

{ #category : #accessing }
DRComposedConstraint >> constraints [

	^ constraints
]

{ #category : #accessing }
DRComposedConstraint >> constraints: anObject [

	constraints := anObject
]

{ #category : #printing }
DRComposedConstraint >> printOn: aStream [

	aStream
		nextPutAll: '(';
		print: constraints first;
		nextPutAll: ' ';
		nextPutAll: self opCode;
		nextPutAll: ' ';
		print: constraints second;
		nextPutAll: ')'

]
