Class {
	#name : #DROopConstraint,
	#superclass : #DRSimpleConstraint,
	#instVars : [
		'typeClass'
	],
	#category : #'Druid-IR-Paths'
}

{ #category : #accessing }
DROopConstraint class >> withType: aClass [

	^ self new
		  typeClass: aClass;
		  yourself
]

{ #category : #testing }
DROopConstraint >> includes: otherConstraint [

	^ self innerConstraint includes: otherConstraint.
]

{ #category : #constraints }
DROopConstraint >> innerConstraint [

	| immediateMaskClass |
	immediateMaskClass := typeClass isImmediateClass
		                      ifTrue: [ DRAnyMaskConstraint ]
		                      ifFalse: [ DRNoneMaskConstraint ].
	^ immediateMaskClass withValue: 7
]

{ #category : #constraints }
DROopConstraint >> loadConstraint: bytes [
	"LOAD header"

	| format bitsAfter |
	format := typeClass classLayout instanceSpecification.
	bitsAfter := bytes * 8 min: 24.
	
	^ DRAnyMaskConstraint withValue: format << bitsAfter
]

{ #category : #arithmetic }
DROopConstraint >> negated [

	^ self innerConstraint negated
]

{ #category : #printing }
DROopConstraint >> opCode [

	^ 'OOP'
]

{ #category : #accessing }
DROopConstraint >> typeClass [

	^ typeClass
]

{ #category : #accessing }
DROopConstraint >> typeClass: anObject [

	typeClass := anObject
]
