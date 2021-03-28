Class {
	#name : #DRConstantCondition,
	#superclass : #DRCondition,
	#instVars : [
		'value'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRConstantCondition class >> value: aDRConstant [ 
	
	^ self new
		value: aDRConstant;
		yourself
]

{ #category : #visiting }
DRConstantCondition >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitConstantCondition: self
]

{ #category : #accessing }
DRConstantCondition >> value [

	^ value
]

{ #category : #accessing }
DRConstantCondition >> value: anObject [

	value := anObject
]
