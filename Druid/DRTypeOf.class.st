Class {
	#name : #DRTypeOf,
	#superclass : #DRCondition,
	#instVars : [
		'typeClass'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRTypeOf class >> typeClass: aClass [

	| instance |
	instance := self new.
	instance typeClass: aClass.
	^ instance
]

{ #category : #testing }
DRTypeOf >> isTypeOf [
	^ true
]

{ #category : #accessing }
DRTypeOf >> opcode [
	
	^ 'type of ', typeClass classType name
]

{ #category : #accessing }
DRTypeOf >> typeClass: aClass [
	typeClass := aClass
]
