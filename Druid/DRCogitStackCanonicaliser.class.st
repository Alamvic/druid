Class {
	#name : #DRCogitStackCanonicaliser,
	#superclass : #DRCogitCanonicaliser,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitStackCanonicaliser >> visitLeftRotate: aDRRotate [ 
	^ self
]

{ #category : #visiting }
DRCogitStackCanonicaliser >> visitPush: aDRPush [

	| pushedValue |
	pushedValue := aDRPush operand1 simpleConstantFold.
	pushedValue isLoad ifFalse: [ ^ self ].
	pushedValue address isAbsolute ifTrue: [
		self flag: #FIXME. "?"
		^ self ].
	aDRPush replaceDependency: pushedValue by: pushedValue address
]
