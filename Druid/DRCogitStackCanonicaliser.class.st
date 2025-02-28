Class {
	#name : 'DRCogitStackCanonicaliser',
	#superclass : 'DRCogitCanonicaliser',
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'visiting' }
DRCogitStackCanonicaliser >> visitPush: aDRPush [

	| pushedValue |
	pushedValue := aDRPush operand1 simpleConstantFold asDRValue.
	pushedValue isLoad ifFalse: [ ^ self ].
	pushedValue address isAbsolute ifTrue: [
		self flag: #FIXME. "?"
		^ self ].
	aDRPush replaceDependency: pushedValue by: pushedValue address
]

{ #category : 'visiting' }
DRCogitStackCanonicaliser >> visitUnspillStackSlot: aDRUnspillStackSlot [ 
	
	"Nothing for now"
]
