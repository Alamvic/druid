Class {
	#name : 'DRStore',
	#superclass : 'DRInstruction',
	#instVars : [
		'size'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRStore >> acceptVisitor: aVisitor [
	
	^ aVisitor visitStore: self
]

{ #category : 'accessing' }
DRStore >> address [

	^ self operand2
]

{ #category : 'accessing' }
DRStore >> address: anAddress [

	^ self operand2: anAddress
]

{ #category : 'testing' }
DRStore >> canMoveOutsideLoop: backJump [

	^ false
]

{ #category : 'testing' }
DRStore >> isJITCompileTimeExpression [

	^ self address isStaged
]

{ #category : 'visiting' }
DRStore >> isMandatoryInstruction [ 

	^ true
]

{ #category : 'visiting' }
DRStore >> isStore [ 

	^ true
]

{ #category : 'visiting' }
DRStore >> opcode [
	
	^ 'Store'
]

{ #category : 'accessing' }
DRStore >> operands: operandList [

	"Why not in the list?"
	size := operandList first value.
	super operands: { operandList second. operandList third asMemoryAddress }
]

{ #category : 'accessing' }
DRStore >> postCopy [

	super postCopy.
	self address: self address copy
]

{ #category : 'users-definitions' }
DRStore >> replaceDependency: anOperand by: anotherOperand [

	(operands includes: anOperand) ifTrue: [
		super replaceDependency: anOperand by: anotherOperand.

		(self address operands includes: anOperand) ifTrue: [
			self address replaceDependency: anOperand by: anotherOperand ].
		^ self ].

	anOperand removeUser: self.
	anotherOperand addUser: self.
	self address replaceDependency: anOperand by: anotherOperand
]

{ #category : 'SCCP' }
DRStore >> sccpLatticeValueFor: sccp [ 
	
	"This instruction has no value"
	^ sccp bottom
]

{ #category : 'accessing' }
DRStore >> size [

	^ size
]

{ #category : 'types' }
DRStore >> type [

	^ DRVoidType new
]
