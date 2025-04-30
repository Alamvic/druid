Class {
	#name : 'DRPiNode',
	#superclass : 'DRCopy',
	#instVars : [
		'constraint'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'initialization' }
DRPiNode class >> constraint: aDRConditionalConstraint on: aDRValue [
	^ self new
		operands: (OrderedCollection with: aDRValue);
		constraint: aDRConditionalConstraint;
		yourself
]

{ #category : 'visiting' }
DRPiNode >> collectConstraints [

	^ self operand collectConstraints intersectionWith: constraint
]

{ #category : 'testing' }
DRPiNode >> constraint [
	
	^ constraint 
]

{ #category : 'testing' }
DRPiNode >> constraint: aDRConditionalConstraint [
	
	constraint ifNotNil: [ constraint removeUserOfValues: self ].
	constraint := aDRConditionalConstraint.
	constraint addUserOfValues: self.

]

{ #category : 'accessing' }
DRPiNode >> dependencies [
	
	^ super dependencies, (constraint ifNil: [{ }] ifNotNil: [constraint asDependencyList]).
]

{ #category : 'testing' }
DRPiNode >> isPiNode [

	^ true
]

{ #category : 'arithmetic' }
DRPiNode >> negated [
	^ self class
		constraint: constraint negated
		on: self operand.
]

{ #category : 'testing' }
DRPiNode >> opcode [
	
	^ 'π'
]

{ #category : 'printing' }
DRPiNode >> prettyPrint [

	^ super prettyPrint, ', ', self opcode, ' ', constraint prettyPrint
		
]

{ #category : 'testing' }
DRPiNode >> removeDependencies [

	self operands asSet do: [ :e | e removeUser: self ].
	self removeDependencyOnConstraint.
]

{ #category : 'testing' }
DRPiNode >> removeDependencyOnConstraint [

	constraint removeUserOfValues: self
]

{ #category : 'removing' }
DRPiNode >> removeFromCFG [
	
	super removeFromCFG.
	self removeDependencyOnConstraint.

]

{ #category : 'replacing' }
DRPiNode >> replaceOperand: anOperand by: anotherOperand [

	super replaceOperand: anOperand by: anotherOperand.
	
	constraint replaceOperand: anOperand by: anotherOperand.
]
