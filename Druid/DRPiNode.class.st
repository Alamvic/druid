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

{ #category : 'converting' }
DRPiNode >> asZ3Constraint: aZ3Solver [

	| var |
	var := aZ3Solver drValAsZ3Val: self ifUnsupported: [ ^ nil ].
	^ self asZ3ConstraintOnVar: var

]

{ #category : 'converting' }
DRPiNode >> asZ3ConstraintOnVar: var [

	| types useUnsigned |
	types := {
		         self operand type.
		         constraint otherValue asDRValue type }.
	useUnsigned := (types allSatisfy: [ :e | e isIntegerType ]) and: [
		               types anySatisfy: [ :e | e isUnsigned ] ].
	^ useUnsigned
		  ifTrue: [ constraint asZ3UnsignedConstraintOnVar: var ]
		  ifFalse: [ constraint asZ3ConstraintOnVar: var ]
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

{ #category : 'replacing' }
DRPiNode >> postCopy [ 

	super postCopy.
	constraint := constraint copy.
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
DRPiNode >> replaceDependency: anOperand by: anotherOperand [

	self replaceOperand: anOperand by: anotherOperand.
	
	constraint replaceOperand: anOperand by: anotherOperand.

]
