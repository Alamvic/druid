Class {
	#name : #DRRegistersInfo,
	#superclass : #DREdge,
	#instVars : [
		'constraints'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #adding }
DRRegistersInfo >> addConstraint: aConditionalConstraint [
	
	constraints add: aConditionalConstraint reg name -> aConditionalConstraint
]

{ #category : #accessing }
DRRegistersInfo >> constraintFor: regKey [

	^ constraints at: regKey
]

{ #category : #accessing }
DRRegistersInfo >> constraints [

	^ constraints
]

{ #category : #accessing }
DRRegistersInfo >> constraints: anObject [

	constraints := anObject
]

{ #category : #initialization }
DRRegistersInfo >> initialize [ 

	constraints := Dictionary new.
]

{ #category : #printing }
DRRegistersInfo >> printOn: aStream [

	aStream nextPutAll: self class name asString.
	aStream nextPutAll: '('.
	aStream print: source id.
	aStream nextPutAll: ' -> '.
	aStream print: destination id.
	aStream nextPutAll: ')'
]

{ #category : #visiting }
DRRegistersInfo >> visitConditionalJump: aDRConditionalJump [

	| constraint |
	"Ignore hardcoded values"
	aDRConditionalJump operand1 isInstruction ifFalse: [ ^ self ].

	constraint := aDRConditionalJump operand1 acceptVisitor: self.
	aDRConditionalJump trueBranch = destination ifTrue: [ 
		self addConstraint: constraint ].
	aDRConditionalJump falseBranch = destination ifTrue: [ 
		self addConstraint: constraint negated ]
]

{ #category : #visiting }
DRRegistersInfo >> visitConstant: aDRConstantValue [ 
	
	^ aDRConstantValue value 
]

{ #category : #visiting }
DRRegistersInfo >> visitCopy: aDRCopy [ 
	
	| operandConstraint constantValue |
	constantValue := aDRCopy operand1 acceptVisitor: self.
	self addConstraint: (DRRegisterConstraint equals: aDRCopy result than: constantValue)
]

{ #category : #visiting }
DRRegistersInfo >> visitGreaterOrEqualsThanComparison: aDRGreaterOrEqualsThanComparison [

	self flag: #TODO. "Visit operands"
	^ DRRegisterConstraint
		  greaterOrEquals: aDRGreaterOrEqualsThanComparison operand1
		  than: aDRGreaterOrEqualsThanComparison operand2 value
]

{ #category : #visiting }
DRRegistersInfo >> visitJump: aDRJump [ 

	"Simple jumps do not have type constraints"
]

{ #category : #visiting }
DRRegistersInfo >> visitReturn: aDRReturn [

	"Returns do not have type constraints"

	
]

{ #category : #visiting }
DRRegistersInfo >> visitTest: aDRTest [ 
	self shouldBeImplemented.
]
