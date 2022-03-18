Class {
	#name : #DRRegistersInfo,
	#superclass : #DREdge,
	#instVars : [
		'constraints',
		'parent'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #adding }
DRRegistersInfo >> addConstraint: aConditionalConstraint [

	constraints
		at: aConditionalConstraint reg name
		put: aConditionalConstraint
]

{ #category : #visiting }
DRRegistersInfo >> composedConstraint: aConstraintClass from: aDRInstruction [

	| operands |
	operands := aDRInstruction operands collect: [ :o | o acceptVisitor: self ].
	(operands anySatisfy: [ :o | (o isNil or: o = false) or: o = true ]) ifTrue: [ ^ nil ].
	^ DRRegisterConstraint
		  reg: aDRInstruction
		  constraint: (aConstraintClass constraints: operands)
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

{ #category : #'as yet unclassified' }
DRRegistersInfo >> deadTarget [

	^ self isDead
		  ifTrue: [ parent deadTarget ifNil: [ parent ] ]
		  ifFalse: [ nil ]
]

{ #category : #initialization }
DRRegistersInfo >> initialize [ 

	constraints := Dictionary new.
]

{ #category : #testing }
DRRegistersInfo >> isDead [
	^ constraints values anySatisfy: [ :c | c isDead ]
]

{ #category : #adding }
DRRegistersInfo >> mergeConstraint: aConditionalConstraint [
	aConditionalConstraint ifNil: [ 1halt ].
	constraints
		at: aConditionalConstraint reg name
		ifPresent: [ :oldConstraint | 
			self addConstraint: (oldConstraint mergeWith: aConditionalConstraint) ]
		ifAbsentPut: aConditionalConstraint
]

{ #category : #accessing }
DRRegistersInfo >> parent: aDRRegistersInfo [

	constraints := aDRRegistersInfo constraints copy.
	parent := aDRRegistersInfo
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
DRRegistersInfo >> visitAddition: aDRAdd [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitAnd: aDRAnd [

	^ self composedConstraint: DRIntersectionConstraint from: aDRAnd
]

{ #category : #visiting }
DRRegistersInfo >> visitBitAnd: aDRBitAnd [
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitBitShift: aDRBitShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitComparison: aDRComparison andDo: aClosure [ 

		| operands |
	operands := aDRComparison operands.
	((operands anySatisfy: #isConstant) and: [ operands anySatisfy: [:o | o isRegister or: o isInstruction ] ]) 
		ifFalse: [ ^ nil "We need a register and a constant" ].

	aDRComparison operand2 isConstant ifFalse: [ "Check constant on right"
		^ aDRComparison reversed acceptVisitor: self "if not reverse it" ]. 

	^ aClosure value: aDRComparison operand1 value: (aDRComparison operand2 acceptVisitor: self)
]

{ #category : #visiting }
DRRegistersInfo >> visitConditionalJump: aDRConditionalJump [

	| constraint |
	"Ignore hardcoded values"
	aDRConditionalJump operand1 isInstruction ifFalse: [ ^ self ].

	constraint := aDRConditionalJump operand1 acceptVisitor: self.
	constraint ifNil: [ ^ self ].
	aDRConditionalJump trueBranch = destination ifTrue: [ 
		self mergeConstraint: constraint ].
	aDRConditionalJump falseBranch = destination ifTrue: [ 
		self mergeConstraint: constraint negated ]
]

{ #category : #visiting }
DRRegistersInfo >> visitConstant: aDRConstantValue [ 
	
	self flag: #TODO. "Remove this, we must know when something is a constant"
	^ aDRConstantValue value 
]

{ #category : #visiting }
DRRegistersInfo >> visitCopy: aDRCopy [

	| constantValue |
	"Only constant copies are importants"
	aDRCopy operand1 isConstant ifFalse: [ ^ nil ].
	constantValue := aDRCopy operand1 acceptVisitor: self.
	self addConstraint:
		(DRRegisterConstraint equals: aDRCopy result than: constantValue)
]

{ #category : #visiting }
DRRegistersInfo >> visitDivision: aDRDivision [ 
		"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitEqualsComparison: aDREqualsThanComparison [

	^ self
		  visitComparison: aDREqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterConstraint equals: reg than: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitGreaterOrEqualsThanComparison: aDRGreaterOrEqualsThanComparison [

	^ self
		  visitComparison: aDRGreaterOrEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterConstraint greaterOrEquals: reg than: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitGreaterThanComparison: aDRGreaterThanComparison [ 

	^ self
		  visitComparison: aDRGreaterThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterConstraint greater: reg than: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitJump: aDRJump [ 

	"Simple jumps do not have type constraints"
]

{ #category : #visiting }
DRRegistersInfo >> visitLeftShift: aDRLeftShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitLessOrEqualsThanComparison: aDRLessOrEqualsThanComparison [

	^ self
		  visitComparison: aDRLessOrEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterConstraint lessOrEquals: reg than: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitLessThanComparison: aDRLessThanComparison [

	^ self
		  visitComparison: aDRLessThanComparison
		  andDo: [ :reg :value | DRRegisterConstraint less: reg than: value ]
]

{ #category : #visitng }
DRRegistersInfo >> visitLoad: aDRLoad [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitMod: aDRMod [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitMultiply: aDRMultiply [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitMultiplyOverflowJump: aDRMultiplyOverflowJump [ 
	self flag: #TODO. "Do nothing?"
]

{ #category : #visiting }
DRRegistersInfo >> visitNegate: aDRNegate [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitNoop: aDRNoop [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitNotEqualsComparison: aDRNotEqualsThanComparison [

	^ self
		  visitComparison: aDRNotEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterConstraint notEquals: reg than: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitOr: aDROr [
		
	^ self composedConstraint: DRUnionConstraint from: aDROr
]

{ #category : #visiting }
DRRegistersInfo >> visitOverflowJump: aDROverflowJump [ 
	self flag: #TODO. "Do nothing?"
]

{ #category : #visiting }
DRRegistersInfo >> visitPhiFunction: aDRPhiFunction [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitReturn: aDRReturn [

	"Returns do not have type constraints"

	
]

{ #category : #visiting }
DRRegistersInfo >> visitRightShift: aDRRightShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitSubstract: aDRSubtract [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitTest: aDRTest [

	^ self
		  visitComparison: aDRTest
		  andDo: [ :reg :value | DRRegisterConstraint mask: reg with: value ]
]

{ #category : #visiting }
DRRegistersInfo >> visitUnsignedCoercion: aDRUnsignedCoercion [ 
	"Do nothing"
]
