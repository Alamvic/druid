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
DRRegistersInfo >> visitAddition: aDRAdd [ 
	"Do nothing"
]

{ #category : #visiting }
DRRegistersInfo >> visitComparison: aDRComparison andDo: aClosure [ 

		| operands |
	operands := aDRComparison operands.
	((operands anySatisfy: #isConstant) and: [ operands anySatisfy: #isRegister ]) 
		ifFalse: [ ^ self "We need a register and a constant" ].

	aDRComparison operand1 isRegister ifFalse: [ "Check register is on left"
		^ aDRComparison reversed acceptVisitor: self "if not reverse it" ]. 

	^ aClosure value: aDRComparison operand1 value: (aDRComparison operand2 acceptVisitor: self)
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
	self shouldBeImplemented.
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
DRRegistersInfo >> visitOverflowJump: aDROverflowJump [ 
	self flag: #TODO. "Do nothing?"
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
	self flag: #TODO. "Mask constraint"
	self notImplemented
]
