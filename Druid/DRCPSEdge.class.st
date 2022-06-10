Class {
	#name : #DRCPSEdge,
	#superclass : #DREdge,
	#instVars : [
		'constraints',
		'parent',
		'branchIndex'
	],
	#category : #'Druid-IR-Paths'
}

{ #category : #'as yet unclassified' }
DRCPSEdge class >> withSource: aSource andDestination: aDestination branchIndex: branchIndex [

	^ (self withSource: aSource andDestination: aDestination)
		  branchIndex: branchIndex;
		  yourself
]

{ #category : #adding }
DRCPSEdge >> addConstraint: aConditionalConstraint to: id [

	^ constraints
		at: id
		put: aConditionalConstraint
]

{ #category : #accessing }
DRCPSEdge >> branchIndex: anInteger [ 
	
	branchIndex := anInteger
]

{ #category : #visiting }
DRCPSEdge >> composedConstraint: aConstraintClass from: aDRInstruction [

	| operands |
	operands := aDRInstruction operands collect: [ :o | o acceptVisitor: self ].
	(operands anySatisfy: [ :o | (o isNil or: o = false) or: o = true ]) ifTrue: [ ^ nil ].
	^ DRRegisterDomain
		  reg: aDRInstruction
		  constraint: (aConstraintClass constraints: operands)
]

{ #category : #accessing }
DRCPSEdge >> constraintFor: regKey [

	^ constraints at: regKey ifAbsent: [ ^ nil ]
]

{ #category : #accessing }
DRCPSEdge >> constraints [

	^ constraints
]

{ #category : #accessing }
DRCPSEdge >> constraints: anObject [

	constraints := anObject
]

{ #category : #accessing }
DRCPSEdge >> deadTarget [

	^ self isDead
		  ifTrue: [ parent deadTarget ifNil: [ parent ] ]
		  ifFalse: [ nil ]
]

{ #category : #initialization }
DRCPSEdge >> initialize [ 

	constraints := Dictionary new.
]

{ #category : #testing }
DRCPSEdge >> isDead [
	^ constraints values anySatisfy: [ :c | c isEmpty ]
]

{ #category : #adding }
DRCPSEdge >> mergeConstraint: aConditionalConstraint to: id [

	constraints
		at: id
		update: [ :oldConstraint | oldConstraint intersectionWith: aConditionalConstraint ]
		initial: aConditionalConstraint
]

{ #category : #accessing }
DRCPSEdge >> parent [
	^ parent
]

{ #category : #accessing }
DRCPSEdge >> parent: aDRRegistersInfo [

	constraints := aDRRegistersInfo constraints copy.
	parent := aDRRegistersInfo
]

{ #category : #printing }
DRCPSEdge >> printOn: aStream [

	aStream nextPutAll: self class name asString.
	aStream nextPutAll: '('.
	aStream print: source id.
	aStream nextPutAll: ' -> '.
	aStream print: destination id.
	aStream nextPutAll: ')'
]

{ #category : #visiting }
DRCPSEdge >> visitAddition: aDRAdd [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitAnd: aDRAnd [

	^ self composedConstraint: DRIntersectionConstraint from: aDRAnd
]

{ #category : #visiting }
DRCPSEdge >> visitBitAnd: aDRBitAnd [
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitBitOr: aDRBitOr [
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitBitShift: aDRBitShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitComparison: aDRComparison andDo: aClosure [ 

		| operands |
	operands := aDRComparison operands.
	((operands anySatisfy: #isConstant) and: [ operands anySatisfy: [:o | o isRegister or: o isInstruction ] ]) 
		ifFalse: [ ^ nil "We need a register and a constant" ].

	aDRComparison operand2 isConstant ifFalse: [ "Check constant on right"
		^ aDRComparison reversed acceptVisitor: self "if not reverse it" ]. 

	^ aClosure value: aDRComparison operand1 value: (aDRComparison operand2 acceptVisitor: self)
]

{ #category : #visiting }
DRCPSEdge >> visitConditionalJump: aDRConditionalJump [

	| domain id |
	"Ignore hardcoded values"
	aDRConditionalJump condition isInstruction ifFalse: [ ^ self ].

	domain := aDRConditionalJump condition acceptVisitor: self.
	domain ifNil: [ ^ self ].

	id := domain reg result name.

	^ branchIndex = 1
		ifTrue: [ self mergeConstraint: domain constraint to: id ]
		ifFalse: [ self mergeConstraint: domain constraint negated to: id ]
]

{ #category : #visiting }
DRCPSEdge >> visitConstant: aDRConstantValue [ 
	
	^ aDRConstantValue value 
]

{ #category : #visiting }
DRCPSEdge >> visitCopy: aDRCopy [

	| constraint constraintOrConstant |
	constraintOrConstant := aDRCopy operand1 acceptVisitor: self.

	constraintOrConstant ifNil: [ ^ nil ].

	constraint := aDRCopy operand1 isConstant
		              ifTrue: [ DREqualsConstraint withValue: constraintOrConstant ]
		              ifFalse: [ constraintOrConstant ].

	^ self addConstraint: constraint to: aDRCopy result name
]

{ #category : #visiting }
DRCPSEdge >> visitDivision: aDRDivision [ 
		"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitEqualsComparison: aDREqualsThanComparison [

	^ self
		  visitComparison: aDREqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain equals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitGreaterOrEqualsThanComparison: aDRGreaterOrEqualsThanComparison [

	^ self
		  visitComparison: aDRGreaterOrEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain greaterOrEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitGreaterThanComparison: aDRGreaterThanComparison [ 

	^ self
		  visitComparison: aDRGreaterThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain greater: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitJump: aDRJump [ 

	"Simple jumps do not have type constraints"
]

{ #category : #visiting }
DRCPSEdge >> visitLeftShift: aDRLeftShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitLessOrEqualsThanComparison: aDRLessOrEqualsThanComparison [

	^ self
		  visitComparison: aDRLessOrEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain lessOrEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitLessThanComparison: aDRLessThanComparison [

	^ self
		  visitComparison: aDRLessThanComparison
		  andDo: [ :reg :value | DRRegisterDomain less: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitLoad: aDRLoad [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitMod: aDRMod [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitMultiply: aDRMultiply [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitMultiplyOverflowJump: aDRMultiplyOverflowJump [ 
	self flag: #TODO. "Do nothing?"
]

{ #category : #visiting }
DRCPSEdge >> visitNegate: aDRNegate [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitNoop: aDRNoop [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitNotEqualsComparison: aDRNotEqualsThanComparison [

	^ self
		  visitComparison: aDRNotEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain notEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitOr: aDROr [
		
	^ self composedConstraint: DRUnionConstraint from: aDROr
]

{ #category : #visiting }
DRCPSEdge >> visitOverflowJump: aDROverflowJump [ 
	self flag: #TODO. "Do nothing?"
]

{ #category : #visiting }
DRCPSEdge >> visitPhiFunction: aDRPhiFunction [ 

	| operandsConstraint phiConstraint |
	operandsConstraint := aDRPhiFunction operands collect: [ :op | op acceptVisitor: self ].
	
	phiConstraint := DRUnionConstraint constraints: operandsConstraint.
	
	^self addConstraint: phiConstraint to: aDRPhiFunction result name
]

{ #category : #visiting }
DRCPSEdge >> visitPhysicalRegister: aDRPhysicalRegister [ 
	
	^ self constraintFor: aDRPhysicalRegister name
]

{ #category : #visiting }
DRCPSEdge >> visitReturn: aDRReturn [

	"Returns do not have type constraints"

	
]

{ #category : #visiting }
DRCPSEdge >> visitRightShift: aDRRightShift [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitStore: aDRLoad [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitSubstract: aDRSubtract [ 
	"Do nothing"
]

{ #category : #visiting }
DRCPSEdge >> visitTest: aDRTest [

	^ self
		  visitComparison: aDRTest
		  andDo: [ :reg :value | DRRegisterDomain mask: reg with: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitUnsignedCoercion: aDRUnsignedCoercion [ 
	"Do nothing"
]
