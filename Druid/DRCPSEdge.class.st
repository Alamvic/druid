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

{ #category : #building }
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
	(operands anySatisfy: [ :o | (o isNil or: [o = false]) or: [o = true] ]) ifTrue: [ ^ nil ].
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

{ #category : #visiting }
DRCPSEdge >> integerOperation: anOperantion [

	"Could be any number"
	^ self
		  addConstraint: DRFullConstraint new
		  to: anOperantion result name
]

{ #category : #testing }
DRCPSEdge >> isDead [
	^ constraints values anySatisfy: [ :c | c isEmpty ]
]

{ #category : #building }
DRCPSEdge >> mergeConstraint: aConditionalConstraint to: id [

	constraints
		at: id
		update: [ :oldConstraint | oldConstraint intersectionWith: aConditionalConstraint ]
		initial: aConditionalConstraint
]

{ #category : #visiting }
DRCPSEdge >> operandConstraint: operand [

	^ (self constraintFor: operand result name) ifNil: [ 
		  operand acceptVisitor: self ]
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

	^ self integerOperation: aDRAdd
]

{ #category : #visiting }
DRCPSEdge >> visitAnd: aDRAnd [

	^ self composedConstraint: DRIntersectionConstraint from: aDRAnd
]

{ #category : #visiting }
DRCPSEdge >> visitBitAnd: aDRBitAnd [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitBitOr: aDRBitOr [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitBitShift: aDRBitShift [ 

	^ nil
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

	| constraint operand |
	operand := aDRCopy operand1.

	constraint := operand isConstant
		              ifTrue: [ 
		              DREqualsConstraint withValue: aDRCopy operand1 value ]
		              ifFalse: [ self operandConstraint: operand ].

	constraint ifNil: [ ^ nil ].

	^ self addConstraint: constraint to: aDRCopy result name
]

{ #category : #visiting }
DRCPSEdge >> visitDivision: aDRDivision [ 

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitEqualsComparison: aDREqualsThanComparison [

	^ self
		  visitComparison: aDREqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain equals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitFalseObject: aDRFalseObject [

	^ nil
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

	^ nil
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

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitMod: aDRMod [ 

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitMultiply: aDRMultiply [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitMultiplyOverflowJump: aDRMultiplyOverflowJump [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitNegate: aDRNegate [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitNoop: aDRNoop [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitNotEqualsComparison: aDRNotEqualsThanComparison [

	^ self
		  visitComparison: aDRNotEqualsThanComparison
		  andDo: [ :reg :value | 
		  DRRegisterDomain notEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitNullValue: aDRNullValue [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitOr: aDROr [
		
	^ self composedConstraint: DRUnionConstraint from: aDROr
]

{ #category : #visiting }
DRCPSEdge >> visitOverflowJump: aDROverflowJump [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitPhiFunction: aDRPhiFunction [

	| branchOperand sourcePredecessor index constraint |
	sourcePredecessor := self parent source.
	index := aDRPhiFunction basicBlock predecessors indexOf: sourcePredecessor.

	index = 0
		ifTrue: [ "Already resolved in parent edge: just propagate" 
			constraint := self parent constraintFor: aDRPhiFunction result name ]
		ifFalse: [ "Edge inmmediatly out from phi: search the constraint for nth operand"
			branchOperand := aDRPhiFunction operands size = 1
				                 ifTrue: [ aDRPhiFunction operands first ]
				                 ifFalse: [ aDRPhiFunction operands at: index ].
			constraint := self parent constraintFor: branchOperand result name ].

	^ constraint ifNotNil: [ 
		  self addConstraint: constraint to: aDRPhiFunction result name ]
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

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitStore: aDRLoad [ 

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitSubstract: aDRSubtract [ 

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitTest: aDRTest [

	^ self
		  visitComparison: aDRTest
		  andDo: [ :reg :value | DRRegisterDomain mask: reg with: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitTrueObject: aDRTrueObject [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitUnsignedCoercion: aDRUnsignedCoercion [ 

	^ nil
]
