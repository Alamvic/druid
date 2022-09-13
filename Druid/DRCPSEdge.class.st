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

	super initialize.
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
	^ constraints anySatisfy: [ :c | c isEmpty ]
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

	| constraintOrDomain |
	(self constraintFor: operand result name) ifNotNil: [ :c | ^ c ].
	constraintOrDomain := operand acceptVisitor: self.
	constraintOrDomain ifNil: [ ^ nil ].

	"As comparisons add constraints to operands (not result) registers they return a register domain"
	^ operand isSetConditionCode
		  ifTrue: [ constraintOrDomain constraint ]
		  ifFalse: [ constraintOrDomain ]
]

{ #category : #visiting }
DRCPSEdge >> operandDomainFromInstruction: aDRInstruction [

	aDRInstruction isCopy ifTrue: [ 
		| constraint |
		constraint := self constraintFor: aDRInstruction result name.
		^ constraint
			  ifNil: [ 
			  self operandDomainFromInstruction: aDRInstruction operand1 ]
			  ifNotNil: [ 
			  DRRegisterDomain reg: aDRInstruction constraint: constraint ] ].

	aDRInstruction isPhiFunction ifTrue: [ 
		^ self operandDomainFromPhi: aDRInstruction ].

	aDRInstruction isSetConditionCode ifTrue: [ 
		^ aDRInstruction acceptVisitor: self ].

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> operandDomainFromPhi: aDRPhiFunction [

	| pathOperand constraint |
	pathOperand := aDRPhiFunction operands
		               detect: [ :op | constraints includesKey: op result name ]
		               ifNone: [ ^ nil ].
	constraint := self operandConstraint: pathOperand.
	^ DRRegisterDomain reg: pathOperand constraint: constraint
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
DRCPSEdge >> visitBitXor: aDRBitOr [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitBranchIfCondition: aDRConditionalJump [

	| domain id |
	"Ignore hardcoded values"
	domain := aDRConditionalJump condition acceptVisitor: self withBranch: aDRConditionalJump.
	domain ifNil: [ ^ self ].

	id := domain reg result name.

	^ branchIndex = 1
		  ifTrue: [ self mergeConstraint: domain constraint to: id ]
		  ifFalse: [ self mergeConstraint: domain constraint negated to: id ]
]

{ #category : #visiting }
DRCPSEdge >> visitBranchIfTrue: aDRBranchIfTrue [

	"Ignore hardcoded values"

	| domain constraint |
	aDRBranchIfTrue operand1 isInstruction ifFalse: [ ^ self ].

	domain := self operandDomainFromInstruction: aDRBranchIfTrue operand1.
	domain ifNil: [ ^ self ].

	constraint := DREqualsConstraint withValue: true. 

	branchIndex = 1
		ifTrue: [ self mergeConstraint: constraint to: domain reg name ]
		ifFalse: [ self mergeConstraint: constraint negated to: domain reg name ]
]

{ #category : #visiting }
DRCPSEdge >> visitComparison: aDRComparison ofBranch: aDRBranch andDo: aClosure [

	| operands |
	operands := aDRBranch operands.
	((operands anySatisfy: #isConstant) and: [ 
		 operands anySatisfy: [ :o | o isRegister or: [ o isInstruction ] ] ]) 
		ifFalse: [ ^ nil "We need a register and a constant" ].

	"Check constant on right"
	"if not reverse it and add it directly"
	operands second isConstant ifFalse: [ 
		^ (aClosure
			   value: operands second
			   value: (operands first acceptVisitor: self)) reversed ].

	^ aClosure
		  value: operands first
		  value: (operands second acceptVisitor: self)
]

{ #category : #visiting }
DRCPSEdge >> visitConstant: aDRConstantValue [ 
	
	^ aDRConstantValue 
]

{ #category : #visiting }
DRCPSEdge >> visitCopy: aDRCopy [

	| constraint operand |
	operand := aDRCopy operand1.

	constraint := (operand isConstant or: [ 
		               operand isObjectReferenceValue ])
		              ifTrue: [ DREqualsConstraint withValue: operand ]
		              ifFalse: [ self operandConstraint: operand ].

	constraint ifNil: [ ^ nil ].

	^ self addConstraint: constraint to: aDRCopy result name
]

{ #category : #visiting }
DRCPSEdge >> visitDivision: aDRDivision [ 

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranch [

	^ self
		  visitComparison: aDREqualsThanComparison
		  ofBranch: aDRBranch
		  andDo: [ :reg :value | DRRegisterDomain equals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitFalseObject: aDRFalseObject [

	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitFloatToIntConversion: aDRFloatToInt [ 
	
	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitGetConditionCode: aDRGetConditionCode [

	^ aDRGetConditionCode condition
		  acceptVisitor: self
		  withBranch: aDRGetConditionCode
]

{ #category : #visiting }
DRCPSEdge >> visitGetFloatBits: aDRGetFloatBits [ 
	
	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitGetFloatFromBits: aDRGetFloatFromBits [ 
	
	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitGreaterOrEqualsThan: aDRGreaterOrEqualsThanComparison inBranch: aDRBranch [

	^ self
		  visitComparison: aDRGreaterOrEqualsThanComparison
		  ofBranch: aDRBranch
		  andDo: [ :reg :value | 
		  DRRegisterDomain greaterOrEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitGreaterThan: aDRGreaterThanComparison inBranch: aDRBranch [

	^ self
		  visitComparison: aDRGreaterThanComparison
		  ofBranch: aDRBranch
		  andDo: [ :reg :value | DRRegisterDomain greater: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitIntToFloatConversion: aDRIntToFloat [ 
	
	^ nil
]

{ #category : #visiting }
DRCPSEdge >> visitJITCompileTimeExpression: aDRJitCompileTimeExpression [
	
	^ aDRJitCompileTimeExpression expression
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
DRCPSEdge >> visitLessOrEqualsThan: aDRLessOrEqualsThanComparison inBranch: aDRBranch [

	^ self
		  visitComparison: aDRLessOrEqualsThanComparison
		  ofBranch: aDRBranch
		  andDo: [ :reg :value | 
		  DRRegisterDomain lessOrEquals: reg than: value ]
]

{ #category : #visiting }
DRCPSEdge >> visitLessThan: aDRLessThanComparison inBranch: aDRBranch [

	^ self
		  visitComparison: aDRLessThanComparison
		  ofBranch: aDRBranch
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
DRCPSEdge >> visitMultiplyOverflow: aDROverflow inBranch: aDRBranch [ 
	
	self flag: #TODO. "Do nothing?"
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
DRCPSEdge >> visitNotEqualsThan: aDRNotEqualsThanComparison inBranch: aDRBranch [ 
	
	^ self
		  visitComparison: aDRNotEqualsThanComparison
		  ofBranch: aDRBranch
		  andDo: [ :reg :value | DRRegisterDomain notEquals: reg than: value ]
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
DRCPSEdge >> visitOverflow: aDROverflow inBranch: aDRBranch [ 
	
	self flag: #TODO. "Do nothing?"
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
DRCPSEdge >> visitTestMask: aDRTestMask inBranch: aDRBranch [

	^ self
		  visitComparison: aDRTestMask
		  ofBranch: aDRBranch
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
