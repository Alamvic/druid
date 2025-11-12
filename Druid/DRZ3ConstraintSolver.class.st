Class {
	#name : 'DRZ3ConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'cfg',
		'memo',
		'assertions',
		'ctxt',
		'signedIntOverflowIsUB',
		'useFloats',
		'visitedPhis'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph [

	^ self for: aDRControlFlowGraph signedIntOverflowIsUB: false
]

{ #category : 'instance creation' }
DRZ3ConstraintSolver class >> for: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean [

	^ self new for: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean
]

{ #category : 'z3' }
DRZ3ConstraintSolver >> adaptSize: op1 with: op2 andDo: aBlock [
	
	^ self adaptSize: op1 with: op2 signed: false andDo: aBlock
]

{ #category : 'z3' }
DRZ3ConstraintSolver >> adaptSize: op1 with: op2 signed: aBoolean andDo: aBlock [

	| _op1 _op2 selector |
	op1 isBitVector ifFalse: [ ^ aBlock value: op1 value: op2 ].

	selector := aBoolean
		            ifTrue: [ #signExtend: ]
		            ifFalse: [ #zeroExtend: ].
	_op1 := op1.
	_op2 := op2.
	op1 sort length < op2 sort length ifTrue: [
		_op1 := op1
			        perform: selector
			        with: op2 sort length - op1 sort length ].
	op2 sort length < op1 sort length ifTrue: [
		_op2 := op2
			        perform: selector
			        with: op1 sort length - op2 sort length ].

	^ aBlock value: _op1 value: _op2
]

{ #category : 'solving' }
DRZ3ConstraintSolver >> addZ3ConstraintFrom: aDRValue [

	aDRValue acceptVisitor: self
]

{ #category : 'z3' }
DRZ3ConstraintSolver >> drValAsZ3Val: aDRValue ifUnsupported: aBlock [

	| converter realValue |
	
	realValue := phiReplacements at: aDRValue ifAbsent: [aDRValue].
	converter := self
		             getZ3ValueConverterFor: realValue type
		             ifUnsupported: aBlock.
	
	^ converter value: (realValue isConstant
			   ifTrue: [ realValue value ]
			   ifFalse: [ realValue name ])
]

{ #category : 'initialization' }
DRZ3ConstraintSolver >> for: aDRControlFlowGraph signedIntOverflowIsUB: aBoolean [

	cfg := aDRControlFlowGraph.
	
	memo := IdentityDictionary new.
	
	signedIntOverflowIsUB := aBoolean.
	
	useFloats := false.
]

{ #category : 'z3' }
DRZ3ConstraintSolver >> getZ3ValueConverterFor: type ifUnsupported: aBlock [

	type isIntegerType ifTrue: [
		^ [ :val | val toBitVector: type numberOfBits ] ].

	type isBooleanType ifTrue: [ ^ [ :val | val toBool ] ].

	(useFloats and: [ type isFloatType ]) ifTrue: [
		^ [ :val | val toFloatingPoint: type numberOfBits ] ].

	(type isUndefined or: [ type isFloatType ]) ifTrue: [ ^ aBlock value ].

	self error: 'Unsupported type'
]

{ #category : 'solving' }
DRZ3ConstraintSolver >> initialize [

	super initialize.
	visitedPhis := IdentitySet new.
]

{ #category : 'solving' }
DRZ3ConstraintSolver >> isSatisfiable: aDRPiNode [

	| solver retval |
	assertions := OrderedCollection new.

	ctxt := Z3Context global.
	solver := ctxt mkSolver.

	self addZ3ConstraintFrom: aDRPiNode.

	assertions do: [ :a | solver assert: a ].
	retval := solver check.

	solver release.
	^ retval ifNil: [ true ]
]

{ #category : 'solving' }
DRZ3ConstraintSolver >> recurseOnlyPis: aDRValue addingConstraintsOn: varForConstraints [

	| currentVal constraint |
	currentVal := aDRValue.
	constraint := true toBool.

	[ currentVal isCopy or: [
		currentVal isPhiFunction and: [ currentVal operands size = 1 ] ] ] whileTrue: [
			currentVal isPiNode ifTrue: [
				constraint := constraint and:
					              (currentVal asZ3ConstraintOnVar: varForConstraints) ].
			currentVal := currentVal operand1
	].

	currentVal isConstant ifTrue: [
		constraint := constraint and: varForConstraints === currentVal value ].

	^ constraint
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitAddition: aDRAdd [

	(signedIntOverflowIsUB and: [
		 aDRAdd type isIntegerType and: [ aDRAdd type isSigned ] ]) ifTrue: [
		| op1 op2 |
		op1 := self drValAsZ3Val: aDRAdd operand1 ifUnsupported: [ ^ nil ].
		op2 := self drValAsZ3Val: aDRAdd operand2 ifUnsupported: [ ^ nil ].
		assertions add: (Z3 mk_bvadd_no_overflow: ctxt _: op1 _: op2 _: true).
		assertions add: (Z3 mk_bvadd_no_underflow: ctxt _: op1 _: op2) ].

	^ self visitBinaryOp: aDRAdd with: #+
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBinaryOp: aDRInstruction with: selector [

	| res op1 op2 afterOperation |
	aDRInstruction operands do: [ :op | self addZ3ConstraintFrom: op ].

	res := self drValAsZ3Val: aDRInstruction ifUnsupported: [ ^ nil ].
	op1 := self
		       drValAsZ3Val: aDRInstruction operand1
		       ifUnsupported: [ ^ nil ].
	op2 := self
		       drValAsZ3Val: aDRInstruction operand2
		       ifUnsupported: [ ^ nil ].

	self
		adaptSize: op1
		with: op2
		signed: aDRInstruction type isSigned
		andDo: [ :adaptedOp1 :adaptedOp2 |
			afterOperation := adaptedOp1 perform: selector with: adaptedOp2.
			self
				adaptSize: res
				with: afterOperation
				signed: aDRInstruction type isSigned
				andDo: [ :_res :_afterOp | assertions add: _res === _afterOp ] ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBitAnd: aDRBitAnd [

	^ self visitBinaryOp: aDRBitAnd with: #&
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitBitXor: aDRBitXor [ 
	
	^ self visitBinaryOp: aDRBitXor with: #bitXor:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCall: aDRCall [ 
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitConstant: aDRConstantValue [

	aDRConstantValue value ifNil: [ ^ nil ].
	assertions add:
		(self drValAsZ3Val: aDRConstantValue ifUnsupported: [ ^ nil ])
		=== aDRConstantValue value
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitCopy: aDRCopy [
	| res op |
	
	aDRCopy dependencies do: [ :dep | self addZ3ConstraintFrom: dep ].

	res := self drValAsZ3Val: aDRCopy ifUnsupported: [ ^ nil ].
	op := self drValAsZ3Val: aDRCopy operand ifUnsupported: [ ^ nil ].

	assertions add: res === op.

	aDRCopy isPiNode ifTrue: [ assertions add: (aDRCopy asZ3Constraint: self) ]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitDivision: aDRDivision [

	^ self visitBinaryOp: aDRDivision with: #/
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitGetConditionCode: aDRGetConditionCode [

	| res piNodeBuilder piNode assertion operand piNodes |
	res := self drValAsZ3Val: aDRGetConditionCode ifUnsupported: [ ^ nil ].

	piNodeBuilder := DRPiNodeBuilder withInstructionFactory:
		                 cfg instructionFactory.
	aDRGetConditionCode condition
		acceptVisitor: piNodeBuilder
		withBranch: aDRGetConditionCode.

	piNodes := piNodeBuilder nodesForTrueBranch.
	piNode := piNodes at: 1 ifAbsent: [ ^ nil ].
	operand := self drValAsZ3Val: piNode operand ifUnsupported: [ ^ nil ].

	assertion := res === (piNode asZ3ConstraintOnVar: operand).

	assertions add: assertion.

	piNodes do: [ :pi |
		pi dependencies copy do: [ :each | each removeUser: pi ] ].

]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitGetFloatBits: aDRGetFloatBits [

	| res op1 |
	res := self drValAsZ3Val: aDRGetFloatBits ifUnsupported: [ ^ nil ].
	self addZ3ConstraintFrom: aDRGetFloatBits operand1.
	
	op1 := self
		       drValAsZ3Val: aDRGetFloatBits operand1
		       ifUnsupported: [ ^ nil ].
	assertions add: res === op1 toBitVector
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitGetFloatFromBits: aDRGetFloatFromBits [

	| res op1 sort opAsFloat |
	res := self
		       drValAsZ3Val: aDRGetFloatFromBits
		       ifUnsupported: [ ^ nil ].
	op1 := self
		       drValAsZ3Val: aDRGetFloatFromBits operand1
		       ifUnsupported: [ ^ nil ].
	op1 sort length ~= 64 ifTrue: [
		self error.
		"op1 := op1 zeroExtend: 64 - op1 sort length" ].
	self addZ3ConstraintFrom: aDRGetFloatFromBits operand1.

	sort := ctxt mkFloatingPointSort64.
	opAsFloat := (Z3 mk_fpa_to_fp_bv: ctxt _: op1 _: sort) simplify.
	assertions add: res === opAsFloat
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitIntToFloatConversion: aDRIntToFloat [

	self flag: #TODO
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitInterpreterReference: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLeftRotate: aDRRotateLeft [

	| constraints res op1 op2 |
	constraints := aDRRotateLeft operands do: [ :op |
		               self addZ3ConstraintFrom: op ].

	res := self drValAsZ3Val: aDRRotateLeft ifUnsupported: [ ^ nil ].
	op1 := self drValAsZ3Val: aDRRotateLeft operand1 ifUnsupported: [ ^ nil ].
	op2 := aDRRotateLeft operand2 simpleConstantFold.
	op2 isNumber ifFalse: [ ^nil ].

	assertions add: res === (op1 bitRotateLeft: op2)
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLeftShift: aDRLeftShift [

	^ self visitBinaryOp: aDRLeftShift with: #bitShiftLeft:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoad: aDRLoad [

	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadArgument: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadReceiver: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadStackPointer: aDRLoadStackPointer [ 
	
	"Nothing"
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitLoadStackValue: aDRLoadStackValue [ 
	self visitLoad: aDRLoadStackValue 
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitMod: aDRMod [ 
	
	"Assumes remainder of signed integer division"
	^ self visitBinaryOp: aDRMod with: #srem:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitMultiply: aDRMultiply [

	^ self visitBinaryOp: aDRMultiply with: #*
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitNullValue: _ [
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitPhiFunction: aDRPhiFunction [

	| res opsConstraints type |
	(visitedPhis includes: aDRPhiFunction) ifTrue: [ ^nil ].
	
	type := aDRPhiFunction type.
	self visitingPhi: aDRPhiFunction do: [
		res := self drValAsZ3Val: aDRPhiFunction ifUnsupported: [ ^ nil ].

	
		phiReplacements at: aDRPhiFunction ifPresent: [ :phiReplacement | | replacementZ3 |
			replacementZ3 := self drValAsZ3Val: phiReplacement ifUnsupported: [ ^ nil ].
			self addZ3ConstraintFrom: phiReplacement.
			self
				adaptSize: res
				with: replacementZ3
				signed: (type isIntegerType and: [ type isSigned])
				andDo: [ :_res :_replacementZ3 |
					assertions add: _res === _replacementZ3 ].
		] ifAbsent: [
			opsConstraints := aDRPhiFunction operands collect: [ :op |
		                  self recurseOnlyPis: op addingConstraintsOn: res ].

			assertions add: (opsConstraints reduce: [ :a :b | a or: b ])
		].
	]
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitSignedCoercion: aDRSignedCoercion [
	
	| res op |
	res := self drValAsZ3Val: aDRSignedCoercion ifUnsupported: [ ^ nil ].
	op := self drValAsZ3Val: aDRSignedCoercion operand1 ifUnsupported: [ ^ nil ].

	"Z3 bitvectors are unsigned, and you treat them as positive/negative when you operate on them"
	self adaptSize: res with: op signed: true andDo: [ :_res :_op | assertions add: _res === _op ].
	
	self addZ3ConstraintFrom: aDRSignedCoercion operand1.
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitSignedRightShift: aDRSignedRightShift [

	^ self visitBinaryOp: aDRSignedRightShift with: #bitShiftRightArithmetic:
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitSubstract: aDRSubtract [

	(signedIntOverflowIsUB and: [
		 aDRSubtract type isIntegerType and: [ aDRSubtract type isSigned ] ]) ifTrue: [
		| op1 op2 |
		op1 := self drValAsZ3Val: aDRSubtract operand1 ifUnsupported: [ ^ nil ].
		op2 := self drValAsZ3Val: aDRSubtract operand2 ifUnsupported: [ ^ nil ].
		assertions add: (Z3 mk_bvsub_no_overflow: ctxt _: op1 _: op2).
		assertions add: (Z3 mk_bvsub_no_underflow: ctxt _: op1 _: op2 _: true) ].

	^ self visitBinaryOp: aDRSubtract with: #-
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitUnsignedCoercion: aDRUnsignedCoercion [

	| res op |
	res := self drValAsZ3Val: aDRUnsignedCoercion ifUnsupported: [ ^ nil ].
	op := self drValAsZ3Val: aDRUnsignedCoercion operand1 ifUnsupported: [ ^ nil ].

	"Z3 bitvectors are unsigned, and you treat them as positive/negative when you operate on them"
	self adaptSize: res with: op signed: false andDo: [ :_res :_op | assertions add: _res === _op ].
	
	self addZ3ConstraintFrom: aDRUnsignedCoercion operand1.
	
]

{ #category : 'visiting' }
DRZ3ConstraintSolver >> visitUnsignedRightShift: aDRUnsignedRightShift [

	^ self visitBinaryOp: aDRUnsignedRightShift with: #bitShiftRight:
]

{ #category : 'as yet unclassified' }
DRZ3ConstraintSolver >> visitingPhi: aDRPhiFunction do: aBlock [ 
	
	visitedPhis add: aDRPhiFunction.
	aBlock value.
	visitedPhis remove: aDRPhiFunction.
]
