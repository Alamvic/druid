Class {
	#name : #DRCogitCanonicaliser,
	#superclass : #DROptimisation,
	#category : #'Druid-Cogit'
}

{ #category : #accessing }
DRCogitCanonicaliser >> applyTo: cfg [

	cfg instructions copy do: [ :e | e acceptVisitor: self ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitAddition: aDRAdd [

	(aDRAdd operands anySatisfy: [ :op | op = 0 asDRValue ]) ifTrue: [ 
		^ aDRAdd replaceBy: (DRCopy
				   operands: (aDRAdd operands select: [ :e | e ~= 0 asDRValue ])
				   result: aDRAdd result) ].

	"Simplify only if we have a constant as operand"
	(aDRAdd operands noneSatisfy: [ :opop | opop isConstant ])
		ifTrue: [ ^ self ].

	"If it is a pattern such as res := ((a + c1) + c2) => res := (a + (c1+c2))"
	aDRAdd operands
		detect: [ :op | 
			op isAdd and: [ 
				(op operands anySatisfy: [ :opop | opop isConstant ])
					and: [ op operands anySatisfy: [ :opop | opop isConstant not ] ] ] ]
		ifFound: [ :subtree | | variableOperand subConstantOperand constantOperand |
			variableOperand := subtree operands detect: [ :op | op isConstant not ].
			subConstantOperand := subtree operands detect: [ :op | op isConstant ].
			constantOperand := aDRAdd operands detect: [ :op | op isConstant ].
			^ aDRAdd replaceBy: (DRAdd
					   operands: { variableOperand . (subConstantOperand value + constantOperand value) asDRValue }
					   result: aDRAdd result) ]
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitBitAnd: aDRBitAnd [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitCopy: aDRCopy [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLeftShift: aDRLeftShift [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoad: aDRLoad [

	| address addressOperands offsetOperand baseOperand addressExpression |
	address := aDRLoad address.
	(address isAbsolute and: [ address expression isAdd ]) ifFalse: [ ^ self ].
	addressExpression := address expression.

	"Cogit Specific optimisation: collapse
		r0 := load framePointerRegister + receiver offset
	into
		r0 := load receiverRegister"
	addressOperands := addressExpression operands.
	(addressOperands first isLoadFramePointer and: [ 
		 addressOperands second isJITCompileTimeExpression:
			 'FrameReceiverOffset' ]) ifTrue: [ 
		^ aDRLoad replaceBy: (DRLoadReceiver result: aDRLoad result) ].

	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := load r1
	=>
	r2 := load r0 + c1
	"
	offsetOperand := addressOperands
		                 detect: [ :op | op isConstant ]
		                 ifNone: [ ^ self ].
	baseOperand := addressOperands
		               detect: [ :op | op isConstant not ]
		               ifNone: [ ^ self ].
	
	^ aDRLoad replaceOperand: address by: (DRBaseOffsetMemoryAddress new
		base: baseOperand;
		offset: offsetOperand;
		yourself)
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadReceiver: aDRLoadReceiver [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitLoadStackValue: aDRLoadStackValue [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitNoop: aDRNoop [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitPush: aDRPush [
	
	| pushedValue |
	pushedValue := aDRPush operand1.
	pushedValue isLoad ifFalse: [ ^ self ].
	aDRPush replaceOperand: pushedValue by: pushedValue address
]

{ #category : #visiting }
DRCogitCanonicaliser >> visitStore: aDRStore [ 
	
	"Nothing"
]
