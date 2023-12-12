Class {
	#name : #DRCogitStackCanonicaliser,
	#superclass : #DRCogitCanonicaliser,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitStackCanonicaliser >> visitLoad: aDRLoad [

	| address addressOperands offsetOperand baseOperand addressExpression |
	address := aDRLoad address.
	(address isAbsolute and: [ address expression isAdd ]) ifFalse: [ ^ self ].
	addressExpression := address expression.

	"Cogit Specific optimisation: collapse
		r0 := load framePointerRegister + receiver offset
	into
		r0 := load receiverRegister"
	addressOperands := addressExpression operands.
	(addressOperands first simpleConstantFold asDRValue isLoadFramePointer and: [
		 addressOperands second simpleConstantFold asDRValue isJITCompileTimeExpression:
			 'FrameReceiverOffset' ]) ifTrue: [
		^ aDRLoad replaceBy: (DRLoadReceiver result: aDRLoad result) ].

	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := load r1
	=>
	r2 := load r0 + c1
	"
	offsetOperand := addressOperands detect: [ :op | op isConstant ] ifNone: [ ^ self ].
	baseOperand := addressOperands detect: [ :op | op isConstant not ] ifNone: [ ^ self ].

	^ aDRLoad replaceOperand: address by: (DRBaseOffsetMemoryAddress new
			   base: baseOperand;
			   offset: offsetOperand;
			   yourself)
]

{ #category : #visiting }
DRCogitStackCanonicaliser >> visitStore: aDRStore [ 
	
	"General optimisation:
	Simplify for now only patterns like
	r1 := r0 + c1.
	r2 := store value r1
	=>
	r2 := store value r0 + c1
	"
	| address addressExpression addressOperands offsetOperand baseOperand |

	address := aDRStore address.
	(address isAbsolute and: [ address expression isAdd ]) ifFalse: [ ^ self ].

	addressExpression := address expression.
	addressOperands := addressExpression operands.
	offsetOperand := addressOperands
		                 detect: [ :op | op isConstant ]
		                 ifNone: [ ^ self ].
	baseOperand := addressOperands
		               detect: [ :op | op isConstant not ]
		               ifNone: [ ^ self ].
	
	^ aDRStore replaceOperand: address by: (DRBaseOffsetMemoryAddress new
		base: baseOperand;
		offset: offsetOperand;
		yourself)
]
