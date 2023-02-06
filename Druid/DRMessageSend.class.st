Class {
	#name : #DRMessageSend,
	#superclass : #DRInstruction,
	#instVars : [
		'methodNode',
		'irGenerator'
	],
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRMessageSend >> acceptVisitor: aVisitor [

	^ aVisitor visitMessageSend: self
]

{ #category : #accessing }
DRMessageSend >> arguments [

	^ operands allButFirst
]

{ #category : #inlining }
DRMessageSend >> generateInlineIRForMethod: classAndMethodToInline [

	| lastFrame methodClass methodToInline |
	"TODO: delegate generation to type?"
	methodClass := classAndMethodToInline key.
	(methodClass isKindOf: DRUndefinedType) ifTrue: [ "If we don't know the receiver's type, copy the message send"
		(DRMethod methodNode: self originAST methodNode) asDRValue
			pushFrameIn: irGenerator
			receiver: self receiver.
		^ irGenerator resolveMessageSend: self originAST receiver: self receiver arguments: self arguments ].

	methodToInline := classAndMethodToInline value.
	lastFrame := irGenerator
		             interpretCode: (DRMethod methodNode: methodToInline)
		             receiver: operands first
		             arguments: operands allButFirst.

	irGenerator finishCodeInterpretation: lastFrame.

	self flag: #TOFIX.
	^ lastFrame mirrorFrame returnValue ifNil: [ lastFrame returnValue ]
]

{ #category : #inlining }
DRMessageSend >> generateInlineIRForMethods: methodsToInline [

	| returnValues lastBlock |
	returnValues := OrderedCollection new.
	methodsToInline allButLastDo: [ :classAndMethod | 
		| jump |
		jump := irGenerator currentBasicBlock endInstruction:
			        (DRBranchIfCondition operands: { 
					         (DRTypeOf typeClass: classAndMethod key).
					         self receiver }).
		jump newTrueBranch: irGenerator newBasicBlock.
		returnValues add:
			(self generateInlineIRForMethod: classAndMethod).
		jump newFalseBranch: irGenerator newBasicBlock ].

	returnValues add:
		(self generateInlineIRForMethod: methodsToInline last).
	
	returnValues size = 1 ifTrue: [ ^ returnValues first ].
	
	lastBlock := irGenerator newBasicBlock.

	returnValues do: [ :returnValue | 
		returnValue basicBlock jumpTo: lastBlock ].
	^ lastBlock addInstruction:
		  (self instructionFactory phiWithVariables: returnValues)
]

{ #category : #inlining }
DRMessageSend >> inline [

	| pieces methodsToInline inlinedMethodReturnValue |
	methodsToInline := self methodsToInline.
	methodsToInline ifEmpty: [ ^ self ].

	"Break message send basic block"
	pieces := self basicBlock breakBy: self.
	pieces first jumpTo: irGenerator newFirstBasicBlock.

	"Generate inlining methods IR"
	irGenerator numberOfArguments: operands size - 1.
	inlinedMethodReturnValue := self generateInlineIRForMethods: methodsToInline.

	"Replace usages by inlines result"
	irGenerator currentBasicBlock jumpTo: pieces last.
	self replaceUsesBy: inlinedMethodReturnValue.
	
	"Remove message send block"
	pieces last removePredecessor: self basicBlock.
	self basicBlock removeFromCFG.
	
	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: irGenerator ir.
	DRDeadCodeElimination new applyTo: irGenerator ir.
	irGenerator ir validate.

]

{ #category : #accessing }
DRMessageSend >> irGenerator: anIrGenerator [ 

	irGenerator := anIrGenerator
]

{ #category : #accessing }
DRMessageSend >> isMandatoryInstruction [

	^ true
]

{ #category : #accessing }
DRMessageSend >> isMessageSend [

	^ true
]

{ #category : #accessing }
DRMessageSend >> methodNode [

	^ methodNode
]

{ #category : #accessing }
DRMessageSend >> methodNode: anObject [

	methodNode := anObject
]

{ #category : #inlining }
DRMessageSend >> methodsToInline [

	| inlinedMethodClasses |
	^ methodNode
		ifNotNil: [ {(#default -> methodNode)} ]
		ifNil: [ 
		  inlinedMethodClasses := self receiverType.
		  (inlinedMethodClasses collect: [ :class | 
				class -> (class astForSelector: self selector) ]) ]
]

{ #category : #printing }
DRMessageSend >> opcode [

	^ '#' , self selector , ' '
]

{ #category : #accessing }
DRMessageSend >> receiver [

	^ self operand1
]

{ #category : #inlining }
DRMessageSend >> receiverType [

	| typePragmas receiverName receiverTypePragmas receiverTypes |
	typePragmas := self originAST methodNode allPragmasNamed: #var:type:.
	receiverName := self originAST receiver name.
	receiverTypePragmas := typePragmas select: [ :pragma | 
		                       pragma arguments first value = receiverName ].
	receiverTypes := receiverTypePragmas collect: [ :pragma | pragma arguments last value.].
	"Unknown should always be the last type, as it's used as a fallback when inlining"
	receiverTypes detect: [:type | type = #Unknown] ifFound: [
		receiverTypes remove: #Unknown.
		receiverTypes add: #Unknown.
		].
	^ receiverTypes collect: [ :type |
		  type = #Unknown
			  ifTrue: [ DRUndefinedType new ]
			  ifFalse: [ DRClassType for: (self class environment at: type) ] ]
]

{ #category : #SCCP }
DRMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"

	
]

{ #category : #accessing }
DRMessageSend >> selector [

	^ self originAST selector
]

{ #category : #testing }
DRMessageSend >> supportConstantOperand [

	^ false
]

{ #category : #types }
DRMessageSend >> type [

	methodNode ifNotNil: [ "TODO: Connext with type system"
		(methodNode hasPragmaNamed: #returnTypeC:) ifTrue: [ "TODO: Match Slang types" 
			1 halt ] ].

	^ DRUndefinedType new
]

{ #category : #copying }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
