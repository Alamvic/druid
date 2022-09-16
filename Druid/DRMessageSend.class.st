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

{ #category : #inlining }
DRMessageSend >> generateInlineIRForMethod: methodToInline [

	| lastFrame |
	lastFrame := irGenerator
		             interpretCode:
		             (DRMethod methodNode: methodToInline)
		             receiver: operands first
		             arguments: operands allButFirst.
	^ lastFrame returnValue
]

{ #category : #inlining }
DRMessageSend >> generateInlineIRForMethods: methodsToInline [

	| returnValues lastBlock |
	returnValues := OrderedCollection new.
	methodsToInline associations allButLastDo: [ :classAndMethod | 
		| jump |
		jump := irGenerator currentBasicBlock endInstruction:
			        (DRBranchIfCondition operands: { 
					         (DRTypeOf typeClass: classAndMethod key).
					         self receiver }).
		jump newTrueBranch: irGenerator newBasicBlock.
		returnValues add:
			(self generateInlineIRForMethod: classAndMethod value).
		jump newFalseBranch: irGenerator newBasicBlock ].

	returnValues add:
		(self generateInlineIRForMethod: methodsToInline values last).
	
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

	pieces := self basicBlock breakBy: self.

	pieces first jumpTo: irGenerator newFirstBasicBlock.

	irGenerator numberOfArguments: operands size - 1.
	
	inlinedMethodReturnValue := self generateInlineIRForMethods: methodsToInline.

	irGenerator currentBasicBlock jumpTo: pieces last.
	"Remove message send block"
	self replaceUsesBy: inlinedMethodReturnValue.
	pieces last removePredecessor: self basicBlock.
	self basicBlock removeFromCFG
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
		ifNotNil: [ {(#default -> methodNode)} asDictionary ]
		ifNil: [ 
		  inlinedMethodClasses := self receiverType.
		  (inlinedMethodClasses collect: [ :class | 
			   class -> (class lookupSelector: self originAST selector) ast ]) asDictionary ]
]

{ #category : #printing }
DRMessageSend >> opcode [
	
	^ 'SEND'
]

{ #category : #accessing }
DRMessageSend >> receiver [

	^ self operand1
]

{ #category : #inlining }
DRMessageSend >> receiverType [

	| typePragmas receiverName receiverTypePragmas |
	typePragmas := self originAST methodNode allPragmasNamed: #var:type:.
	receiverName := self originAST receiver name.
	receiverTypePragmas := typePragmas select: [ :pragma | pragma arguments first value = receiverName ].
	^ receiverTypePragmas collect: [ :pragma | self class environment at: (pragma arguments last value) ]
]

{ #category : #SCCP }
DRMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"

	
]

{ #category : #types }
DRMessageSend >> type [

	(methodNode hasPragmaNamed: #returnTypeC:) ifTrue: [ "TODO: Match Slang types" 
		1 halt ].

	"Default type"
	^ DRIntegerType new
]

{ #category : #copying }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
