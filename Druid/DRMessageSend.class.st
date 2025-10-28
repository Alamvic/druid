Class {
	#name : 'DRMessageSend',
	#superclass : 'DRInstruction',
	#instVars : [
		'methodNode',
		'inlineGenerator'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRMessageSend >> acceptVisitor: aVisitor [

	^ aVisitor visitMessageSend: self
]

{ #category : 'accessing' }
DRMessageSend >> arguments [

	^ operands allButFirst
]

{ #category : 'accessing' }
DRMessageSend >> dependencies [

	^ super dependencies , self innerDependencies
]

{ #category : 'testing' }
DRMessageSend >> hasBlockArgument [

	^ self arguments anySatisfy: [ :arg | arg isDRBlockClosure ]
]

{ #category : 'optimizations' }
DRMessageSend >> inline [
	
	"Used by the visualization"
	
	self controlFlowGraph applyOptimisation: (DRSingleInline forMessageSend: self)
]

{ #category : 'accessing' }
DRMessageSend >> inlineGenerator [

	^ inlineGenerator
]

{ #category : 'accessing' }
DRMessageSend >> inlineGenerator: anIrGenerator [ 

	inlineGenerator := anIrGenerator
]

{ #category : 'inline' }
DRMessageSend >> innerDependencies [

	inlineGenerator ifNil: [ ^ #(  ) ].
	^ (inlineGenerator allTemporaryInstructions reject: #isDRBlockClosure)
		  copyWithoutAll: self operands
]

{ #category : 'accessing' }
DRMessageSend >> isMandatoryInstruction [

	^ true
]

{ #category : 'accessing' }
DRMessageSend >> isMessageSend [

	^ true
]

{ #category : 'types' }
DRMessageSend >> isScopedInstruction [

	^ self hasBlockArgument
]

{ #category : 'accessing' }
DRMessageSend >> methodNode [

	^ methodNode
]

{ #category : 'accessing' }
DRMessageSend >> methodNode: anObject [

	methodNode := anObject
]

{ #category : 'printing' }
DRMessageSend >> opcode [

	^ '#' , self selector , ' '
]

{ #category : 'accessing' }
DRMessageSend >> receiver [

	^ self operand1
]

{ #category : 'types' }
DRMessageSend >> receiverType [

	(self receiver type lookupSelector: self selector) ifNil: [ "Check if the receiver type understands me, if not then fallback to Undefined type"
		^ DRUndefinedType new ].

	^ self receiver type
]

{ #category : 'replacing' }
DRMessageSend >> replaceDependency: anOperand by: anotherOperand [

	(self innerDependencies includes: anOperand) ifTrue: [
		self replaceInnerDependencie: anOperand by: anotherOperand ].

	super replaceDependency: anOperand by: anotherOperand
]

{ #category : 'replacement' }
DRMessageSend >> replaceInnerDependencie: anInstructionDependency by: anotherInstruction [

	self inlineGenerator executionStack
		replaceTemporaries: anInstructionDependency
		by: anotherInstruction
]

{ #category : 'SCCP' }
DRMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"
]

{ #category : 'SCCP' }
DRMessageSend >> sccpLatticeValueFor: sccp [ 
	
	^ sccp bottom
]

{ #category : 'accessing' }
DRMessageSend >> selector [

	^ self originAST selector
]

{ #category : 'testing' }
DRMessageSend >> supportConstantOperand [

	^ false
]

{ #category : 'types' }
DRMessageSend >> typeWithAlreadySeen: _ [

	methodNode ifNotNil: [ "TODO: Connext with type system"
		(methodNode hasPragmaNamed: #returnTypeC:) ifTrue: [ "TODO: Match Slang types"
			1 halt ] ].

	^ DRUndefinedType new
]

{ #category : 'copying' }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
