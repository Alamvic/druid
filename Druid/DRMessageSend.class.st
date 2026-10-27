Class {
	#name : 'DRMessageSend',
	#superclass : 'DRInstruction',
	#instVars : [
		'methodNode',
		'parentScope'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRMessageSend class >> operands: ops [

	self haltIf: [ ops first value = nil ].
	super operands: ops.
]

{ #category : 'visiting' }
DRMessageSend >> acceptVisitor: aVisitor [

	^ aVisitor visitMessageSend: self
]

{ #category : 'accessing' }
DRMessageSend >> arguments [

	^ operands allButFirst
]

{ #category : 'testing' }
DRMessageSend >> blockOperandCount [

	^ self operands count: [ :arg | arg simpleConstantFold isBlock ]
]

{ #category : 'optimizations' }
DRMessageSend >> inline [
	
	"Used by the visualization"
	
	self controlFlowGraph applyOptimisation: (DRSingleInline forMessageSend: self)
]

{ #category : 'accessing' }
DRMessageSend >> inlinedScope [

	"Search the inlined scope (between inlining and inlined methods) from the parent.
	  Returns nil if this send was not inlined"

	^ parentScope inlinedScopes
		  detect: [ :scope | scope node = self ]
		  ifNone: [ nil "error?" ]
]

{ #category : 'accessing' }
DRMessageSend >> isMandatoryInstruction [

	^ true
]

{ #category : 'accessing' }
DRMessageSend >> isMessageSend [

	^ true
]

{ #category : 'testing' }
DRMessageSend >> isMethod [

	^ false
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
DRMessageSend >> parentScope [

	^ parentScope
]

{ #category : 'accessing' }
DRMessageSend >> parentScope: aDRScope [

	parentScope := aDRScope
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
DRMessageSend >> typeWithAlreadySeen: aCollection [

	^ DRUndefinedType new
]

{ #category : 'copying' }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
