Class {
	#name : #DRSuperInstructionCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#instVars : [
		'nextBytecodes',
		'currentBytecode',
		'interpreter',
		'compilerClass'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #factory }
DRSuperInstructionCustomisation class >> interpreter: anInterpreter compilerClass: compilerClass currentBytecode: currentBytecode nextBytecodes: nextBytecodes [

	^ self new
		  interpreter: anInterpreter;
		  compilerClass: compilerClass;
		  currentBytecode: currentBytecode;
		  nextBytecodes: nextBytecodes;
		  yourself
]

{ #category : #'accessing - bytecodes' }
DRSuperInstructionCustomisation >> bytecodeSelector: bytecode [

	^ self bytecodeTable at: bytecode + 1
]

{ #category : #'accessing - bytecodes' }
DRSuperInstructionCustomisation >> bytecodeSizeOf: bytecode [

	bytecode < 224 ifTrue: [ ^ 1 ].
	bytecode < 248 ifTrue: [ ^ 2 ].
	^ 3
]

{ #category : #'class initialization' }
DRSuperInstructionCustomisation >> bytecodeTable [

	^ interpreter class bytecodeTable
]

{ #category : #'ast-nodes' }
DRSuperInstructionCustomisation >> caseNodeFor: nextBytecode [

	^ RBMessageNode
		  receiver: (RBMessageNode
				   receiver: (RBVariableNode named: 'nextBytecode')
				   selector: #=
				   arguments: { nextBytecode asLiteralNode })
		  selector: #ifTrue:
		  arguments: { (RBBlockNode body: (RBSequenceNode statements: {
						    (RBAssignmentNode
							     variable: (RBVariableNode named: 'bytecodePC')
							     value: self nextBytecodePCNode).
						    (RBReturnNode value: (RBMessageNode
								      receiver: RBVariableNode selfNode
								      selector:
								      (self selectorForSuperInstruction: nextBytecode))) })) }
]

{ #category : #accessing }
DRSuperInstructionCustomisation >> compilerClass: aClass [

	compilerClass := aClass
]

{ #category : #accessing }
DRSuperInstructionCustomisation >> currentBytecode: bytecode [

	currentBytecode := bytecode
]

{ #category : #'accessing - bytecodes' }
DRSuperInstructionCustomisation >> currentBytecodeSize [

	^ self bytecodeSizeOf: currentBytecode 
]

{ #category : #'ast-nodes' }
DRSuperInstructionCustomisation >> fetchNextBytecodeNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: 'nextBytecode')
		  value: (RBMessageNode
				   receiver: (RBVariableNode named: 'objectMemory')
				   selector: #fetchByte:ofObject:
				   arguments: {
						   self nextBytecodePCNode.
						   (RBVariableNode named: 'methodObj') })
]

{ #category : #api }
DRSuperInstructionCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [

	aGeneratorMethodBuilder addVariableNamed: 'nextBytecode'.
	aGeneratorMethodBuilder addStatement: self fetchNextBytecodeNode.
	nextBytecodes do: [ :next |
		aGeneratorMethodBuilder addStatement: (self caseNodeFor: next) ]
]

{ #category : #accessing }
DRSuperInstructionCustomisation >> interpreter: anInterpreter [

	interpreter := anInterpreter
]

{ #category : #'ast-nodes' }
DRSuperInstructionCustomisation >> nextBytecodePCNode [

	^ RBMessageNode
		  receiver: (RBVariableNode named: 'bytecodePC')
		  selector: #+
		  arguments: { self currentBytecodeSize asLiteralNode   }
]

{ #category : #factory }
DRSuperInstructionCustomisation >> nextBytecodes: bytecodes [

	nextBytecodes := bytecodes
]

{ #category : #api }
DRSuperInstructionCustomisation >> optimize: anIR [
	"Compile the super instructions. Maybe we want to do this in another moment."

	| compiler |
	nextBytecodes do: [ :next |
		compiler := DRBytecodeCompilerCompiler new
			            bytecodes: {
					            (currentBytecode -> (self bytecodeSelector: currentBytecode)).
					            (next -> (self bytecodeSelector: next)) };
			            interpreter: interpreter;
			            targetName: (self selectorForSuperInstruction: next);
			            configureForCompilerClass: compilerClass.

		compiler compile ]
]

{ #category : #'ast-nodes' }
DRSuperInstructionCustomisation >> selectorForSuperInstruction: nextBytecode [

	^ #_ join: {
			  #gen.
			  (self bytecodeSelector: currentBytecode).
			  (self bytecodeSelector: nextBytecode) }
]
