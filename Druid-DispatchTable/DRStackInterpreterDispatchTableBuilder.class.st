Class {
	#name : #DRStackInterpreterDispatchTableBuilder,
	#superclass : #DRAbstractDispatchTableBuilder,
	#category : #'Druid-DispatchTable-Model'
}

{ #category : #'as yet unclassified' }
DRStackInterpreterDispatchTableBuilder class >> primitiveTableMethodFrom: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		yourself
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableArrayMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	primTableNode := (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self buildPrimitiveTableArrayNode ].
	^ primTableNode
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { #primitiveTable asPCGArgument . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableIVarAssignmentNode [

	^ PCGAssignmentNode new
		  variable: #primitiveTable asPCGInstanceVariable;
		  value: self buildPrimitiveTableInitialValueNode
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode [

	^ PCGAssignmentNode new
		  variable: #MaxCompiledPrimitiveIndex asPCGGlobal;
		  value: self maxCompiledPrimitives asPCG
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildInitializePrimitiveTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: #initializePrimitiveTable) bodyBlock: [ : body |
		  body
			<< self buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode
			<< self buildInitializePrimitiveTableIVarAssignmentNode;
			<< self buildInitializePrimitiveTableAssignmentNode ]
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildPrimitiveTableArrayNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self initPrimitiveTable asPCG returnIt
]

{ #category : #'accessing - building' }
DRStackInterpreterDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
		receiver: #CArrayAccessor asPCGGlobal
		selector: #on:
		arguments:
			(PCGMessageNode
				receiver: #Array asPCGGlobal
				selector: #new:
				arguments: (PCGMessageNode
					receiver: #MaxCompiledPrimitiveIndex asPCGGlobal
					selector: #+
					arguments: { 1 asPCG }) )
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> collectAllMethodsOn: aJITCompilerClass [
	"Answer a <Collection> of <PCGMethodNode> to be installated in a JIT compiler class"

	^ {
		self buildInitializePrimitiveTableArrayMethodNode .
		self buildInitializePrimitiveTableMethodNode
		} collect: [ : pcgNode |
			pcgNode
				protocol: #'class initialization';
				installOn: aJITCompilerClass.
			pcgNode ]
]

{ #category : #initialization }
DRStackInterpreterDispatchTableBuilder >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #'accessing - installation' }
DRStackInterpreterDispatchTableBuilder >> installAllMethodsOn: aJITCompilerClass [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ {
		self buildInitializePrimitiveTableArrayMethodNode .
		self buildInitializePrimitiveTableMethodNode
		} do: [ : pcgNode |
			pcgNode
				protocol: #'class initialization';
				installOn: aJITCompilerClass ]
		displayingProgress: 'Installing methods'
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> maxCompiledPrimitives [
	"Answer the <Number> of compiled primitives in the receiver"
	
	^ self primTableNode maxCompiledPrimitives
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> primTableNode [

	^ primTableNode
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRStackInterpreterDispatchTableBuilder >> primitiveSpec: anObject [

	primitiveSpec := anObject
]
