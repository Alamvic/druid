"
This class can take a `DRCompilationUnit` as input (using `primitiveTableMethodFrom:`) and install methods in a JIT compiler class (using `installAllMethodsOn:`).

Methods starting with `build` answer Pharo Code Generator node instances, used to build the methods in the JIT compiler class.

"
Class {
	#name : #DRCogitDispatchTableBuilder,
	#superclass : #DRAbstractDispatchTableBuilder,
	#category : #'Druid-DispatchTable-Model'
}

{ #category : #'as yet unclassified' }
DRCogitDispatchTableBuilder class >> primitiveTableMethodFrom: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		yourself
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableArrayMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	primTableNode := (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self buildPrimitiveTableArrayNode ].
	^ primTableNode
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { #primitiveTable asPCGArgument . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableIVarAssignmentNode [

	^ PCGAssignmentNode new
		  variable: #primitiveTable asPCGInstanceVariable;
		  value: self buildPrimitiveTableInitialValueNode
]

{ #category : #accessing }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode [

	^ PCGAssignmentNode new
		  variable: #MaxCompiledPrimitiveIndex asPCGGlobal;
		  value: self maxCompiledPrimitives asPCG
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildInitializePrimitiveTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: #initializePrimitiveTable) bodyBlock: [ : body |
		  body
			<< self buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode
			<< self buildInitializePrimitiveTableIVarAssignmentNode;
			<< self buildInitializePrimitiveTableAssignmentNode ]
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildPrimitiveTableArrayNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self initPrimitiveTable asPCG returnIt
]

{ #category : #'accessing - building' }
DRCogitDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

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
DRCogitDispatchTableBuilder >> collectAllMethodsOn: aJITCompilerClass [
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
DRCogitDispatchTableBuilder >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #'accessing - installation' }
DRCogitDispatchTableBuilder >> installAllMethodsOn: aJITCompilerClass [
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
DRCogitDispatchTableBuilder >> maxCompiledPrimitives [
	"Answer the <Number> of compiled primitives in the receiver"

	^ self primTableNode maxCompiledPrimitives
]

{ #category : #accessing }
DRCogitDispatchTableBuilder >> primTableNode [

	^ primTableNode
]

{ #category : #accessing }
DRCogitDispatchTableBuilder >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRCogitDispatchTableBuilder >> primitiveSpec: anObject [

	primitiveSpec := anObject
]
