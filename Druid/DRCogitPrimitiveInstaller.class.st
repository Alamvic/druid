"
This class can take a `DRCompilationUnit` as input (using `primitiveTableMethodFrom:`) and install methods in a JIT compiler class (using `installAllMethodsOn:`).

Methods starting with `build` answer Pharo Code Generator node instances, used to build the methods in the JIT compiler class.

"
Class {
	#name : #DRCogitPrimitiveInstaller,
	#superclass : #Object,
	#instVars : [
		'primitiveSpec',
		'primTableNode'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'as yet unclassified' }
DRCogitPrimitiveInstaller class >> primitiveTableMethodFrom: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		yourself
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildInitializePrimitiveTableArrayMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	primTableNode := (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self buildPrimitiveTableArrayNode ].
	^ primTableNode
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildInitializePrimitiveTableAssignmentNode [
	"Answer a <PCGMessageNode> with the primitive table initialization setter message send"

	^ PCGMessageNode
			receiver: #self asPCGNode
			selector: #table:from:
			arguments: { #primitiveTable asPCGArgument . #'self primitiveTableArray' asPCGArgument }
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildInitializePrimitiveTableIVarAssignmentNode [

	^ PCGAssignmentNode new
		  variable: #primitiveTable asPCGInstanceVariable;
		  value: self buildPrimitiveTableInitialValueNode
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode [

	^ PCGAssignmentNode new
		  variable: #MaxCompiledPrimitiveIndex asPCGGlobal;
		  value: self maxCompiledPrimitives asPCG
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildInitializePrimitiveTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: #initializePrimitiveTable) bodyBlock: [ : body |
		  body
			<< self buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode
			<< self buildInitializePrimitiveTableIVarAssignmentNode;
			<< self buildInitializePrimitiveTableAssignmentNode ]
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildPrimitiveTableArrayNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self initPrimitiveTable asPCG returnIt
]

{ #category : #'accessing - building' }
DRCogitPrimitiveInstaller >> buildPrimitiveTableInitialValueNode [

	^ PCGMessageNode
		receiver: #CArrayAccessor asPCGGlobal
		selector: #on:
		arguments:
			{ PCGMessageNode
				receiver: #Array asPCGGlobal
				selector: #new:
				arguments: { PCGMessageNode
					receiver: #MaxCompiledPrimitiveIndex asPCGGlobal
					selector: #+
					arguments: { 1 asPCG } .} }
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> collectAllMethodsOn: aJITCompilerClass [
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
DRCogitPrimitiveInstaller >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #'accessing - installation' }
DRCogitPrimitiveInstaller >> installAllMethodsOn: aJITCompilerClass [
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
DRCogitPrimitiveInstaller >> maxCompiledPrimitives [
	"Answer the <Number> of compiled primitives in the receiver"

	^ (self primTableNode statements detect: #isReturn) value statements size
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> primTableNode [

	^ primTableNode
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> primitiveSpec: anObject [

	primitiveSpec := anObject
]
