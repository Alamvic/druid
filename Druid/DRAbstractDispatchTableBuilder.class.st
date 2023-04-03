"
Sibclasses accept a `DRCompilationUnit` as input (using `primitiveTableMethodFrom:`) and install methods in a JIT compiler class (using `installAllMethodsOn:`).

Methods starting with `build` answer Pharo Code Generator node instances, used to build the methods in the JIT compiler class.

"
Class {
	#name : #DRAbstractDispatchTableBuilder,
	#superclass : #Object,
	#instVars : [
		'primTableNode',
		'primitiveSpec'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'as yet unclassified' }
DRAbstractDispatchTableBuilder class >> primitiveTableMethodFrom: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		yourself
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableBuilder >> arrayAccessorGlobalName [

	self subclassResponsibility
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableBuilder >> buildInitializePrimitiveTableArrayMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	primTableNode := (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self buildPrimitiveTableArrayNode ].
	^ primTableNode
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableBuilder >> buildInitializePrimitiveTableAssignmentNode [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode [

	^ PCGAssignmentNode new
		  variable: self maxPrimitiveIndexGlobalName asPCGGlobal;
		  value: self maxCompiledPrimitives asPCG
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableBuilder >> buildPrimitiveTableArrayNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self initPrimitiveTable asPCG returnIt
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> buildPrimitiveTableInitialValueNode [

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
DRAbstractDispatchTableBuilder >> collectAllMethodsOn: aJITCompilerClass [
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
DRAbstractDispatchTableBuilder >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableBuilder >> initPrimitiveTableSelector [

	self subclassResponsibility
]

{ #category : #'accessing - installation' }
DRAbstractDispatchTableBuilder >> installAllMethodsOn: aJITCompilerClass [
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
DRAbstractDispatchTableBuilder >> maxCompiledPrimitives [
	"Answer the <Number> of compiled primitives in the receiver"

	^ self primTableNode maxCompiledPrimitives
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> maxPrimitiveIndexGlobalName [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> primTableNode [

	^ primTableNode
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRAbstractDispatchTableBuilder >> primitiveSpec: anObject [

	primitiveSpec := anObject
]
