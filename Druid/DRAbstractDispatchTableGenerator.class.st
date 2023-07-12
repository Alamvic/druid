"
Sibclasses accept a `DRCompilationUnit` as input (using `primitiveTableMethodFrom:`) and install methods in a JIT compiler class (using `installAllMethodsOn:`).

Methods starting with `build` answer Pharo Code Generator node instances, used to build the methods in the JIT compiler class.

"
Class {
	#name : #DRAbstractDispatchTableGenerator,
	#superclass : #Object,
	#instVars : [
		'primTableNode',
		'compilationUnit'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRAbstractDispatchTableGenerator class >> fromCompilationUnit: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		compilationUnit: aDRCompilationUnit;
		yourself
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> arrayAccessorGlobalName [

	self subclassResponsibility
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> buildInitializePrimitiveTableArrayMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	primTableNode := (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self buildPrimitiveTableArrayNode ].
	^ primTableNode
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> buildInitializePrimitiveTableAssignmentNode [

	self subclassResponsibility
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> buildInitializePrimitiveTableIVarAssignmentNode [

	^ PCGAssignmentNode new
		  variable: self buildPrimitiveTableVariableNode;
		  value: self buildPrimitiveTableInitialValueNode
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode [

	^ PCGAssignmentNode new
		  variable: self maxPrimitiveIndexGlobalName asPCGGlobal;
		  value: self maxCompiledPrimitives asPCG
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> buildInitializePrimitiveTableMethodNode [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: self initPrimitiveTableSelector) bodyBlock: [ : body |
		  body
			<< self buildInitializePrimitiveTableMaxCompiledPrimitiveIndexNode;
			<< self buildInitializePrimitiveTableIVarAssignmentNode;
			<< self buildInitializePrimitiveTableAssignmentNode;
			<< self buildPrimitiveTableVariableNode returnIt ]
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> buildPrimitiveTableArrayNode [
	"Answer a <PCGReturnNode> with the primitive table initialization setter message send"

	^ self primitiveTable asPCG returnIt
]

{ #category : #'accessing - primitives' }
DRAbstractDispatchTableGenerator >> buildPrimitiveTableVariableNode [

	^ #primitiveTable asPCGInstanceVariable
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> compilationUnit [

	^ compilationUnit
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> compilationUnit: anObject [

	compilationUnit := anObject
]

{ #category : #'accessing - building' }
DRAbstractDispatchTableGenerator >> initPrimitiveTableSelector [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> maxCompiledPrimitives [
	"Answer the <Number> of compiled primitives in the receiver"

	^ self primTableNode maxCompiledPrimitives
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> maxPrimitiveIndexGlobalName [

	self subclassResponsibility
]

{ #category : #accessing }
DRAbstractDispatchTableGenerator >> primTableNode [

	^ primTableNode
]

{ #category : #initialization }
DRAbstractDispatchTableGenerator >> primitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self compilationUnit primitiveTable
]
