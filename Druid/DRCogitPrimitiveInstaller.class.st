Class {
	#name : #DRCogitPrimitiveInstaller,
	#superclass : #Object,
	#instVars : [
		'primitiveSpec'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRCogitPrimitiveInstaller class >> primitiveTableMethodFrom: aDRCompilationUnit [
	"Answer a <DRCogitPrimitiveInstaller> method representation of aDRCompilationUnit"

	^ self new
		primitiveSpec: aDRCompilationUnit;
		yourself
]

{ #category : #initialization }
DRCogitPrimitiveInstaller >> initPrimitiveTable [
	"Answer a <Collection> with primitive table entries"

	^ self primitiveSpec initPrimitiveTable
]

{ #category : #generation }
DRCogitPrimitiveInstaller >> initializePrimitiveTableArrayMethod [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: #primitiveTableArray)
			bodyBlock: [ :body |
				body << self primitiveTableArray ]
]

{ #category : #generation }
DRCogitPrimitiveInstaller >> initializePrimitiveTableMethod [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ (PCGMethodNode selector: #initializePrimitiveTable)
			bodyBlock: [ :body |
				body
					<< 	(PCGAssignmentNode new
							variable: #primitiveTable asPCGInstanceVariable;
							value: self primitiveTableInitialValue);
					<< (PCGMessageNode
								receiver: #self asPCGNode
								selector: #table:from:
								arguments: { #primitiveTable asPCGArgument . #'self primitiveTableArray' asPCGArgument } ) ]
]

{ #category : #generation }
DRCogitPrimitiveInstaller >> installAllMethodsOn: aJITCompilerClass [
	"Answer a <PCGMethodNode> ready for installation in a JIT compiler class"

	^ {
		self initializePrimitiveTableArrayMethod .
		self initializePrimitiveTableMethod
		} do: [ : pcgMethod |
			pcgMethod
				protocol: #'class initialization';
				installOn: aJITCompilerClass ]
		displayingProgress: 'Installing methods'
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> primitiveSpec [

	^ primitiveSpec
]

{ #category : #accessing }
DRCogitPrimitiveInstaller >> primitiveSpec: anObject [

	primitiveSpec := anObject
]

{ #category : #generation }
DRCogitPrimitiveInstaller >> primitiveTableArray [

	^ self initPrimitiveTable asPCG
]

{ #category : #generation }
DRCogitPrimitiveInstaller >> primitiveTableInitialValue [

	^ PCGMessageNode
		receiver: #CArrayAccessor asPCGGlobal
		selector: #on:
		arguments: {
			(PCGMessageNode
				receiver: #MaxCompiledPrimitiveIndex asPCGGlobal
				selector: #+
				arguments: { 1 asPCG }) } asPCG
]
