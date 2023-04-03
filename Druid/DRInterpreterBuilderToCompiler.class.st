Class {
	#name : #DRInterpreterBuilderToCompiler,
	#superclass : #DRInterpreterToCompiler,
	#instVars : [
		'interpreter',
		'primitiveIndex'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - primitives' }
DRInterpreterBuilderToCompiler >> addPrimitives: aCollection [ 

	(self collectPrimitives: aCollection)
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]
]

{ #category : #adding }
DRInterpreterBuilderToCompiler >> collectPrimitives: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection
		  reject: [ :obj |
			  obj isNumber or: [
				  obj isNil or: [ (self isSelectedPrimitive: (self targetSuperclass lookupSelector: obj)) not ] ] ]
		  thenCollect: [ :symOrCompiledMethod |
			  symOrCompiledMethod isCompiledMethod
				  ifTrue: [ symOrCompiledMethod selector ]
				  ifFalse: [ symOrCompiledMethod ] ]
]

{ #category : #'accessing - compiler' }
DRInterpreterBuilderToCompiler >> compilationUnitClass [

	^ DRStackCompilationUnit
]

{ #category : #'accessing - compiler' }
DRInterpreterBuilderToCompiler >> compileAll [

	self newInterpreter.
	self compilationUnit 
			compiler: self compilerCompiler;
			compileAll
]

{ #category : #initialization }
DRInterpreterBuilderToCompiler >> initialize [

	super initialize.
	primitiveIndex := 0.
	compilationUnit := DRStackCompilationUnit new
		dispatchTableBuilder: DRStackInterpreterDispatchTableBuilder new;
		yourself
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreter: anObject [

	interpreter := anObject
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreterClass [

	^ self interpreter
		ifNil: [ super interpreterClass ]
		ifNotNil: [ : intrp | intrp class ]
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^  interpreter primitiveTable
]

{ #category : #'instance creation' }
DRInterpreterBuilderToCompiler >> newInterpreter [
	" Answer a new <AbstractInterpreter|StackInterpreter> instance "
	
	memory := self newMemory.
	self targetSuperclass
		ifNil: [ 	self targetSuperclass: CogVMSimulatorLSB ].
	self targetClass
		ifNil: [ self targetClass: self newRandomTargetInterpreterClass ].
	interpreter := self targetClass basicNew
		  objectMemory: memory;
		  basicInitialize.
	^ interpreter
]

{ #category : #adding }
DRInterpreterBuilderToCompiler >> newPrimitive: aString [
	" Answer a new primitive object with its metadata and CFG generated "

	^ DRInterpreterPrimitiveObject new
		primitiveNumber: self primitiveIndex;
		sourceMethod: (self targetSuperclass lookupSelector: aString);
		yourself
]

{ #category : #'instance creation' }
DRInterpreterBuilderToCompiler >> newRandomInterpreterClassName [
	" Answer a <String> representing a new class name to be used as interpreter containing a #basicPrimitiveTable"

	^ String streamContents: [ : stream |
		stream 
			<< self newclassPrefix;
			<< (UUIDGenerator next asString copyUpTo: $-) ]
]

{ #category : #'instance creation' }
DRInterpreterBuilderToCompiler >> newRandomTargetInterpreterClass [
	" Answer a new <Class> prefixed with receiver's class prefix and following a random name "

	^ Smalltalk image classInstaller make: [ : builder |
			builder
				name: self newRandomInterpreterClassName;
				superclass: self targetSuperclass;
				category: self newRandomInterpreterClassName ].
]

{ #category : #'instance creation' }
DRInterpreterBuilderToCompiler >> newclassPrefix [

	^ #DRInterpreter
]

{ #category : #initialization }
DRInterpreterBuilderToCompiler >> primitiveIndex [

	^ primitiveIndex :=  primitiveIndex + 1
]

{ #category : #'accessing - primitives' }
DRInterpreterBuilderToCompiler >> primitives [

	^ self compilationUnit primitives
]

{ #category : #'accessing - primitives' }
DRInterpreterBuilderToCompiler >> primitivesCount [
	"Answer a <Number> specifying how many primitive methods implements the receiver. Note that primitive table entry with 0 is ignored"

	| primTableSize |

	primTableSize := self primitiveTable size.
	(self primitiveTable includes: 0)
		ifTrue: [ ^ primTableSize - 1 ].
	^ primTableSize
]
