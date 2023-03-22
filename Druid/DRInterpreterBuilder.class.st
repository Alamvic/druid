Class {
	#name : #DRInterpreterBuilder,
	#superclass : #DRAbstractCompilerBuilder,
	#traits : 'TDRMethodSelector',
	#classTraits : 'TDRMethodSelector classTrait',
	#instVars : [
		'primitiveTable',
		'primitiveIndex'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'instance creation' }
DRInterpreterBuilder class >> newInterpreterNamed: aClassName superclass: aSuperclass [

	^ self new
		targetSuperclass: aSuperclass;
		newInterpreterNamed: aClassName;
		yourself.
]

{ #category : #'instance creation' }
DRInterpreterBuilder class >> newRandomlyNamedInterpreterWithSuperclass: aSuperclass [

	^ self new
		targetSuperclass: aSuperclass;
		newInterpreter;
		yourself
]

{ #category : #'instance creation' }
DRInterpreterBuilder >> basicNew [

	self targetClass: self newRandomTargetClass.
	self targetSuperclass: self defaultTargetSuperclass.
	^ self targetClass new
]

{ #category : #'accessing - compiler' }
DRInterpreterBuilder >> compilationUnitClass [

	^ DRStackCompilationUnit
]

{ #category : #accessing }
DRInterpreterBuilder >> defaultTargetSuperclass [

	^ StackInterpreter
]

{ #category : #'accessing - model' }
DRInterpreterBuilder >> generateBuildModelAndCompileIn: aClass [
	"Generate and install the receiver's primitives into JIT compiler aClass"

	self flag: #toreview.
	self generateBuildModel
		jitCompilerClass: (self environmentAt: aClass);
		compileAll
]

{ #category : #initialization }
DRInterpreterBuilder >> initialize [

	super initialize.
	primitiveIndex := 0.
	compilationUnit := DRStackCompilationUnit new
		dispatchTableBuilder: DRStackInterpreterDispatchTableBuilder new;
		yourself
]

{ #category : #initialization }
DRInterpreterBuilder >> initializeWithOptions: aCollection objectMemoryClass: aClass [
]

{ #category : #accessing }
DRInterpreterBuilder >> interpreterPrimitiveTable [

	^ self primitiveTable
]

{ #category : #'instance creation' }
DRInterpreterBuilder >> newInterpreter [
	"Answer a new instance of the receiver's target class, representing a basic interpreter"

	self targetSuperclass
		ifNil: [ 	self targetSuperclass: CogVMSimulatorLSB ].
	self targetClass
		ifNil: [ self targetClass: self newRandomTargetClass ].
	^ self targetClass new
]

{ #category : #'instance creation' }
DRInterpreterBuilder >> newInterpreterNamed: aClassName [
	"Answer a new instance of the receiver's target class, representing a basic interpreter"

	self targetClass: (self environmentAt: aClassName).
	^ self targetClass new
]

{ #category : #accessing }
DRInterpreterBuilder >> newPrimitive: aString [
	" Answer a new primitive object with its metadata and CFG generated "

	^ DRPrimitiveObject new
		primitiveNumber: self primitiveIndex;
		sourceSelector: aString;
		yourself
]

{ #category : #accessing }
DRInterpreterBuilder >> newclassPrefix [

	^ #DRInterpreter
]

{ #category : #accessing }
DRInterpreterBuilder >> primitiveIndex [

	^ primitiveIndex :=  primitiveIndex + 1
]

{ #category : #accessing }
DRInterpreterBuilder >> primitiveTable [
	" Answer a <Collection> of tuples representing primitive methods metadata "

	^ primitiveTable
		ifNil: [ primitiveTable := self targetClass basicPrimitiveTable ]
]

{ #category : #private }
DRInterpreterBuilder >> primitives [
	" Answer the <Collection> of receiver's primitives "

	^ primitiveTable
]

{ #category : #private }
DRInterpreterBuilder >> primitives: aCollection [

	primitiveTable := aCollection
]

{ #category : #accessing }
DRInterpreterBuilder >> primitivesCount [
	"Answer a <Number> specifying how many primitive methods implements the receiver. Note that primitive table entry with 0 is ignored"

	| primTableSize |

	primTableSize := self primitiveTable size.
	(self primitiveTable includes: 0)
		ifTrue: [ ^ primTableSize - 1 ].
	^ primTableSize
]

{ #category : #accessing }
DRInterpreterBuilder >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "
	| primitiveCandidates |

	primitiveCandidates := self
		matchMethod: #beginsWith:;
		scanSuperclasses: true;
		allowDuplicates: false;
		methodsFromPattern: 'prim'.
	^ primitiveCandidates select: selectBlock
]

{ #category : #accessing }
DRInterpreterBuilder >> targetClass [

	^ self compilationUnit targetClass
]

{ #category : #accessing }
DRInterpreterBuilder >> targetClass: anObject [

	self compilationUnit targetClass: anObject
]
