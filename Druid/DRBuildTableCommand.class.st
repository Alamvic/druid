"
When executed, this command builds an Interpreter subclass and a JIT Compiler Class, for the selected entries (primitives), using the values (class names and superclasses) supplied by the user in the UI.

"
Class {
	#name : #DRBuildTableCommand,
	#superclass : #DRDispatchTableMenuCommand,
	#category : #'Druid-Commands'
}

{ #category : #default }
DRBuildTableCommand class >> defaultDescription [

	^ 'Build Dispatch Table'
]

{ #category : #'accessing - defaults' }
DRBuildTableCommand class >> defaultIconName [

	^ #add
]

{ #category : #default }
DRBuildTableCommand class >> defaultName [

	^ 'Build Dispatch Table'
]

{ #category : #testing }
DRBuildTableCommand >> canBeExecuted [
	" Receiver can be executed if there are selections "

	^ self application selectedEntries notEmpty and: [ self requiredEntriesNotEmpty ]
]

{ #category : #executing }
DRBuildTableCommand >> createInterpreterBuilder [

	| superClass interpreterBuilder compilationUnit |

	superClass := self interpreterSuperclassName
		ifEmpty: [ StackInterpreter ]
		ifNotEmpty: [ : superClassName | (self class environment at: superClassName) ].
	interpreterBuilder := self interpreterClassName
		  ifEmpty: [  DRInterpreterBuilder forRandomInterpreterWithSuperclass: superClass ]
		  ifNotEmpty: [ :interpreterName |
			  DRInterpreterBuilder
				  forInterpreter: interpreterName
				  superclass: superClass ].

	interpreterBuilder primitives: self selectedEntries asArray.
	compilationUnit := interpreterBuilder generateBuildModel.
	compilationUnit compileInitializePrimitiveTable.
	^ interpreterBuilder
]

{ #category : #executing }
DRBuildTableCommand >> execute [
	"Private - Generate the interpreter and JIT classes using the selected entries"

	| interpreterBuilder |

	interpreterBuilder := self createInterpreterBuilder.

	(DRInterpreterCompiler fromInterpreter: interpreterBuilder)
		generateBuildModelAndCompileIn: self jitClassName
		superclass: self jitSuperclassName.

	Smalltalk tools browser openOnClass: (self class environment at: self jitClassName asSymbol).
	Smalltalk tools browser openOnClass: (self class environment at: self interpreterClassName asSymbol)
]

{ #category : #accessing }
DRBuildTableCommand >> newRandomInterpreterClassName [
	"Answer a new class name to be used as interpreter containing a #basicPrimitiveTable"

	^ String streamContents: [ : stream |
		stream
			<< #DRInterpreter;
			<< (UUIDGenerator next asString copyUpTo: $-) ]
]

{ #category : #testing }
DRBuildTableCommand >> requiredEntriesNotEmpty [
	"Answer <true> if the required class names presenters has values"

	^ { self interpreterClassName .
		self interpreterSuperclassName .
		self jitClassName .
		self jitSuperclassName } allSatisfy: #notEmpty
]
