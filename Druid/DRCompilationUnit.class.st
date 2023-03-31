"
Abstract class to provide compilation behavior. Subclasses represents a Unit of compilation (a class), which can be asked for:

- Set a target class and its superclass (`DRCompilationUnit>>#targetClass:`, `DRCompilationUnit>>#targetSuperclass:`)..
- Adding primitives (`DRCompilationUnit>>#addPrimitiveEntries`, `DRCompilationUnit>>#addPrimitiveEntry:`).
- Compile added primitives (`DRCompilationUnit>>#compilePrimitive:`, `DRCompilationUnit>>#compileAll`).
- Configure the primitive table (`DRCompilationUnit>>#initPrimitiveTable`).

"
Class {
	#name : #DRCompilationUnit,
	#superclass : #Object,
	#instVars : [
		'targetClass',
		'targetSuperclass',
		'compiler',
		'compilerBuilder'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRCompilationUnit class >> fromCompilerBuilder: aDRCompilerBuilder [

	^  self new
			compilerBuilder: aDRCompilerBuilder;
			yourself
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compiler [

	^ compiler

]

{ #category : #accessing }
DRCompilationUnit >> compiler: aDRCompiler [ 

	compiler := aDRCompiler
]

{ #category : #accessing }
DRCompilationUnit >> compilerBuilder [

	^ compilerBuilder
]

{ #category : #accessing }
DRCompilationUnit >> compilerBuilder: anObject [

	compilerBuilder := anObject
]

{ #category : #accessing }
DRCompilationUnit >> targetClass [
	" Answer a <Class> which is the where the methods will be stored "

	^ targetClass
]

{ #category : #accessing }
DRCompilationUnit >> targetClass: aClass [

	targetClass := aClass
]

{ #category : #accessing }
DRCompilationUnit >> targetSuperclass [
	" Answer a <Class> acting as the receiver's superclass for compilation "

	^ targetSuperclass
]

{ #category : #accessing }
DRCompilationUnit >> targetSuperclass: aClass [

	targetSuperclass := aClass
]
