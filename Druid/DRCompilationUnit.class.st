"
Abstract class to provide compilation behavior. Subclasses represents a Unit of compilation (a class), which can be asked for:

- Set a target class and its superclass (`DRCompilationUnit>>#targetClass:`, `DRCompilationUnit>>#targetSuperclass:`)..
- A specific compilation builder to help in the compilation process.

"
Class {
	#name : 'DRCompilationUnit',
	#superclass : 'Object',
	#instVars : [
		'targetClass',
		'targetSuperclass'
	],
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'accessing' }
DRCompilationUnit >> defaultTargetSuperclass [

	^ StackToRegisterMappingCogit
]

{ #category : 'accessing' }
DRCompilationUnit >> targetClass [
	" Answer a <Class> which is the where the methods will be stored "

	^ targetClass
]

{ #category : 'accessing' }
DRCompilationUnit >> targetClass: aClass [

	targetClass := aClass
]

{ #category : 'accessing' }
DRCompilationUnit >> targetSuperclass [
	" Answer a <Class> acting as the receiver's superclass for compilation "

	^ targetSuperclass
		ifNil: [ targetSuperclass := self defaultTargetSuperclass ]
]

{ #category : 'accessing' }
DRCompilationUnit >> targetSuperclass: aClass [

	targetSuperclass := aClass
]
