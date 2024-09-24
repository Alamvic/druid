Class {
	#name : #DRMetaCompilationTest,
	#superclass : #DRDruidTest,
	#instVars : [
		'cogitStyle',
		'compilerClass'
	],
	#category : #'Druid-Tests'
}

{ #category : #'building suites' }
DRMetaCompilationTest class >> cogitStyleParameter [
	"Two ways to compile code with different calling conventions: simple stack vs stack to register mapping"

	^ ParametrizedTestMatrix new
		addCase: { #cogitStyle -> #beForStackToRegisterMapping. #compilerClass -> DruidTestRTLCompiler};
		addCase: { #cogitStyle -> #beForSimpleStack. #compilerClass -> SimpleDruidTestRTLCompiler};
		yourself
]

{ #category : #'building suites' }
DRMetaCompilationTest class >> testParameters [ 

	^ super testParameters * self cogitStyleParameter
]

{ #category : #accessing }
DRMetaCompilationTest >> cogitStyle: aSymbol [

	cogitStyle := aSymbol
]

{ #category : #accessing }
DRMetaCompilationTest >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRMetaCompilationTest >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #'helpers-compiling' }
DRMetaCompilationTest >> jitCompilerClass [

	^ compilerClass
]
