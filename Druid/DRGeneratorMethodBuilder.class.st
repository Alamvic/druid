Class {
	#name : #DRGeneratorMethodBuilder,
	#superclass : #Object,
	#instVars : [
		'selector',
		'statements',
		'variableNames'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRGeneratorMethodBuilder >> addStatement: aRBMessageNode [ 
	
	statements add: aRBMessageNode
]

{ #category : #accessing }
DRGeneratorMethodBuilder >> addVariableNamed: aVariableName [ 
	
	variableNames add: aVariableName
]

{ #category : #accessing }
DRGeneratorMethodBuilder >> initialize [

	super initialize.	
	statements := OrderedCollection new.
	variableNames := OrderedCollection new.
]

{ #category : #accessing }
DRGeneratorMethodBuilder >> methodNode [
	
	^ RBMethodNode
		selector: selector
		body: (RBSequenceNode
			temporaries: (variableNames collect: [ :name | RBVariableNode named: name ])
			statements: statements)
]

{ #category : #accessing }
DRGeneratorMethodBuilder >> selector: aSelector [
	
	selector := aSelector
]

{ #category : #accessing }
DRGeneratorMethodBuilder >> statements [
	^ statements
]
