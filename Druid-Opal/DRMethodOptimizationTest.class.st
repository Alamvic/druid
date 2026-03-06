Class {
	#name : 'DRMethodOptimizationTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'as yet unclassified' }
DRMethodOptimizationTest >> inputClass [

	^ DrOpalExamples 
]

{ #category : 'initialization' }
DRMethodOptimizationTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler irGenerator: DRMethodIRGenerator new.
	optimisation := DRInline new
]
