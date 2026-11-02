Class {
	#name : 'DRCleanTemporaryInstructionsTest',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'tests' }
DRCleanTemporaryInstructionsTest >> testConditionalStoreAfterIsVectorWithCondition [

	"nested blocks"

	self assertCompilationNoInlineFor: #methodBlockTempConditionalWritingAfter fromClass: DrOpalExamples arguments: {}.
]

{ #category : 'tests' }
DRCleanTemporaryInstructionsTest >> testStoreAfterIsVector [

	self assertCompilationNoInlineFor: #methodBlockTempWritingAfter fromClass: DrOpalExamples arguments: {}.
]

{ #category : 'tests' }
DRCleanTemporaryInstructionsTest >> testStoreAfterIsVectorWithCondition [

	"nested blocks"

	self assertCompilationNoInlineFor: #methodBlockConditionTempWritingAfter fromClass: DrOpalExamples arguments: {}.
]

{ #category : 'tests' }
DRCleanTemporaryInstructionsTest >> testStoreBeforeIsCopied [

	self assertCompilationNoInlineFor: #methodBlockTempCopied fromClass: DrOpalExamples arguments: {}.
]

{ #category : 'tests' }
DRCleanTemporaryInstructionsTest >> testStoreInsideIsVector [

	self assertCompilationNoInlineFor: #methodBlockTempWritingInside fromClass: DrOpalExamples arguments: {}.
]
