"
This is a helper class to collect primitive method which couldn't be compiled by Druid.

"
Class {
	#name : #DRPrimitiveCompilerCompilerCollector,
	#superclass : #DRPrimitiveCompilerCompiler,
	#instVars : [
		'failedPrimitives'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveCompilerCompilerCollector >> failedPrimitives [

	^ failedPrimitives
		ifNil: [
			failedPrimitives :=
				PluggableDictionary new
					equalBlock: [ :a :b | a class == b class ];
					hashBlock: [ :a | a class identityHash ];
					yourself 	]
]

{ #category : #accessing }
DRPrimitiveCompilerCompilerCollector >> failedPrimitivesAt: anException add: aString [

	self failedPrimitives
		at: anException
		ifPresent: [ : fps | fps add: aString ]
		ifAbsent: [ self failedPrimitives at: anException put: (OrderedCollection with: aString)  ]
]

{ #category : #'generation-IR' }
DRPrimitiveCompilerCompilerCollector >> generateDruidIR [
	"Try to generate the Druid IR for the receiver's source method, if fails, record the failure"

	^ [ super generateDruidIR ]
	on: Exception
	do: [ : ex | ex handlePrimitiveCompilationFor: self ]
]

{ #category : #testing }
DRPrimitiveCompilerCompilerCollector >> hasFailedPrimitives [
	"Answer <true> if the receiver recorded failure during compilation"

	^ self failedPrimitives isNotEmpty
]
