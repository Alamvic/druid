Class {
	#name : #DRPrimitiveCCCollector,
	#superclass : #DRPrimitiveCompilerCompiler,
	#instVars : [
		'failedPrimitives'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'instance creation' }
DRPrimitiveCCCollector class >> forInterpreter: aDRInterpreterCompiler [ 

	^ self new
		interpreter: aDRInterpreterCompiler;
		yourself
]

{ #category : #accessing }
DRPrimitiveCCCollector >> failedPrimitives [

	^ failedPrimitives
		ifNil: [ failedPrimitives := Dictionary new ]
]

{ #category : #accessing }
DRPrimitiveCCCollector >> failedPrimitivesAt: anException add: aString [

	self halt.
	self failedPrimitives
		at: anException
		ifPresent: [ : fps | fps add: aString ]
		ifAbsent: [ self failedPrimitives at: anException put: (OrderedCollection with: aString)  ]
]

{ #category : #'generation-IR' }
DRPrimitiveCCCollector >> generateDruidIR [

	^ [ super generateDruidIR ]
	on: Exception
	do: [ : ex | ex handlePrimitiveCompilationFor: self ]
]
