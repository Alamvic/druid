Class {
	#name : 'DRMethodProfiler',
	#superclass : 'Object',
	#instVars : [
		'method',
		'typesDictionary'
	],
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'factory' }
DRMethodProfiler class >> forMethod: aCompiledMethod [

	^ self new
		  method: aCompiledMethod;
		  yourself
]

{ #category : 'adding' }
DRMethodProfiler >> addType: aDRType forNode: aRBNode [

	typesDictionary
		at: aRBNode
		ifPresent: [ :types | types add: aDRType ]
		ifAbsentPut: [ { aDRType } asSet ]
]

{ #category : 'initialization' }
DRMethodProfiler >> initialize [

	typesDictionary := Dictionary new
]

{ #category : 'accessing' }
DRMethodProfiler >> method: aCompiledMethod [

	method := aCompiledMethod
]

{ #category : 'accessing' }
DRMethodProfiler >> typesFor: aRBNode [

	^ typesDictionary at: aRBNode ifAbsent: [ {  } ]
]
