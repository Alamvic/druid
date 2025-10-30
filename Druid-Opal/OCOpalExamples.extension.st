Extension { #name : 'OCOpalExamples' }

{ #category : '*Druid-Opal' }
OCOpalExamples >> foo [
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> index [

	^ 1
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> methodArgument: arg [

	^ [ arg ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> noRemoteBlockReturn [
	(collection at: self index ifAbsent: [ ^ self ])
		open
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlockReadInBlock [
	| t1 |
	t1 := 1.
	[ t1 < 5 ] whileTrue: [ [ t1 := 10 ] value ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlockWriteInBlock [
	| t1 |
	t1 := 1.
	[ t1 < 5 ] whileTrue: [ t1 := 10 ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlockWriteInNestedBlock [
	| t1 |
	t1 := 1.
	^[ [ t1 < 5 ] whileTrue: [ t1 := 10 ] ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlockWriteInNestedBlockCase2 [

	| t1 |
	t1 := 1.
	(true) ifTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 10 ] ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlockWriteReadInBlock [
	| t1 |
	t1 := 1.
	[ t1 < 5 ] whileTrue: [ t1 := 10. [ t1 ] ]
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> optimizedBlocksAndSameNameTemps [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 1. a]
			ifFalse: [| a | a := 2. a].
	c := String new: s.
	1 to: s do:
		[:i| c at: i put: (c at: i)].
	^c
]

{ #category : '*Druid-Opal' }
OCOpalExamples >> singleRemoteTempVar [
	| index block |
	index := 0.
	block := [
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	^ collection
]
