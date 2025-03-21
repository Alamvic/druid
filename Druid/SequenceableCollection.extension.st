Extension { #name : 'SequenceableCollection' }

{ #category : '*Druid' }
SequenceableCollection >> allUntil: element [

	^ (self splitOn: element) first
]

{ #category : '*Druid' }
SequenceableCollection >> matchArgumentsAgainst: aPragma [

	aPragma arguments with: self do: [ : pragmaArgument : queryArgument |
		queryArgument = pragmaArgument
			ifFalse: [ ^ false ] ].
	^ true
]
