Extension { #name : 'Integer' }

{ #category : '*Druid' }
Integer >> maskOfBytes [

	| res |
	res := 0.
	self timesRepeat: [ res := res << 8 + 16rFF ].
	^ res
]
