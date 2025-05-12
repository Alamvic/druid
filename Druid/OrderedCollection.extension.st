Extension { #name : 'OrderedCollection' }

{ #category : '*Druid' }
OrderedCollection >> whileNotEmpty: aBlock [

	"aCol whileNotEmpty: [:current :col :marked | ...iteration... ]"

	| marked |
	marked := Set new.

	[ self isEmpty ] whileFalse: [ 
		aBlock cull: self removeFirst cull: self cull: marked ].

	^ marked
]
