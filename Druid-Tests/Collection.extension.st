Extension { #name : 'Collection' }

{ #category : '*Druid-Tests' }
Collection >> badCount [

	| r |
	r := 0.
	self do: [ r := r + 1 ].
	^ r 
]
