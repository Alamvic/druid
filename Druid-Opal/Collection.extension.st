Extension { #name : 'Collection' }

{ #category : '*Druid-Opal' }
Collection >> slowCount [

	| r |
	r := 0.
	self do: [ :each | r := r + 1 ].
	^ r 
]
