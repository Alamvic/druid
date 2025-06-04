Extension { #name : 'Collection' }

{ #category : '*Druid-Tests' }
Collection >> slowCount [

	| r |
	r := 0.
	self do: [ :each | r := r + 1 ].
	^ r 
]
