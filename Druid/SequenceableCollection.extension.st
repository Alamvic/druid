Extension { #name : #SequenceableCollection }

{ #category : #'*Druid' }
SequenceableCollection >> allUntil: element [

	^ (self splitOn: element) first
]
