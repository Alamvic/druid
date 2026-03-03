Extension { #name : 'OCCommentToken' }

{ #category : '*Druid' }
OCCommentToken class >> value: aString in: aStartPosition [

	^ self
		  value: aString
		  start: aStartPosition
		  stop: aStartPosition + aString size
]
