Extension { #name : #RBCommentToken }

{ #category : #'*Druid' }
RBCommentToken class >> value: aString in: aStartPosition [

	^ self
		  value: aString
		  start: aStartPosition
		  stop: aStartPosition + aString size
]
