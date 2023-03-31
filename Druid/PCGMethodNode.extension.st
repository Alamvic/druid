Extension { #name : #PCGMethodNode }

{ #category : #'*Druid' }
PCGMethodNode >> maxCompiledPrimitives [

	^ self returnNode maxCompiledPrimitives
]

{ #category : #'*Druid' }
PCGMethodNode >> returnNode [
	"Answer the receiver's <PCGReturnNode>"

	^ self statements detect: #isReturn
]
