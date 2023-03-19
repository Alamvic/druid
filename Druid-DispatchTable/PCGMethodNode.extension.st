Extension { #name : #PCGMethodNode }

{ #category : #'*Druid-DispatchTable' }
PCGMethodNode >> maxCompiledPrimitives [

	^ self returnNode maxCompiledPrimitives
]

{ #category : #'*Druid-DispatchTable' }
PCGMethodNode >> returnNode [
	"Answer the receiver's <PCGReturnNode>"

	^ self statements detect: #isReturn
]
