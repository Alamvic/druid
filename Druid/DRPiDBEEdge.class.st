Class {
	#name : 'DRPiDBEEdge',
	#superclass : 'DREdge',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiDBEEdge >> = anObject [

	^ self class = anObject class and: [
		  self source = anObject source and: [
			  self destination = anObject destination ] ]
]

{ #category : 'accessing' }
DRPiDBEEdge >> hash [

	^ source hash bitXor: destination hash
]
