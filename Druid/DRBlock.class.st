Class {
	#name : #DRBlock,
	#superclass : #Object,
	#instVars : [
		'block',
		'home'
	],
	#category : #'Druid-Interpreter'
}

{ #category : #accessing }
DRBlock >> block [
	^ block
]

{ #category : #accessing }
DRBlock >> block: anObject [
	block := anObject
]

{ #category : #accessing }
DRBlock >> home [
	^ home
]

{ #category : #accessing }
DRBlock >> home: anObject [
	home := anObject
]
