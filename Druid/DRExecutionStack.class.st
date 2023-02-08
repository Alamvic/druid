Class {
	#name : #DRExecutionStack,
	#superclass : #Object,
	#instVars : [
		'stateStack',
		'mirrorStack'
	],
	#category : #'Druid-Tests'
}

{ #category : #frames }
DRExecutionStack >> pushFrame: aDRStackStack [

	mirrorStack push: aDRStackStack mirrorFrame.
	stateStack push: aDRStackStack
]
