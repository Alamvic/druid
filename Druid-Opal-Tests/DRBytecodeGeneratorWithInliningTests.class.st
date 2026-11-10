Class {
	#name : 'DRBytecodeGeneratorWithInliningTests',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'tests' }
DRBytecodeGeneratorWithInliningTests >> test001_InlineAndReturnToDoResult [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnToDo fromClass: DROpalExamples.
	
	"The message was inlined"
	self deny: (newMethod messages includes: #to:do:). 
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.

	self assert: result equals: 1
]

{ #category : 'tests' }
DRBytecodeGeneratorWithInliningTests >> test002_InlineMethodWithExplicitReturn [

	| newMethod receiver result |
	newMethod := self compileSelector: #messageSendWithExplicitReturn fromClass: DROpalExamples.
	
	"The message was inlined"
	self assert: newMethod messages isEmpty. 
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 84
]

{ #category : 'tests' }
DRBytecodeGeneratorWithInliningTests >> test003_InlineMethodWithImplicitReturn [

	| newMethod receiver result |
	newMethod := self compileSelector: #messageSendWithImplicitReturn fromClass: DROpalExamples.
	
	"The message was inlined"
	self deny: (newMethod messages includes: #timesRepeat:). 
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.

	self assert: result equals: 2
]
