Class {
	#name : 'DRSingleInline',
	#superclass : 'DRInline',
	#instVars : [
		'messageSend'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'instance creation' }
DRSingleInline class >> forMessageSend: aDRMessageSend [ 
	
	^ self new
		messageSend: aDRMessageSend;
		yourself
]

{ #category : 'accessing' }
DRSingleInline >> doApply: cfg [

	self inline: messageSend
]

{ #category : 'accessing' }
DRSingleInline >> messageSend: aDRMessageSend [ 
	messageSend := aDRMessageSend
]
