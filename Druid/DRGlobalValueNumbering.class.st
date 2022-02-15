Class {
	#name : #DRGlobalValueNumbering,
	#superclass : #Object,
	#instVars : [
		'dominatorTree',
		'valueNumbers',
		'hashTables'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRGlobalValueNumbering >> applyTo: cfg [

	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
]
