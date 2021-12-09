Class {
	#name : #BaselineOfDruid,
	#superclass : #BaselineOf,
	#category : #BaselineOfDruid
}

{ #category : #baseline }
BaselineOfDruid >> baseline: spec [
  <baseline>
  spec
    for: #(common)
    do: [ 
		spec
			baseline: 'PharoVM' 
			with: [ spec
				className: 'BaselineOfVMMaker';
				repository: 'github://pharo-project/opensmalltalk-vm:druid' ].
      spec package: 'Druid' with: [ spec requires: 'PharoVM' ] ]

]

{ #category : #doits }
BaselineOfDruid >> changeRepositoryType [
	| repos newRepo |
	(Smalltalk hasClassNamed: #MCGitRepository) ifFalse: [ ^ self ].
	
	repos := MCRepositoryGroup default repositories
		select: [ :r | (r description includesSubstring: 'LibGit:master') or: [ 
			r description includesSubstring: 'LibGit/master' ] ].
	repos ifEmpty: [  ^ self ].
	
	'github-cache/theseion' asFileReference deleteAll.
	newRepo := (Smalltalk at: #MCGitRepository) new
		repoUrl: 'git@github.com:theseion/LibGit';
		ensureDirectory;
		yourself.
		
	MCRepositoryGroup allInstances
		select: [ :group | group repositories includesAnyOf: repos ]
		thenDo: [ :group | 
			repos do: [ :repo | group removeRepository: repo ].
			group addRepository: newRepo ]
]

{ #category : #doits }
BaselineOfDruid >> postLoadActions [
	self changeRepositoryType
]
