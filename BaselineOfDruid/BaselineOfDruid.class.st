Class {
	#name : #BaselineOfDruid,
	#superclass : #BaselineOf,
	#category : #BaselineOfDruid
}

{ #category : #baselines }
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
		spec
			baseline: 'PharoCodeGenerator'
			with: [ spec repository: 'github://juliendelplanque/PharoCodeGenerator/src' ].
		spec
			baseline: 'AIGraphAlgorithms'
			with: [ spec
				className: 'BaselineOfAIGraphAlgorithms';
				repository: 'github://pharo-ai/graph-algorithms:master' ].
		spec
			package: 'Druid' with: [ spec requires: #('PharoVM' 'PharoCodeGenerator' 'AIGraphAlgorithms') ];
			package: 'Druid-Tests' with: [ spec requires: 'Druid' ];
			package: 'Druid-UI' with: [ spec requires: #('Druid' 'Druid-Tests') ].

		spec
			group: 'Core' with: #('Druid' );
			group: 'DruidTests' with: #('Core' 'Druid-Tests');
			group: 'UI' with: #('Druid-UI');
			group: 'default' with: #('Druid' 'DruidTests' 'Druid-UI') ]
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
