"Preloading script"

"Set iceberg to work on HTTPs by default, we don't have SSH on gitlab-CI"
Iceberg remoteTypeSelector: #httpsUrl.

"Load the Pharo VM repository and lock it"
EpMonitor disableDuring: [ 
	Metacello new
		repository: 'gitlocal://pharo-vm';
		baseline: 'VMMaker';
		onConflictUseLoaded;
		load;
		lock
].
