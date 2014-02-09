<h1>Emacs</h1>

<h2>P4 Sync Revivion:</h2>
    Emacs p4 plugin doesn't have provision to sync a file for given revision. p4-sync-revision enables to sync given revision of the current buffer file if its not opened for pending changelist.

<h3>p4-fstat-output</h3>
	Function to run fstat command and parse the output and returns matched value for given regex.
Ex:</br> 
- To get depot file : (p4-fstat-output "-T depotFile" "depotFile \\(.+\\)$")</br>
- To get headRev: (p4-fstat-output "-T headRev" "headRev \\([0-9]+\\)$")

<h3>p4-get-head-revision:</h3>
	Runs fstat -T headRev and process output to get the head rev.

<h3>p4-sync-revision:</h3>
	It is a p4cmd function, which gets the file revision less than the head revision and sync the same.
- File should be controlled by perforce and it shouldn't have opened for pending changelist.</br>
- file revision 0 is not allowed, since it is used to delete the file. Use p4-delete to do the same.</br>
- file revision should be less then or equal to head revision(fstat -T headRev).
