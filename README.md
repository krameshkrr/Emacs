<h1>Emacs</h1>

<h2>P4 Sync Revivion:</h2>
p4-sync-revision enables to sync given revision of the file if its not opened for pending changelist.

<h3>p4-fstat-output</h3>
Function to run fstat command and parse the output and returns matched value for given regex.
Ex:</br> 
- To get depot file : (p4-fstat-output "-T depotFile" "depotFile \\(.+\\)$")</br>
- To get headRev: (p4-fstat-output "-T headRev" "headRev \\([0-9]+\\)$")

<h3>p4-get-head-revision :</h3>
Runs fstat -T headRev and process output to get the head rev.

<h3>p4-sync-revision :</h3>
It is a p4cmd function, which gets the file revision less than the head revision and sync the same.</br>
- File should be controlled by perforce and it shouldn't have opened for pending changelist.</br>
- file revision 0 is not allowed, since it is used to delete the file. Use p4-delete to do the same.</br>
- file revision should be less then or equal to head revision(fstat -T headRev).

<h2>Customized P4 Submit:</h2>
we can't submit current buffer file by using existing p4-submit function, instead it will show all opened files and unnecessary files should be deleted before submiting. The customized p4-submit function will allow you to submit the current file(only if it is opened for some operation(edit, add etc.,), otherwise it will show all the opened files to submit) and it will prompt you if you are trying to submit an empty diff file.

<h3>p4-empty-diff-p :</h3>
Instead of checking all the files opened in perforce, modified this function to check for given file if we pass the file name to this function.

<h3>p4-submit-set-current-file :</h3>
The files other than the current file in submit form will get removed from p4-submit form.

<h3>p4-async-process-command :</h3>
Modified p4-submit process is being done as below:</br>
- p4 change -o  => To edit the description of the current changelist before submitting the changelist</br>
- Deleting the files other than the current buffer file to submit single file in a changelist if the file name is given to this function.</br>
- call p4-submit.

<h3>p4-submit :</h3>
Getting the file name of the current buffer and checks whether the file is opened for edit/add and it has some diff with the depot file and calling p4-async-process-command function to submit the changelist.

<h3>Customized which-func mode</h3>
- Which Function mode is a global minor mode.  When enabled, the current function name is continuously displayed in emacs mode line, in certain major modes.
- Modified format of the function name displayed in the mode-line, since fully qualified name of the function name may exceed the window width.
- As of now, I've hard coded the max-length of the function name displayed in the mode-line. This should be customized based on the window-width and rest space in the mode-line.
- Enable this for particular major mode:</br>
<code>
    ;; Adding hook to the cperl mode  
	(eval-after-load 'cperl-mode  
	  '(add-hook 'cperl-mode-hook 'set-which-func-current t))
</code>
