<h1>Emacs</h1>

<h2>P4 Customization:</h2>
<h3>P4 Sync Revivion:</h3>
p4-sync-revision enables to sync given revision of the file if its not opened for pending changelist.

<h3>Customized P4 Submit:</h3>
Existing p4 submit function limits us to submit all the opened files. The customized p4-submit function will allow you to submit the current file(only if it is opened for some operation(edit, add etc.,), otherwise it will show all the opened files to submit) and it will prompt you if you are trying to submit an empty diff file.

<h2>Color theme:</h2>
A dark theme based on vi default theme.
</br></br>
![alt tag](http://postimg.org/image/s3xp94b49/09192ca3/)

<h2>Customized which-func mode:</h2>
- Which Function mode is a global minor mode.  When enabled, the current function name is continuously displayed in emacs mode line, in certain major modes.
- Modified format of the function name displayed in the mode-line, since fully qualified name of the function name may exceed the window width.
- As of now, I've hard coded the max-length of the function name displayed in the mode-line. This should be customized based on the window-width and rest space in the mode-line.
- Enable this for particular major mode:  
<code>
    ;; Adding hook to the cperl mode  
    (eval-after-load 'cperl-mode  
	  '(add-hook 'cperl-mode-hook 'set-which-func-current t))
</code>

<h2>Kill-temporary-buffers:</h2>
Simple snippet to Kill unnecessary buffers such as *compilation*, *P4-output* etc., except some valid buffers(*scratch*, *Messages*).