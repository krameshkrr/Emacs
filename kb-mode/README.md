<h1>Emacs</h1>

<h2>kb-mode (kill-buffer minor mode)</h2>
Minor mode for managing temporary buffers which enables to kill temporary buffers easily by providing user-friendly functions to select buffers to be killed more importanytly timer object can be set to kill the temporary buffers automatically in given interval.

<h3>Install</h3>
Copy kb-mode.el to your load-path and add below snippet to your emacs init file.</br>
```
    (require 'kb-mode)
    (kb-minor-mode 1)
```
<h3>Customizations</h3>
The below variables can be customized for your emacs session.
<h4>Variables</h4>
```
kb-default-temp-buffer-regex "\\*.+\\*"
  "String: Default regex to match temp buffers."

kb-kill-indirect-buffers nil
  "Boolean: Flag to indicate whether or not to kill indirect buffers."

kb-kill-if-modified nil
  "Boolean: Flag to indicate whether or not to kill the buffer if it has modifed content."

kb-prompt-for-running-process nil
  "Boolean: Flag to indicate whether or not to prompt user when killing a buffer in which a process run."

kb-kill-anyway nil 
  "Boolean: Flag to indicate whether or not kill the buffers regardless of its state such as modified, indirect buffer etc."

kb-clean-periodically t
  "Boolean: Flag to indicate whether or not to run temp buffer cleaning periodically."

kb-clean-interval 1000
  "Number: Time interval to run temp buffer cleaning."
  
kb-idle-time-max 900
  "The number of seconds before some buffers become eligible for killing."

kb-kill-visible-buffer nil
  "Flag to indicate whether or not to kill temp buffer which is visible to the user."

kb-kill-buffers-without-any-file t
  "Flag to use the buffers which are not associated with any file instead of default regex."

```

<h4>Hooks</h4>
```
kb-minor-mode-hook
  "*Hook called when temp-buffer-manager minor mode is activated or deactivated."

kb-minor-mode-timer-hook
  "*Hook called when temp-buffer-manager minor mode timer is started."
```

<h3>Required thirty party packages</h3>
<li> ido-mode </li>

<h3>Defalut temporary buffer selection:</h3>
Set either selection regex(currently '\*\.+\*') or kb-kill-buffers-without-any-file to kill all the buffers which are not associated with any file.

<h3>Manage Buffer exceptions</h3>
To add or delete buffer exceptions in order to prevent some buffers to kill.
```
kb-add-current-buffer-to-exception
    Current default buffer will be added to the exception list.
kb-remove-current-buffer-from-exception
    Current default buffer will be deleted from exception list.
kb-exception-add 
    Select any existing buffers from the choices given to add to the exception list. 
kb-exception-delete
    Select any buffer added to the exception list to remove.
```
<h3>Manage Major-Mode exceptions</h3>
Add or delete major-modes to the exception list to prevent those modes to be killed.
```
kb-mode-exception-add
kb-mode-exception-delete
```

<h3>Manage buffer state</h3>
Considering below buffer states before killing.
```
1. whether buffer has modified content
    By default the modified buffer will not be deleted unless kb-kill-if-modified is set.
2. Whether an active process in running on the buffer
    if kb-prompt-for-running-process is set, user will be prompted to decide whether the buffer should be deleted.
3. idle buffers: Set maximuum idle time for buffers to become eligible for killing.
4. Visible buffers: Set kb-kill-visible-buffers to kill the currently viewing buffers otherwise it won't kill.
5. Whether the buffer is an indirect buffer (not implemented yet)
```

<h3>Manage Timer</h3>
Set the interval(kb-clean-interval) to clean temporary buffers and start the timer.
```
To start the timer:
    M-x kb-start-timer
To stop the timer:
    M-x kb-stop-timer
```
<h3>Kb-help</h3>
kb-help function let you know the current conditions used to kill temporary buffer. Ex:
```
Temporary buffer management:

Conditions used to kill temp buffers:
Buffer exception list:
      *Messages*, *scratch*

Buffer selection default regex:
      \*.+\*
Buffer selection exception regex:
      nil

Buffer mode exceptions:
      nil

Buffer state conditions:
      Buffers will not be killed if it has modified content.(To customize: kb-kill-if-modified)
      Buffers with running process will be killed without prompting.(To customize: kb-prompt-for-running-process)

Kill buffers periodically:
      To start the timer M-x kb-start-timer and the temp buffers will be killed for each 10 seconds

      To stop the timer M-x kb-stop-timer.
```
