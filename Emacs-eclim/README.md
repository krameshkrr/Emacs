<h1>Eclim-workspace:</h1>
Enabling easy way to open eclipse workspace through eclim. 
- Listing recent eclipse workspaces and allowing to choose from them
- Unlock the workspace if its restricted to have one instance of workspace
- Starting eclimd for the workspace
- Runs user defined hooks once eclims started. (Ex: you can project explorer for the workspace)

<h2>Install</h2>
Copy eclim-workspace.el to your load path and byte compile the same. Add below line to your emacs init file.
```
(require 'eclim-workspace)
```

Apart from the required variables for eclimd and eclim, Set below variables in your init file
```
eclim-ws-parent-dir - Root directory for all your workspaces
eclim-ws-eclipse-config-dir - eclipse configuration directory where all the configurations are stored.
```

To open workspace: M-x eclim-ws-open or you can set any key bindings to this function.

<h2>Customization:</h2>
You can add your additional customization when eclimd starts by adding hook.
```
eclim-ws-eclimd-start-hook
```
