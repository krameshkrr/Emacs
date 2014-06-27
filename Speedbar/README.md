<h1>Speedbar customization:</h1>

Speedbar customization includes below useful functions to make it more user-friendly.

<h2>Set default directory from cursor point:</h2>
Set parent directory(least opened ancesstor) of the cursor line file as defalut directory in speedbar. This will enable you to view the directory hierarchy in better way (Simply get rid off grandparent and its above directories to get clear view of current hierarchy).

<h2>Goto parent directory:</h2>
Simply go back to the parent directory of default until the root.

<h2>Set buffer directory in speedbar:</h2>
Sometimes you may want to navigate through directories in speedbar and you just need to reset it back the directory you are currently working on. This will enables you to set current buffer's directory(the file you are currently editing) in speedbar. </br></br>
Making sure below use cases before setting speedbar's default directory:
<li>The buffer should be associated with some file(temporary buffers would not be associated with any files).
<li>Speedbar should have opened.
