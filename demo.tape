Output demo.gif

Set FontSize 18
Set Width 1400
Set Height 1000

Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
# Sleep 200ms
Enter

# Scroll down to aeson-typescript
Sleep 2s
Down@100ms 7
Sleep 2s
Tab
Sleep 2s

# Go to issues
Down 1
Sleep 1s
Tab
Sleep 2s

# Search for Markdown issue
Type "s"
Sleep 500ms
Space
Type "markdown"
Enter
Sleep 2s

# Go to markdown issue
Down 1
Sleep 500ms
Tab
Sleep 2s

# Zoom the issue
Type "z"
Sleep 2s

# Scroll down slowly
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms
Ctrl+v
# Sleep 500ms
# Ctrl+v
Sleep 2s

# Go into comment mode
Type "c"
Sleep 2s

# Type and submit a new comment
Type "Here's a **new** comment!"
Sleep 2s
Alt+Enter
Sleep 2s

# Close the modal and the issues
Ctrl+q
Left 1
Sleep 500ms
Left 1
Sleep 500ms
Left 1
Sleep 500ms
Down 2

# Open the workflows
Tab
Sleep 1
# Go to the first workflow and open it
Down
Sleep 500ms
Tab
Sleep 1
# Go to the first job and open it
Down
Sleep 500ms
Tab
Sleep 1
# Open the first log entry (?)
Down 1
Sleep 1
Tab
Sleep 1
Type "z"

# Final sleep
Sleep 5s
