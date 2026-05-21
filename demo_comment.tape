Output demo_comment.gif

Set FontSize 18
Set Width 1400
Set Height 1000

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 2s
Down@200ms 7
Sleep 1s
Tab
Sleep 1s
Down 1
Sleep 1s
Tab
Sleep 1s
Type "s"
Sleep 100ms
Space
Type "markdown"
Sleep 2s
Enter
Sleep 2s
Show

# Go to markdown issue
Down 1
Sleep 1s
Tab
Sleep 1s
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms
Ctrl+v
Sleep 500ms

# Zoom the issue
Type "z"
Sleep 500ms

# Jump to the end
Ctrl+End
Sleep 1s

# Go into comment mode
Type "c"
Sleep 1s

# Type and submit a new comment
Type "Here's a **new** comment!"
Enter
Enter
Type "```python"
Enter
Enter
Type "```"
Up
Type "print('Syntax highlighted code')"
Sleep 2s
Alt+Enter
Sleep 5s
