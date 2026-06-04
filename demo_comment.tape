# Chapter 3 — Commenting on an issue (on the sauron repo)
# Pressing 'c' on the issue node jumps straight to the comment composer.
# NOTE: this tape POSTS A REAL COMMENT to codedownio/sauron #1 on render.
Output demo_comment.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Down 7
Tab
Sleep 3s
Down 1
Tab
Sleep 4s
Type "s"
Sleep 500ms
Ctrl+e
Type " Markdown"
Sleep 1000ms
Enter
Sleep 2s
Down 1
Tab
Sleep 4s
Show

# Sit on the open issue, then jump straight to the comment composer with 'c'
Source pause_medium.tape
Type "c"
Source pause_medium.tape

# Compose a comment with markdown + a fenced code block (Preview renders live)
Type "Here's some Python:"
Enter
Enter
Type "```python"
Enter
Type "print('syntax highlighting works')"
Enter
Type "```"
Source pause_long.tape

# Submit (Meta/Alt+Enter), show it post, then close the composer
Alt+Enter
Source pause_fetch.tape
Ctrl+q
Source pause_medium.tape
