# Chapter 2 — Issue browsing & search (on the sauron repo)
# Ends with issue #1 (Markdown test issue) open, scrolled to the bottom.
Output demo_issues.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Down 7
Tab
Sleep 3s
Down 1
Show

# Sit on the Issues section a beat before opening it
Source pause_medium.tape
Tab
Source pause_fetch.tape

# Scroll through the open issues
Down@500ms 4
Source pause_short.tape
Up@400ms 4
Source pause_short.tape

# Search — append a term to the GitHub qualifiers to narrow down
Type "s"
Source pause_short.tape
Ctrl+e
Type " Markdown"
Source pause_medium.tape
Enter
Source pause_medium.tape

# Open the matching issue (short pause, then straight into scrolling)
Down 1
Source pause_short.tape
Tab
Source pause_long.tape

# Scroll all the way through the rendered markdown body
Ctrl+v
Source pause_medium.tape
Ctrl+v
Source pause_medium.tape
Ctrl+v
Source pause_medium.tape
Ctrl+v
Source pause_medium.tape
Ctrl+v
# Longer hold at the bottom
Sleep 4s
