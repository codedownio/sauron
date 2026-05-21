Output demo_overview.gif

Set FontSize 18
Set Width 1400
Set Height 1000

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 2s
Show

Sleep 1s

# Open notifications
Tab
Sleep 2s

# Scroll through some notifications
Down@200ms 3
Sleep 1s

# Close notifications
Left 1
Sleep 1s

# Scroll down through repos
Down@200ms 5
Sleep 1s

# Open a repo's issues/PRs
Tab
Sleep 1s
Down@200ms 2
Sleep 1s

# Close and continue scrolling
Left 1
Sleep 1s
Down@200ms 5
Sleep 1s

# Open another repo
Tab
Sleep 1s
Down@200ms 2
Sleep 1s
Left 1
Sleep 1s

# Scroll down through more headings
Down@200ms 5
Sleep 1s
Down@200ms 5
Sleep 2s
