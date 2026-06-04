# Chapter 1 — Repo viewing (multi-repo dashboard)
# Ends with codedownio/sauron's Issues section opened.
Output demo_overview.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Show

# Take in the multi-repo dashboard
Source pause_medium.tape

# Open a repo to reveal its sections, and sit on it a beat
Down@400ms 2
Source pause_short.tape
Tab
Source pause_long.tape

# Collapse it and move down to the sauron repo
Left
Source pause_short.tape
Down@400ms 5
Source pause_short.tape

# Open sauron, go to its Issues section, and open that too
Tab
Source pause_medium.tape
Down 1
Source pause_short.tape
Tab
Source pause_fetch.tape
