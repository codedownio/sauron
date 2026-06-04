# Chapter 5 — Branches (ahead/behind, checks, PR status) on aeson-typescript
# Press 'b' to jump to the branches node, open a branch, then a commit within it.
Output demo_branches.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Down 2
Tab
Sleep 3s
Show

# Jump to the Your-branches section and open it
Source pause_short.tape
Type "b"
Source pause_short.tape
Tab
Source pause_fetch.tape

# Take in the columns (ahead/behind, checks, last commit, PR), then open a branch
Source pause_medium.tape
Down 1
Source pause_short.tape
Tab
Source pause_long.tape

# Open a commit within the branch to see its changes
Down 1
Source pause_short.tape
Tab
Source pause_long.tape
Ctrl+v
Source pause_medium.tape
