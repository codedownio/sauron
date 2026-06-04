# Chapter 4 — Workflow runs & job logs (on the sandwich repo)
# Jumps to a failed run, into a failed job, down to the failed "Build" step,
# and scrolls to the compiler error.
Output demo_workflows.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Down 5
Tab
Sleep 3s
Show

# Jump to the Actions section and open it
Source pause_short.tape
Type "a"
Source pause_short.tape
Tab
Source pause_fetch.tape

# The latest run failed — open it to see its jobs (a mix of passed and failed)
Source pause_medium.tape
Down 1
Source pause_short.tape
Tab
Source pause_fetch.tape
Source pause_medium.tape

# Open a failed job (its step list can take a few seconds to load)
Down 1
Tab
Sleep 6s
# Go down to the step that failed (Build) and open it
Down@200ms 10
Source pause_short.tape
Tab
Source pause_fetch.tape

# Scroll down through the build output to the error
Ctrl+v
Source pause_medium.tape
Ctrl+v
Source pause_medium.tape
Ctrl+v
# Rest on the failure
Sleep 4s
