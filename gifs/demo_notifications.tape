# Chapter 6 — Notifications (auto-jump to the latest comment)
Output demo_notifications.gif
Source header.tape

Hide
Type@1ms "sauron -c $HOME/tools/sauron/demo_config.yaml"
Enter
Sleep 6s
Show

# Open the notifications list
Source pause_medium.tape
Tab
Source pause_fetch.tape

# Scroll through a few notifications
Down@500ms 3
Source pause_short.tape
Up@400ms 3
Source pause_short.tape

# Open one — it fetches the thread and jumps straight to the latest comment,
# highlighting it. Hold long enough to take that in.
Down 1
Tab
Sleep 8s
