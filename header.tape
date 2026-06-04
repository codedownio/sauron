# Shared VHS settings for all sauron demo tapes.
# Source'd at the top of every demo_*.tape; do not set Output here.

Set FontSize 18
Set Width 1400
Set Height 1000
# Set Theme "Catppuccin Mocha"   # uncomment & pick one for a consistent palette

# Pause "variables" — vhs has no real variables, but `Source`-ing a file runs
# its commands inline, so each pause lives in ONE place and is reused via
# `Source pause_*.tape`. Retune a duration by editing the file:
#   pause_short.tape  = 700ms    after a keypress / small navigation
#   pause_medium.tape = 1500ms   let a view settle, read a beat
#   pause_long.tape   = 3s       sit on something so the viewer absorbs it
#   pause_fetch.tape  = 4s       wait for a data fetch (issue / job / branch load)
# A few deliberate one-off holds (issue end, notification open) use an explicit
# longer Sleep, commented where they appear.
