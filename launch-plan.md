# Sauron Launch Plan

## Pre-Launch (1–2 weeks before)

### Polish the repo

- Add a 30-second "what you get" code snippet right at the top of the README (before the GIF)
- Be explicit about what makes Sauron better than the GitHub web UI or `gh` CLI
- Add a comparison table vs. `gh`, `gitui`, `lazygit`, etc.
- Focus on outcomes, not methods — people star projects they understand and trust

### Prepare distribution

- Add pre-built binaries to the GitHub release (Linux x86_64, macOS aarch64 at minimum)
- Consider adding to Homebrew and nixpkgs
- Friction at install kills adoption

### Write a "How I built Sauron" post

- Publish on Dev.to or your own blog 1–2 days before launch so it's indexed when traffic peaks
- Lean into "why Haskell for a TUI" as a hook — this is genuinely interesting to the HN crowd

### Seed early feedback

- Reach out to 5–10 trusted people to review your README, landing page, and initial posts before launch

## Launch Day

### Hacker News (primary channel)

- Title: "Show HN: Sauron – A terminal UI for all your GitHub repos"
- The Haskell community punches above its weight on HN
- Post HN first, then Reddit ~30 minutes later to stagger feedback

### Reddit targets

- **r/haskell** — guaranteed engagement, focus on implementation
- **r/commandline** — focus on UX and workflow
- **r/programming** — general audience, lead with the problem it solves
- **r/selfhosted** — if applicable, emphasize the multi-repo dashboard angle

### Timing

- Tuesday–Thursday, 8–10 AM PT for maximum visibility

### Engage aggressively

- Respond to comments within two hours — builds goodwill and boosts algorithmic visibility
- Be honest about limitations and roadmap
- Don't argue with critics — acknowledge feedback gracefully

## Post-Launch (weeks 1–4)

### Steady cadence

- Users prefer steady progress over sporadic bursts
- Even "fixed three bugs and improved documentation" counts as a meaningful update

### Attract contributors

- Label issues "good first issue"
- Provide clear build instructions and an architecture overview
- Be upfront about contribution friendliness to counteract the "Haskell is scary" factor

### Follow-up content

- Comparison post: Sauron vs. gh vs. lazygit
- Short tutorial on multi-repo YAML config
- "Week 1 retrospective" post

## Key Differentiators to Lead With

These are things most GitHub TUIs don't do — lead with them in every pitch:

- **Multi-repo dashboard** with YAML config
- **Workflow run monitoring** with job log drill-down
- **Notifications management** from the terminal
- **Split-view mode** (`--split-logs`)

The "tower's-eye view" tagline is strong — drive home that this is about *observability across repos*, not just another git TUI.

## What Not to Do

- Don't ask people to star the repo
- Don't post to 10 subreddits simultaneously
- Don't oversell — being honest about limitations builds more trust than hype
