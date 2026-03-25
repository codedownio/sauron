<p align="center">
  <img src="logo.jpeg" width="200" alt="sauron logo" />
</p>

<h1 align="center">sauron</h1>

<p align="center">
  A terminal UI dashboard for monitoring your GitHub repositories.
  <br />
  Issues, pull requests, workflow runs, branches, and notifications — all in one place.
</p>

---

![demo](https://github.com/user-attachments/assets/TODO)

## Features

- **Multi-repo dashboard** — organize repos into named sections with a simple YAML config
- **Issues & pull requests** — browse, search, comment, close/reopen, and create new issues
- **Workflow runs & job logs** — monitor CI status, drill into job logs with syntax highlighting
- **Branches** — view all, yours, active, and stale branches with ahead/behind counts
- **Notifications** — view and manage GitHub notifications, mark as read/done
- **Health checks** — periodic background polling to keep statuses up to date
- **Split view** — optional side-by-side layout with logs panel (`--split-logs`)
- **Keyboard-driven** — full hotkey support for fast navigation

## Install

### From source (Stack)

```bash
git clone https://github.com/codedownio/sauron.git
cd sauron
stack install
```

### From source (Nix)

```bash
nix build github:codedownio/sauron
```

## Setup

On first run, sauron will walk you through GitHub OAuth authentication. You can also pass a token directly:

```bash
sauron --token YOUR_GITHUB_TOKEN
```

## Configuration (single-repo)

Just run sauron in the directory of a given repo, and it will load the repo.

## Configuration (all repos)

To browse all the repos owned by your GitHub account, run

```bash
sauron --all
```

## Configuration (organized repos)

Create a YAML config file to define which repos to monitor:

```yaml
settings:
  check_period: 600000000  # Health check interval in microseconds

sections:
- display_name: "My Projects"
  repos:
  - owner/repo-one
  - owner/repo-two
  - name: owner/important-repo
    settings:
      check_period: 300000000

- display_name: "Team"
  repos:
  - myorg/*  # All repos from an org
```

Then run:

```bash
sauron -c path/to/config.yaml
```

Or just put your config file in ~/.config/sauron/config.yaml, and sauron will load it from there automatically.

## Keybindings

| Key | Action |
|-----|--------|
| `Enter` / `Tab` | Toggle open/close |
| `n` / `p` | Next / previous item |
| `o` | Open in browser |
| `H` `I` `P` `A` | Open repo home / issues / pulls / actions in browser |
| `r` / `R` | Refresh selected / refresh all |
| `s` | Search |
| `c` | Comment on issue/PR or create new issue |
| `C` | Close/reopen issue or cancel workflow |
| `z` | Zoom into selected item |
| `N` `B` `F` `L` | Next / back / first / last page |
| `d` | Mark notification done |
| `q` | Quit |

## CLI Options

```
sauron [OPTIONS]

  --token STRING       OAuth token for GitHub
  -c, --config FILE    Config file path (YAML)
  -r INT               Max concurrent GitHub API requests (default: 10)
  --debug-file FILE    Write debug logs to file
  --auth               Force OAuth authentication flow
  --all                Show all repos for the authenticated user
  --color-mode MODE    Force color mode: full, 240, 16, 8, none
  --split-logs         Split view: app on left, logs on right
```

## License

BSD-3-Clause
