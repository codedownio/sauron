<p align="center">
  <img src="logo.jpeg" width="200" alt="sauron logo" />
</p>

<h1 align="center">sauron</h1>

<p align="center">
  A terminal UI for your GitHub repositories.
  <br />
  Issues, pull requests, workflow runs, branches, and notifications — all in one place.
</p>

---

![demo](./demo.gif)

## Features

- **Multi-repo dashboard** — organize repos into named sections with a simple YAML config
- **Issues & pull requests** — browse, search, comment, close/reopen, and create new issues
- **Workflow runs & job logs** — monitor CI status, drill into job logs with syntax highlighting
- **Branches** — view all, yours, active, and stale branches with ahead/behind counts
- **Notifications** — view and manage GitHub notifications, mark as read/done
- **Health checks** — periodic background polling to keep statuses up to date
- **Split view** — optional side-by-side layout with logs panel (`--split-logs`)
- **Keyboard-driven** — full hotkey support for fast navigation

## Why Sauron?

Most Git TUIs (`lazygit`, `gitui`) focus on local operations — staging, committing, rebasing. They don't talk to the GitHub API. The `gh` CLI can fetch GitHub data but isn't an interactive dashboard. Sauron fills the gap: a persistent, keyboard-driven TUI that watches **multiple repos at once** and gives you live access to issues, PRs, CI runs, and notifications without leaving the terminal.

| Feature | Sauron | `gh` CLI | `gh-dash` | lazygit | gitui |
|---|:---:|:---:|:---:|:---:|:---:|
| Multi-repo dashboard | :white_check_mark: | :x: | :white_check_mark: | :x: | :x: |
| Browse/search issues and PRs | :white_check_mark: | list only | :white_check_mark: | :x: | :x: |
| Comment on issues/PRs | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: |
| Close/reopen issues/PRs | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: |
| Create issues | :white_check_mark: | :white_check_mark: | :x: | :x: | :x: |
| Syntax highlighting | :white_check_mark: | :x: | :x: | :white_check_mark: | :white_check_mark: |
| Monitor workflow runs | :white_check_mark: | :x: | :x: | :x: | :x: |
| Drill into CI job logs | :white_check_mark: | `--log` flag | :x: | :x: | :x: |
| Auto-refresh / health checks | :white_check_mark: | :x: | :white_check_mark: | :x: | :x: |
| GitHub notifications | :white_check_mark: | :x: | :white_check_mark: | :x: | :x: |
| Branches with ahead/behind | :white_check_mark: | :x: | :x: | :white_check_mark: | :x: |
| YAML config for repos | :white_check_mark: | :x: | :white_check_mark: | :x: | :x: |
| Local git operations | :x: | :x: | :x: | :white_check_mark: | :white_check_mark: |
| Open in browser | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: |

## Install

### Pre-built binaries

Download the latest release for your platform from [GitHub Releases](https://github.com/codedownio/sauron/releases):

```bash
# Linux (x86_64)
TMPDIR=$(mktemp -d)
curl -sL https://github.com/codedownio/sauron/releases/latest/download/sauron-x86_64-linux-0.1.0.1.tar.gz | tar xz -C "$TMPDIR"
sudo mv "$TMPDIR/sauron" /usr/local/bin/
rmdir "$TMPDIR"

# macOS (Apple Silicon)
TMPDIR=$(mktemp -d)
curl -sL https://github.com/codedownio/sauron/releases/latest/download/sauron-aarch64-darwin-0.1.0.1.tar.gz | tar xz -C "$TMPDIR"
sudo mv "$TMPDIR/sauron" /usr/local/bin/
rmdir "$TMPDIR"

# macOS (Intel)
TMPDIR=$(mktemp -d)
curl -sL https://github.com/codedownio/sauron/releases/latest/download/sauron-x86_64-darwin-0.1.0.1.tar.gz | tar xz -C "$TMPDIR"
sudo mv "$TMPDIR/sauron" /usr/local/bin/
rmdir "$TMPDIR"
```

### From source (Nix)

```bash
nix run github:codedownio/sauron/v0.1.0.1
```

### From source (Stack)

```bash
git clone https://github.com/codedownio/sauron.git
cd sauron
stack install
```

## Setup

On first run, sauron will walk you through GitHub OAuth authentication. You can also pass a token directly:

```bash
sauron --token YOUR_GITHUB_TOKEN
```

## Configuration (single-repo)

Just run sauron in the directory of a given repo, and it will show that repo!

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
