module Sauron.SampleConfig (
  sampleConfig
  ) where

import Data.String.Interpolate (i)
import Relude


sampleConfig :: String
sampleConfig = [i|
settings:
  \# Default health check period for repos (in microseconds)
  check_period: 600000000

nodes:
  \# GitHub notifications
  - type: notifications

  \# Paginated issue search
  - type: issues
    display_name: "My Issues"
    query: "is:issue state:open archived:false assignee:@me sort:updated-desc"

  \# Heading with paginated pull request searches
  - type: heading
    display_name: "My Pulls"
    open: true
    children:
    - type: pulls
      display_name: "Needs your review"
      query: "is:pr is:open review-requested:@me sort:updated-desc"
    - type: pulls
      display_name: "Needs action"
      query: "is:pr is:open author:@me review:changes_requested sort:updated-desc"
    - type: pulls
      display_name: "Ready to merge"
      query: "is:pr is:open author:@me review:approved sort:updated-desc"

  \# Headings with repos (type defaults to "repo" when omitted)
  - type: heading
    display_name: "Web Frameworks"
    open: true
    children:
    - name: vercel/next.js
    - name: remix-run/remix
    - name: sveltejs/svelte

  - type: heading
    display_name: "Languages & Compilers"
    children:
    - name: rust-lang/rust
    - name: golang/go
    - name: ziglang/zig

  - type: heading
    display_name: "Tools"
    children:
    - name: neovim/neovim
    - name: kovidgoyal/kitty
    - name: NixOS/nixpkgs
      settings:
        check_period: 1200000000

  \# Standalone repo at top level
  - name: anthropics/anthropic-sdk-python
|]
