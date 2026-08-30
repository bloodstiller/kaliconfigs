# exegol-setup.sh — Improvement Backlog

Context for whoever (whichever Claude) picks this up: `exegol-setup.sh` is a
re-runnable, checkpointed Ubuntu host-bootstrap script for an Exegol pentest
setup. Each unit of work is gated by `is_done "<checkpoint>"` /
`mark_done "<checkpoint>"` against `~/.exegol_setup_checkpoints/`, wrapped in
`section "Label" "🔤"` / `skip_section`, and most commands run through the
`spin` (fatal on failure, prints captured output) or `spin_soft`
(warns, non-fatal) helpers. `TOTAL_STEPS` must match the actual number of
`section "..."` calls in the file — miscounting it breaks the progress bar's
percentage math (this has already happened once).

None of the below has been implemented yet — this is a prioritized list to
work through, not a changelog.

---

## 1. Security

### 1.1 SSH key material passed via process argv (highest priority)
`deploy_ssh_keys()` currently does:
```bash
python3 - "$HOME/.ssh" "$decrypted" <<'PYEOF'
```
`$decrypted` (the sops-decrypted private key material) becomes a command-line
argument to the `python3` process, which means it's visible — even if only
briefly — to any other process on the box via `ps aux` / `/proc/<pid>/cmdline`.
For a script whose entire job in this section is handling private key
material, that's the wrong pattern.

**Fix direction:** pass `$decrypted` via stdin instead, or write it to a
`mktemp` file created with `install -m 600 /dev/null` (mirroring the pattern
already used for `AGE_KEY_FILE`) and have the Python heredoc read from that
path, deleting it immediately after with a `trap`/explicit `rm` even on
failure paths.

### 1.2 No checksum/signature verification on downloaded binaries
Every download in the script trusts HTTPS + `wget` blindly:
- Eclipse Temurin JDK tarball (`_temurin_url`)
- sops binary
- Nerd Fonts zips
- Obsidian `.deb`
- ligolo-ng agent/proxy binaries (run on target machines during engagements)

The last two are particularly worth hardening since they end up executed on
client infrastructure during engagements, not just your own host.

**Fix direction:** most of these publish `SHA256SUMS` / per-asset checksums
alongside releases (GitHub releases JSON already gives you the asset list in
`_gh_api`'s response — the checksum asset, if published, is just another
entry in that same array). Verify after download, before `chmod +x` /
extraction / `apt install`. Fail the `spin` block (or the individual loop
iteration, for ligolo-ng) if the hash doesn't match rather than silently
proceeding.

### 1.3 Inconsistent secret handling
SSH keys go through the full sops + age encrypted-vault treatment. Nessus
credentials (`~/Tools/.env`) are plaintext, `chmod 600`, no encryption at
rest. Possibly intentional (different threat models), but worth an explicit
decision rather than it just being how the script evolved. If it should be
consistent, the Nessus `.env` could be sops-encrypted too and decrypted at
`start-nessus.sh` runtime the same way `deploy_ssh_keys` does.

---

## 2. Reliability

### 2.1 `curl` calls have no timeout (same class of bug as the wget hangs)
We already fixed every `wget -q` call to include
`--timeout=30 --tries=3 --waitretry=3` after ligolo-ng downloads hung
indefinitely and forced a manual Ctrl+C. The exact same failure mode still
exists on every bare `curl -s` call:
- `_gh_api()` (used by Obsidian, Nerd Fonts, sops, ligolo-ng)
- `_temurin_url()` (JDK resolution for Burp Pro)
- Docker GPG key fetch (`curl -fsSL https://download.docker.com/...`)
- Google Cloud GPG key fetch (`curl -fsSL https://packages.cloud.google.com/...`)

**Fix direction:** add `--connect-timeout 15 --max-time 30` (or similar) to
every `curl` invocation, consistent with the wget flags already applied
elsewhere. For `_gh_api` specifically, consider a retry loop (2–3 attempts
with backoff) before treating it as a hard failure, since transient network
blips shouldn't force manual re-runs of an entire section.

### 2.2 `git clone`/`git pull` have no stall protection either
Same underlying issue, different transport. Affects: `kaliconfigs` dotfiles
clone, Doom Emacs clone, oh-my-zsh plugin clones, tmux plugin manager clone,
HackTricks wiki clone, reverse-shell-generator clone, SharpCollection sparse
clone/pull.

**Fix direction:** `git -c http.lowSpeedLimit=1000 -c http.lowSpeedTime=30
clone ...` (abort if transfer drops below 1000 bytes/sec for 30s) applied
consistently, or wrap clones in `timeout 120 git clone ...`.

### 2.3 SharpCollection's update path uses `git pull --depth 1`
On a shallow clone, `git pull --depth 1` can behave inconsistently depending
on git version/config (shallow history + merge semantics don't always play
well together).

**Fix direction:** `git fetch --depth 1 && git reset --hard origin/master`
is the more reliable "just get me the latest state" pattern for a
shallow/sparse checkout that's only ever read from, never committed to.

### 2.4 Apply the ligolo-ng "continue past failure" pattern more broadly?
Worth a deliberate decision: right now, most single-item downloads (JDK,
Obsidian, sops, Nerd Fonts) are still fatal-by-default via bare `spin` calls
— any failure aborts the whole script, matching the rest of the script's
existing philosophy. Only the ligolo-ng loop (many independent platform
assets) was changed to the `if spin ...; then ... else ... fi` pattern so
one bad asset doesn't block the other 29. Confirm this split (single item =
fatal, multi-item loop = per-item soft-fail) is the intended policy, or
extend the soft-fail pattern elsewhere if not.

---

## 3. Maintainability / workflow

### 3.1 No way to target a single checkpoint from the CLI
Re-running one section currently means manually
`rm ~/.exegol_setup_checkpoints/<name>` first. Fine occasionally, tedious
when iterating on the script itself.

**Fix direction:** add `--only=<checkpoint_name>` and/or `--force` flags,
parsed at the top of the script, that either skip straight to one section or
clear its specific checkpoint file before running normally.

### 3.2 Most tools install-once, never update on re-run
Docker, the Exegol wrapper itself, Oh My Zsh + plugins, Doom Emacs, tmux
plugin manager, etc. are all "install if missing, otherwise skip" — no
update path on subsequent runs. Only the sections touched most recently
(ligolo-ng, SharpCollection, and partially JDK/sops/Nerd Fonts/Obsidian via
their "latest release" lookups) have real update-awareness.

**Fix direction:** decide if the intended model is "install once per VM,
rebuild VM to refresh" (in which case, document that explicitly at the top
of the script) or "this script is meant to be idempotently re-run to catch
updates" (in which case, the `is_done` checks would need to become
version-aware the way ligolo-ng's `.version` stamp files are, rather than
just presence-of-directory checks).

### 3.3 Closing instructions only live in terminal scrollback
The final printed block (Burp activation steps, Nessus credential setup,
VPN flag reminder, cloud tooling notes, etc.) is long and useful — but only
exists in scrollback, gone after the terminal clears or the session ends.

**Fix direction:** write the same content to
`~/Tools/NEXT_STEPS.md` (or similar) at the end of the run, so the manual
follow-up steps survive independently of the terminal session.

---

## 4. Smaller / optional

### 4.1 No installed-version manifest
JDK, sops, Nerd Fonts, Obsidian, and ligolo-ng are all "latest at time of
run" with nothing recording which version actually landed on a given box.
Could matter if a specific toolchain version ever needs to be reproduced for
a report or to explain a discrepancy between engagement boxes.

**Fix direction:** append resolved versions to a simple
`~/.exegol/my-resources/INSTALLED_VERSIONS.txt` (or JSON) as each section
completes.

### 4.2 No log rotation
`exegol-setup-<timestamp>.log` files accumulate in `$HOME` indefinitely.

**Fix direction:** simple cleanup at script start — e.g., delete
`exegol-setup-*.log` files older than N days, or cap to the last N runs.

