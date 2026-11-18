# Emacs site configuration refactor notes

**Scope:** `site-pkgs.org` refactor based on the previously audited source and keymap scan. The public repository's current `master` version of `site-keys.el` was also checked. The bundle does not replace the rest of `~/.emacs-site/`, because the other active `site-*.el` files were not supplied for editing.

## Refactored files

- `site-pkgs.org` — cleaned, consolidated configuration; every substantive edit has a `;;; #PNX(ID): ...` marker.
- `site-attic.org` — non-tangled archive of removed disabled package declarations and commented-out `use-package` blocks. It is for manual recovery only and is not loaded by `site-boot.el`.
- `site-pkgs.diff` — unified diff against the supplied original `site-pkgs.org`.

## Main changes applied

### Completion and search

- Removed Auto-Complete and IDO; retained Corfu + Vertico/Orderless/Marginalia/Consult.
- Removed Company as a completion UI, including Company-posframe/global activation. Company remains deferred only as a provider for legacy Ansible, Terraform and AUCTeX backends, adapted through Cape's CAPF bridge.
- Removed unused ace-jump-mode and the unbound Avy declaration.
- Removed the redundant ack/ag/deadgrep/rg search frontends and later grep key overrides; Consult + ripgrep is the primary search path. `wgrep` remains for editing grep results.
- Removed the unused deferred `regex-tool` declaration.

### Project and UI cleanup

- `project.el` is the primary project interface: the project Hydra and Dashboard backend now use it, and the Python Behave helper prefers it.
- Projectile remains deferred as a compatibility layer for terminal/Eshell integrations and existing commands; it is not globally enabled. `consult-projectile` and `treemacs-projectile` were removed.
- Removed Minions; Mood-Line is the only active modeline implementation. Delight remains complementary.
- Removed the empty Fontaine section.
- Removed external Which-Key setup and enabled Emacs' built-in Which-Key.
- Replaced stale IDO/Helm buffer Hydra commands with `consult-buffer` and `ibuffer`.

### Folding and keymaps in `site-pkgs.org`

- Removed Kirigami, Savefold, Vimish-Fold and their dead configuration.
- Retained Tree-sitter folding, Outline/outline-indent and Hideshow; the fold Hydra now uses those commands.
- Moved Treemacs to `C-c T`; Python pytest keeps the lowercase `C-c t` prefix. These are case-distinct key sequences, not a direct conflict.
- Moved the lsp-mode prefix to `C-c L`, preserving `C-c l` for the Python backend switcher.
- Removed duplicate ESS assignments: `M-c M-j`, `M-c M-r` and `M-c M-e` each now have one binding; the displaced commands use `M-c M-u` and `M-c M-E`.

### Python and LSP

- Removed Elpy, EIN and pyvenv-auto. `uv-mode` + PET is the environment workflow.
- Fixed the Python `:custom`/`cond` misuse by moving interpreter selection to `:init`.
- Consolidated Flycheck, lsp-mode, Eglot, lsp-ui, Consult-LSP and lsp-treemacs into one LSP configuration; removed the second LSP block and its unconditional Python `lsp-deferred` hooks.
- Added a buffer-local dispatcher on `hack-local-variables-hook`, so `.dir-locals.el` overrides are read before client selection. It stops any existing client before starting exactly one:
  - ordinary Python buffers: lsp-mode + basedpyright + Flycheck + Ruff format-on-save;
  - percent-format files under a `notebooks` directory: Eglot + basedpyright + Flymake, without Ruff format-on-save.
- Both `python-mode` and `python-ts-mode` use the same Eglot basedpyright mapping. `h7/python-lsp-backend` is a safe local variable (`lsp` or `eglot`) for explicit overrides.
- Replaced the unknown `rass` server command with the explicit `basedpyright-langserver --stdio` command. Ruff is retained as the formatter, not configured as a second Eglot language server.
- `h7/lsp-setup` now prints `uv tool install basedpyright` instead of asking lsp-mode to install Microsoft's Pyright.
- Flycheck is enabled for lsp-mode buffers; the dispatcher disables it for Eglot buffers, where Flymake is used.
- Julia retains Eglot-jl; its extended startup timeout is buffer-local. The dormant lsp-julia declaration was removed. R remains ESS-based; no R LSP server is configured.

### Other correctness and startup changes

- Fixed the malformed `hl-todo` `:custom` entries.
- Replaced destructive assignment to `major-mode-remap-alist` with `add-to-list`.
- Removed redundant `package-initialize`, old req-package/Quelpa bootstrap code and the invalid `:ensure f` system-package declaration. Emacs' built-in `use-package` and normal package activation are used.
- Raised GC threshold only during startup and restored it to 16 MiB in `emacs-startup-hook`.
- Switched Org to the Emacs-bundled version (`:ensure nil`) rather than installing a second ELPA copy.
- Removed disabled Org-ref/Org-roam integrations; Citar and Denote remain.
- Removed unactivated ef-themes and other unused deferred declarations.
- Added safe defaults for helper functions that were referenced before later startup files load; Denote and Citar resolve their final paths after `after-init-hook`. Removed diagnostic calls to `h7/use-vertico`, replaced the `h7/use-py-jupyter` dependency with a direct availability check, replaced `emacs-d` path calls, guarded optional `ha-leader`, and corrected two Elfeed command-name typos.

### Disabled and commented-out code

- Active `:disabled` use-package forms were removed from `site-pkgs.org`.
- 81 commented-out `use-package` blocks and the disabled declarations extracted from the original source are archived in `site-attic.org`.
- Explanatory comments and a few intentional examples remain in `site-pkgs.org`; the active file contains no commented-out `use-package` declarations and no active `:disabled` declarations.

## Inactive configuration files

These were identified as **not loaded by the current `site-boot.el` sequence** in the earlier audit. Before deleting them, search for explicit `load`/`require` references and check `custom-file` separately.

- `site-load.el` — legacy loader branch for Emacs versions older than 24.3; not part of the current startup path.
- `site-init.el` — legacy initialization/keymap module; not loaded by the current boot sequence. Its key bindings in the scan should not be treated as active runtime bindings.
- `site-custom.el` — not loaded by the current boot sequence; confirm that `custom-file` does not point to it before removal.
- `site-config.org` — legacy Org configuration source; not tangled or loaded by the current boot sequence.
- `site-local.el` — not loaded by the current boot sequence; current local overrides are in `~/.emacs-local.el`.
- `site-attic.org` — new archival file created by this refactor; intentionally not loaded or tangled.

`site-keys.el` remains active and was not modified in this bundle.

## Key-map issues

### `site-pkgs.org`

- Treemacs was moved from `C-c f` to `C-c T`. Python pytest retains `C-c t`. The old note suggesting that Treemacs occupied `C-c t` was inaccurate for the supplied `site-pkgs.org` source; uppercase/lowercase are distinct key sequences.
- lsp-mode uses `C-c L`; the Python backend switcher uses `C-c l e` and `C-c l l`.
- The project Hydra no longer calls `consult-projectile` or `treemacs-projectile`; the buffer Hydra no longer calls IDO/Helm; the fold Hydra no longer calls Vimish-Fold.
- ESS duplicate assignments were consolidated as listed above.

### `site-keys.el` (public repository `master`)

- **`C-f`** is bound to `consult-line`, overriding Emacs' normal `forward-char`. It is also redundant with the existing Consult search binding (`M-s l`). Consider removing this global override to restore normal cursor movement.
- **`C-t`** is bound to `forward-char`, overriding Emacs' normal `transpose-chars`. This is separate from `C-c t` (Python pytest).
- **`C-, ,` and `C-, C-,`** both call `ranger`. The Ranger declaration was disabled and has been removed from the refactored `site-pkgs.org`; these two keys are now unresolved. Rebind to `project-dired`, `dired-jump` or another active file/project command.
- **F4 family:** plain F4 calls `vimish-fold-toggle`; Ctrl-F4 calls `vimish-fold-refold-all-from-marks`; Ctrl-Shift-F4 calls `vimish-fold-unfold-all`; Meta-Shift-F4 calls `vimish-fold`. Those commands are no longer provided by the active configuration. Shift-F4 (`hydra-fold/body`) remains valid because the Hydra was rewritten to use Outline/Hideshow/Tree-sitter folding. Rebind the other F4 variants to that Hydra or to the new folding commands.
- **Numeric keypad:** `kp-enter` and `kp-insert` both call `yank`. This is a redundant alias rather than a functional conflict; retain it if the duplication is intentional. The modified keypad rectangle commands are otherwise internally consistent.
- **F12** calls `h7/make-note-frame`. Its definition was not in the supplied `site-pkgs.org` or keymap scan; verify that an active earlier-loaded file defines it.

The current public source does **not** bind Ctrl-F4 to a Helm command: it binds `vimish-fold-refold-all-from-marks`. The actual key assignments are visible in the repository's [`site-keys.el`](https://github.com/hute37/emacs-site/blob/master/site-keys.el), notably the C-comma map, F4 group, C-f/C-t assignments and keypad bindings.

## Validation and manual rollout

Static validation of the generated Org file:

- 142 Emacs Lisp source blocks; every `#+begin_src` has a matching `#+END_SRC`.
- Parentheses balance in every Emacs Lisp source block.
- No active `:disabled` use-package declarations.
- No commented-out `use-package` declarations in the active file.
- No duplicate exact package declarations, except three separate `use-package emacs` blocks that configure different built-in settings.

I could not run `org-babel-tangle`, `byte-compile-file` or `emacs --debug-init` here because an Emacs executable is not available in the build environment. Treat this as a statically checked candidate, not a runtime-tested replacement.

Recommended manual evaluation order:

1. Tangle to a temporary file and compare it with the current `site-pkgs.el` before replacing anything.
2. Test startup with `emacs --debug-init` after applying only the startup/helper changes.
3. Test ordinary `python-mode` and `python-ts-mode` files, plus percent-format notebooks in a `notebooks` directory. Check `lsp-mode` vs `eglot-managed-mode`, Flycheck vs Flymake, Ruff format-on-save and interpreter selection.
4. Test Julia Eglot and R ESS.
5. Apply the `site-keys.el` fixes separately after reviewing the listed broken key bindings.
