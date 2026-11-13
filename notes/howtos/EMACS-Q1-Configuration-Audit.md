---
title: TODO:(title)
subtitle: TODO:(subtitle)
# {{{ // %+

category: LLM-Style
keywords: [GEN, TODO:(keywords)]
abstract: |
  TODO:(abstract)

  ...

doctype: md-report

# }}} // %+
---
<!-- {{{ #TAG: TODO:(toc) // -->

<!-- markdownlint-disable MD012 -->
<!-- markdownlint-disable MD013 -->
<!-- markdownlint-disable MD025 -->
<!-- markdownlint-disable MD033 -->
<!-- markdownlint-disable MD051 -->
<!-- markdownlint-disable MD053 -->


# TOC

1. [Q:1 - Emacs configuration audit: Modernisation Report](#q1)
   - see: [TODO:(a1-ref-claude) (Claude)](#a1-claude)
   - see: [Modernisation Audit and Guidance for site-pkgs.org (Gemini)](#a1-gemini)
   - see: [Emacs configuration audit: `site-pkgs.org` (ChatGPT)](#a1-chatgpt)
   - see: [Emacs configuration audit: `site-pkgs.org` (Perplexity)](#a1-perplexity)
   - see: [Emacs Configuration Audit: `site-pkgs.org` (DeepSeek)](#a1-deepseek)
   - see: [*Audit of `site-pkgs.org`: Modernisation Report (Kimi)](#a1-kimi)
   - see: [TODO:(a1-ref-vibe) (Vibe)](#a1-vibe)
2. [Q:2 - TODO:(q2-ref)](#q2)
   - see: [TODO:(a2-ref-claude) (Claude)](#a2-claude)
   - see: [TODO:(a2-ref-gemini) (Gemini)](#a2-gemini)
   - see: [TODO:(a2-ref-chatgpt) (ChatGPT)](#a2-chatgpt)
   - see: [TODO:(a2-ref-perplexity) (Perplexity)](#a2-perplexity)
   - see: [TODO:(a2-ref-deepseek) (DeepSeek)](#a2-deepseek)
   - see: [TODO:(a2-ref-kimi) (Kimi)](#a2-kimi)
   - see: [TODO:(a2-ref-vibe) (Vibe)](#a2-vibe)
3. [A:a - TODO:(appendix-a)](#aa)


<details>
<summary></summary>

```{=latex}
\begin{comment}
```

</details>

---

|                   |                        |
|-------------------|------------------------|
| [<<<<](README.md) | [PDF](TODO:(file).pdf) |

---

<details>
<summary>[index]</summary>

[[_TOC_]]

</details>
<details>
<summary></summary>

```{=latex}
\end{comment}
```

</details>

<!-- ::}}} \\ %0. -->
<!-- ::{{{ #TAG: TODO:(q1-section) // -->

# Q:1

## Q:1 - [**Emacs configuration audit: Modernisation Report**]

[^](#toc)


## Role

You are a senior Emacs maintainer and configuration auditor. You specialise in modernising long-lived, `use-package`-based configurations written in org-mode (org-babel tangled to Emacs Lisp), with MELPA/ELPA package sources.

Your answers must be accurate, evidence-based and conservative. Never invent packages, variables, functions or file contents.

## Context

### Emacs and operating systems

- Distribution: the "alexmurray" `emacs-snap`, channel `edge`, classic confinement.
- `emacs-version`: `32.0.50` (development build). Judge "built-in" status against this version.
- Operating systems: Ubuntu (24.04 and 26.04) on X11 with XFCE; Fedora 44 and Arch-based (CachyOS, EndeavourOS) on Wayland with GNOME; Windows WSL 2 (systemd enabled, X11/Wayland support); Ubuntu-based Podman containers with an X11 socket mapped.
- Emacs is started from a terminal in a shell with an already activated virtual environment.

### Use cases

- Development in Python, R and Julia for NLP/NLU projects.
- Documentation in org-mode, Markdown and LaTeX, with `pandoc` PDF production.
- DevOps in Bash, Ansible and Terraform on Microsoft Azure.
- Terminals and remote work: `vterm`, `ghostel`, `eshell` and `tramp`.
- CLI-based AI-agent-assisted projects (tools not yet determined, in evaluation phase).

### Toolchain on `PATH`

- Python: `uv`-managed environments providing `basedpyright`, `ruff` and `pytest`.
- Node.js: `npm`-managed environment providing `markdownlint-cli2`.
- R: `renv`, only inside Podman containers.
- Julia: `juliaup` with several environments; `JuliaWorkspaces` and `LanguageServer` are available in all of them.
- Shells: `bash` (with `starship`) and `zsh` (with oh-my-zsh).
- Search and file utilities: `rg` (ripgrep), `ag`, `fd`, `eza`.

### Configuration layout

- `~/.emacs.el`: entry point.
- `~/.emacs-start.el`: symbolic link to `~/.emacs-site/site-start.el`, which invokes `~/.emacs-site/site-boot.el`.
- `~/.emacs-local.el`: unversioned local overrides.
- `~/.emacs-site/`: Git-versioned directory (public repository: https://github.com/hute37/emacs-site).
- `site-pkgs.org` is the source of `site-pkgs.el`. It is the main configuration and is attached to this request.
- `site-boot.el` loads, in order: `site-pref`, `site-conf`, local overrides, `site-pkgs`, `site-func`, `site-cust`, `site-misc`, `site-edit`, `site-view`, `site-keys`, `site-tool`, `site-user`.
- Present but not loaded by `site-boot.el`: `site-attic.org`, `site-config.org`, `site-custom.el`, `site-init.el`, `site-load.el` (legacy branch for Emacs older than 24.3) and `site-local.el`. Report whether they appear dead.

### Provided material and its limits

- Authoritative input: the attached `site-pkgs.org`.
- Not provided: all other `site-*.el` files. For any finding that depends on them (global keybindings, hooks, `site-misc`, `site-keys`), state "not verifiable from the provided files". The output of the `rg` command: `rg -n "global-set-key|keymap-set|define-key|bind-key" ~/.emacs-site/*.el` is provided as a separated attachment: `emacs-keymap-scan.txt`.
- Use the GitHub repository only if you can browse. Note that it may differ from the attached file.

## Objective

The configuration originates from the 1990s on UNIX systems, and many parts are deprecated, commented out or redundant. Audit `site-pkgs.org` and produce actionable modernisation guidance.

### Operational definitions

- _Obsolete_ means one of the following:
  - The package is archived, unmaintained or no longer on its archive.
  - A built-in feature in Emacs 32.0.50 supersedes it (for example `use-package`, `eglot`, `project.el`, tree-sitter modes, `which-key`, `use-short-answers`).
  - A modern replacement is clearly preferred (for example Helm, Ivy/Counsel/Swiper, Company, Projectile and `ido` are candidates for replacement by vertico, consult, corfu and `project.el`).
  - The code uses a function or variable that is deprecated or obsolete.
- _Unused_ means any of the following:
  - The block is commented out, `:disabled`, or marked `:tangle no`.
  - The package is installed but never configured, bound or hooked.
  - It is dead code (for example a branch for old Emacs versions).
- _Duplicate or conflicting_ means two or more active features that provide the same function. Examples include multiple completion frameworks, the same key bound twice, overlapping linters or formatters, flymake versus flycheck, or multiple `completion-at-point` sources fighting each other.

### Target design

- Preferred completion stack: vertico, orderless, marginalia, consult, embark (and `embark-consult`) for the minibuffer, and corfu with cape for in-buffer completion. Recommend additions or removals only with reasons.
- Both LSP clients must remain.
  - `lsp-mode` is assumed for ordinary Python code. If the file shows a different LSP client, report that.
  - `eglot` handles percent-format Jupyter notebooks (Python files with `# %%` cell markers).
  - Both clients operate on `.py` files in the same major mode, so propose a deterministic per-buffer mechanism that guarantees exactly one client per buffer. Consider the `# %%` marker, a directory-local variable and a hook. Notebooks are always stored in `notebooks` subdirectories of base project folder. Also check how diagnostics (flymake versus flycheck), `completion-at-point`, formatting (`ruff`), eldoc, xref and key prefixes interact in each case.
  - Report which client, if any, is configured for R and Julia.
- `eshell` and `tramp` are out of scope for this pass. List any related findings under "Deferred" without analysing them.

## Task

Work in this order, and do not skip a step:

1. Build a complete inventory of every package, `use-package` form and significant configuration block in `site-pkgs.org`, with its status (active, disabled, commented out, tangle-disabled).
2. Classify each item as obsolete, unused, duplicate/conflicting, or healthy.
3. Produce the three deliverables below, each finding backed by evidence from the file.
4. Before answering, verify that every finding cites an org heading or package name that exists in the attachment, and that no recommendation contradicts the LSP/eglot requirement.

### Deliverables

1. A commented list of obsolete features, each with the current best alternative.
2. A commented list of duplicated active features that are confusing (keymaps) or conflicting.
3. Concrete cleanup and enhancement changes that improve daily use. Prioritise by impact, and include startup-time and keybinding-ergonomics improvements where evidence supports them.

### Constraints

- Preserve the org-tangle workflow. Snippets must be valid Emacs Lisp for `use-package`, and each must state the target org heading and whether it replaces or adds code.
- Do not output the whole file. Each change must be independently applicable, ordered by priority, and accompanied by a short rollback hint.
- Guard platform-specific code (X11, Wayland, WSL, containers, terminal versus graphical frames).
- Keep the existing `h7/` naming convention for new helpers.
- Include the issues in `site-boot.el` in the audit if you find them.
- If you are not sure about a package fact or an Emacs 32 feature, mark it _needs verification_ with Low confidence. Do not guess.

### Finding record format

Use one nested heading per finding, followed by exactly these fields:

```markdown
#### OBS-01: Helm still loaded alongside vertico

- Location: org heading "Completion", `use-package helm`
- Status: obsolete and duplicate
- Evidence: `helm-mode` is enabled while `vertico-mode` is also active
- Recommendation: remove Helm; map `M-x` and `C-x b` to consult/vertico equivalents
- Risk: Low; Effort: Low; Confidence: High
```

Use the prefixes `OBS-` (obsolete), `DUP-` (duplicate/conflict), `ENH-` (enhancement) and `DEF-` (deferred).

## Output Format

Reply in GitLab Flavored Markdown (GLFM) that passes `markdownlint-cli2` with default rules:

- Use exactly one H1, then nested headings with no skipped levels. Prefer headings to bold text for structure.
- Use _underscores_ for emphasis.
- Use only dashes (`-`) for unordered lists, with correct indentation.
- Place code fences at the start of a line, always with a language tag (for example `elisp`, `bash`, `markdown`). Do not nest code blocks.
- Leave one blank line around headings, lists and code blocks.

Document structure:

1. A short executive summary and a summary table of all finding IDs with status, risk and priority.
2. Obsolete features.
3. Duplicated or conflicting features, including the LSP/eglot coexistence design.
4. Cleanup and enhancement changes.
5. Deferred items (`eshell`, `tramp`).
6. Additional Notes for important points the task did not cover.
7. Appendix: package inventory (one row per package: name, status, verdict).
8. Footnote definitions.

Reading rules: the attachment may contain rendering-only markup (`<details><summary>` blocks, `{=latex}` blocks, `[!tip]`/`[!note]` block quotes, `{{{`/`}}}` fold tags, `[^]` internal links). Ignore it when reading, and do not reproduce it. The only exception is footnote references.

References: cite online documentation as Markdown footnotes, linked from the relevant sentence. Prefix every footnote label with `rf-` (for example `[^rf-vertico]`). Use only official or well-known pages (GNU Emacs manuals, package repositories, MELPA) whose URLs you are confident about. Do not invent URLs.


<details>
<summary></summary>

```{=latex}
\newpage
```

</details>


# A:1 (Claude)

[^](#toc) **_TODO:(a1-ref-claude)_**

TODO:(a1-claude) ...

# A:1 (Gemini)

[^](#toc) [**_Modernisation Audit and Guidance for site-pkgs.org_**](https://share.gemini.google/9kvZ8zuMixwW)

## Executive Summary

An audit of `site-pkgs.org` against GNU Emacs 32.0.50 reveals a long-lived configuration that combines modern completion and navigation paradigms (`vertico`, `consult`, `corfu`, `embark`, `orderless`) with legacy scaffolding (`company`, `flycheck`, `etags`, unused elpa bootstrapping blocks). While the underlying Org-babel literate structure is robust, several duplicated completion stacks, conflicting linter backends (`flycheck` vs `flymake`), and syntax errors in Python buffer hooks require remediation.

This document delivers an inventory classification, structured finding records (`OBS-`, `DUP-`, `ENH-`, `DEF-`), a deterministic Python LSP/Eglot coexistence design for Jupyter notebooks versus standard scripts, actionable enhancement snippets with rollback instructions, deferred items, and a complete package inventory.

### Summary Table of Findings

| ID | Category | Status | Risk | Priority |
| --- | --- | --- | --- | --- |
| `OBS-01` | Obsolete | `company` and `company-posframe` active alongside `corfu` | Low | High |
| `OBS-02` | Obsolete | `flycheck` active globally while Emacs 32 / Eglot prefer `flymake` | Low | Medium |
| `OBS-03` | Obsolete | `etags` / `gtags` configured alongside built-in `project.el` / `xref` | Low | Low |
| `DUP-01` | Duplicate/Conflict | Dual in-buffer completion systems (`company` backends vs `corfu`/`cape`) | Medium | High |
| `DUP-02` | Duplicate/Conflict | Python LSP conflict (`lsp-mode` vs `eglot` multi-server specification) | Medium | High |
| `DUP-03` | Duplicate/Conflict | Unbalanced Python notebook hook syntax error (`notebooks` path match) | High | High |
| `ENH-01` | Enhancement | Clean up dead/unloaded legacy files (`site-load.el`, etc.) | Low | Low |
| `DEF-01` | Deferred | `eshell` and `tramp` configurations | None | Deferred |

---

## Obsolete Features

#### OBS-01: Company completion stack loaded alongside Corfu

* Location: org heading "Completion "at-point"", `use-package company`
* Status: obsolete and duplicate
* Evidence: `company` and `company-posframe` are enabled in `site-pkgs.org` while `corfu` and `cape` are fully configured as the primary in-buffer completion stack
* Recommendation: remove `company`, `company-posframe`, and company-specific backend packages (`company-ansible`, `company-terraform`, `company-auctex`); rely entirely on `corfu`, `cape`, and `completion-at-point-functions`
* Risk: Low; Effort: Low; Confidence: High

#### OBS-02: Flycheck enabled globally alongside built-in Flymake

* Location: org heading "Lang: LSP.mode", `use-package flycheck`
* Status: obsolete
* Evidence: `global-flycheck-mode` is turned on, whereas `eglot` relies natively on `flymake` and Emacs 32 provides robust built-in diagnostic infrastructure
* Recommendation: disable or remove `flycheck`; migrate any custom markdown/shell checkers to `flymake` or `lsp-mode` native diagnostic providers
* Risk: Low; Effort: Low; Confidence: High

#### OBS-03: Etags and Gtags minor modes configured

* Location: org heading "Project", `use-package etags` / `gtags`
* Status: obsolete
* Evidence: manual tagging tools are superseded by `project.el`, `ripgrep` (`rg`), and LSP/Eglot workspace symbol navigation in Emacs 32
* Recommendation: remove `etags` and `gtags` use-package blocks; use `project.el` and `consult` for symbol searching
* Risk: Low; Effort: Low; Confidence: High

---

## Duplicated or Conflicting Features

#### DUP-01: Dual In-Buffer Completion Frameworks

* Location: org heading "Completion "at-point""
* Status: duplicate and conflicting
* Evidence: both `corfu` (with `global-corfu-mode`) and `company` (with `company-posframe`) are loaded, causing overlapping UI popups and conflicting TAB/RET keybindings in programming modes.
* Recommendation: purge all `company` packages. Retain `corfu` as the sole in-buffer completion frontend, augmented by `cape` for supplementary completion-at-point backends (dabbrev, file, dictionary).
* Risk: Medium; Effort: Low; Confidence: High

#### DUP-02: Python LSP Client Coexistence (`lsp-mode` vs `eglot`)

* Location: org heading "Lang: Python/lsp"
* Status: conflicting if unmanaged
* Evidence: `lsp-mode` (`lsp-pyright`) is configured as the default for `.py` files, while `eglot` is intended for percent-format Jupyter notebooks (`# %%`) in `notebooks/` subdirectories. Without strict buffer-local isolation, both clients attempt to manage the same buffers, causing CPU contention, duplicate diagnostics, and conflicting edits.
* Recommendation: enforce strict mutual exclusivity. Ordinary Python files use `lsp-mode` with `basedpyright` and `ruff-format-on-save-mode`. Files inside `notebooks/` subdirectories automatically disconnect `lsp-mode` and invoke `eglot` with the multi-server setup (`basedpyright-langserver` + `ruff server`).
* Risk: Medium; Effort: Medium; Confidence: High

#### DUP-03: Syntax Error in Notebook Hook

* Location: org heading "Lang: Python/lsp", line `(add-hook 'python-ts-mode-hook ...)`
* Status: conflicting / broken code
* Evidence: the provided code for auto-activating Eglot on notebook files contains an unclosed `and` expression:
```elisp
(when (and buffer-file-name 
           (string-match-p "notebooks" (buffer-file-name))
  (remove-hook ...))

```


This triggers `Wrong type argument: integer-or-marker-p, nil` or Lisp syntax errors on buffer load.
* Recommendation: correct the parentheses matching for the `and` form.
* Risk: High; Effort: Low; Confidence: High

---

## Cleanup and Enhancement Changes

Here are the prioritized, concrete Emacs Lisp snippets to modernize `site-pkgs.org`. Each change is independently applicable, includes rollback instructions, and respects platform guards and naming conventions.

### 1. Fix Python Notebook Eglot Hook Syntax (Priority: High)

* Target Org Heading: `* Lang :: Python/lsp`
* Action: Replaces broken hook code.

```elisp
;; Replaces the malformed python-ts-mode hook for notebooks
(add-hook 'python-ts-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "/notebooks/" buffer-file-name))
              (remove-hook 'python-mode-hook #'lsp-deferred t)
              (remove-hook 'python-ts-mode-hook #'lsp-deferred t)
              (h7/python-use-eglot))))

```

* Rollback hint: Restore the original lambda block from version control if notebook activation fails.

### 2. Purge Obsolete `company` Stack (Priority: High)

* Target Org Heading: `* Completion "at-point"`
* Action: Replaces the `company` and `company-posframe` use-package blocks with clean `corfu` / `cape` integration.

```elisp
;; Replaces company blocks with pure corfu/cape setup
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  :hook (after-init . global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-complete)
        ("TAB"   . corfu-complete)
        ([tab]   . corfu-complete)
        ("RET"   . corfu-insert)
        ("C-n"   . corfu-next)
        ("C-p"   . corfu-previous)
        ("C-g"   . corfu-quit)))

```

* Rollback hint: Re-enable `company` and `company-posframe` blocks from git history.

### 3. Clean Unloaded Legacy Configuration Files (Priority: Low)

* Target Org Heading: `* Config: =site-pkgs=`
* Action: Add a note or verification check confirming that `site-attic.org`, `site-config.org`, `site-custom.el`, `site-init.el`, `site-load.el` (legacy pre-24.3 branch), and `site-local.el` are unreferenced by `site-boot.el` and can be archived safely.

```elisp
;; Verification note: site-attic.org, site-config.org, site-custom.el,
;; site-init.el, site-load.el, and site-local.el are dead/unloaded artifacts.

```

* Rollback hint: Move files back from an archive folder if any required snippets were accidentally omitted.

---

## Deferred Items (`eshell`, `tramp`)

In accordance with the task instructions, analysis of `eshell` and `tramp` configurations is deferred.

* Inventory of deferred blocks in `site-pkgs.org`:
* `eshell` core settings, custom prompts, eshell builtins (`eshell/f`, `eshell/gst`, `eshell/cat`, etc.), and `eshell-syntax-highlighting`.
* `tramp` remote connection handling and path resolution.


* Status: operational but excluded from this modernization pass.

---

## Additional Notes

* **Lexical Binding**: Ensure every tangled file header contains `-*- lexical-binding: t; -*-` as correctly present in `lic-head`.
* **Keybinding Ergonomics**: The configuration maintains the `h7/` prefix for user helpers and standardizes minibuffer actions around `vertico`, `consult`, and `embark`.
* **Startup Time**: Lazy-loading packages via `:hook`, `:commands`, and `:defer` keeps Emacs 32 startup snappy despite a comprehensive package set.

---

## Appendix: Package Inventory

| Package Name | Status | Verdict |
| --- | --- | --- |
| `bind-key` | Active | Healthy |
| `diminish` | Active | Healthy |
| `use-package-ensure-system-package` | Active | Healthy |
| `req-package` | Disabled | Unused / Remove |
| `general` | Active | Healthy |
| `zoom-window` | Active | Healthy |
| `ef-themes` | Active | Healthy |
| `minions` | Active | Healthy |
| `mood-line` | Active | Healthy |
| `delight` | Active | Healthy |
| `all-the-icons` | Active | Healthy |
| `solaire-mode` | Active | Healthy |
| `hl-todo` | Active | Healthy |
| `dash`, `f`, `s` | Active | Healthy |
| `hydra`, `use-package-hydra` | Active | Healthy |
| `ace-jump-mode` | Active | Healthy |
| `avy` | Active | Healthy |
| `regex-tool`, `visual-regexp` | Active | Healthy |
| `which-key` | Active | Healthy |
| `helpful` | Active | Healthy |
| `undo-tree` | Active | Healthy |
| `recentf` | Active | Healthy |
| `popper` | Active | Healthy |
| `comint` | Active | Healthy |
| `restart-emacs` | Active | Healthy |
| `autorevert` | Active | Healthy |
| `visible-mark` | Active | Healthy |
| `hilit-chg`, `goto-chg` | Active | Healthy |
| `outline-indent`, `treesit-fold`, `kirigami` | Active | Healthy |
| `calc` | Active | Healthy |
| `crux` | Active | Healthy |
| `server`, `websocket`, `simple-httpd` | Active | Healthy |
| `ligature` | Active | Healthy |
| `evil` | Active | Healthy |
| `cape`, `capf-autosuggest`, `esh-autosuggest` | Active | Healthy |
| `corfu` | Active | Healthy |
| `company`, `company-posframe`, `company-ansible`, `company-terraform`, `company-auctex` | Active | Obsolete (Migrate to Corfu) |
| `marginalia`, `all-the-icons-completion` | Active | Healthy |
| `vertico` | Active | Healthy |
| `consult`, `consult-dir`, `consult-todo` | Active | Healthy |
| `orderless` | Active | Healthy |
| `embark`, `embark-consult` | Active | Healthy |
| `savehist` | Active | Healthy |
| `olivetti` | Active | Healthy |
| `git-timemachine`, `vdiff` | Active | Healthy |
| `ls-lisp`, `dired`, `dired-x`, `find-dired`, `fd-dired`, `dired-preview`, `ztree` | Active | Healthy |
| `projectile`, `consult-projectile`, `treemacs`, `etags` | Active | Etags Obsolete |
| `ag`, `rg`, `deadgrep`, `grep` | Active | Healthy |
| `ghostel`, `vterm`, `eshell`, `eat` | Active | Healthy |
| `proced`, `sh-script` | Active | Healthy |
| `markdown-mode`, `markdown-ts-mode`, `pandoc-mode` | Active | Healthy |
| `css-mode`, `nxml-mode` | Active | Healthy |
| `lsp-mode`, `lsp-ui`, `consult-lsp`, `lsp-treemacs`, `dap-mode`, `treesit-auto` | Active | Healthy |
| `flycheck` | Active | Obsolete (Use Flymake/LSP) |
| `google-c-style`, `ess`, `polymode` | Active | Healthy |
| `python`, `uv-mode`, `pet`, `pyvenv-auto`, `with-venv`, `lsp-pyright`, `eglot`, `ruff-format`, `python-pytest`, `cov`, `gherkin-mode`, `zmq`, `jupyter`, `code-cells` | Active | Healthy |
| `julia-mode`, `eglot-jl`, `lsp-julia` | Active | Healthy |
| `maxima`, `ruby-mode`, `lua-mode`, `haskell-mode`, `plantuml-mode` | Active | Healthy |
| `json-mode`, `json-reformat`, `yaml-ts-mode`, `yaml-mode` | Active | Healthy |
| `verb`, `restclient`, `restclient-jq` | Active | Healthy |
| `ansible`, `ansible-doc`, `jinja2-mode`, `poly-ansible` | Active | Healthy |
| `company-terraform`, `docker`, `dockerfile-mode` | Active | Healthy |
| `google-this`, `elfeed-org`, `elfeed` | Active | Healthy |
| `auctex`, `auctex-latexmk`, `cdlatex`, `tex` | Active | Healthy |
| `flyspell`, `jinx`, `biblio`, `nov` | Active | Healthy |
| `pdf-tools`, `pdf-view-restore`, `pdfgrep`, `djvu` | Active | Healthy |
| `org`, `org-tree-slide`, `org-context`, `org-modern`, `ob-async`, `denote`, `denote-menu`, `denote-refs`, `citar`, `citar-embark`, `citar-denote`, `consult-notes` | Active | Healthy |
| `dashboard` | Active | Healthy |

---

## Footnote Definitions

[^ag-emacs]: GNU Emacs Development Manual, version 32.0.50. [https://www.gnu.org/software/emacs/](https://www.gnu.org/software/emacs/)
[^ag-vertico]: Vertico Minibuffer Completion, Minad. [https://github.com/minad/vertico](https://github.com/minad/vertico)
[^ag-consult]: Consult completing-read utilities, Minad. [https://github.com/minad/consult](https://github.com/minad/consult)
[^ag-corfu]: Corfu In-Buffer Completion, Minad. [https://github.com/minad/corfu](https://github.com/minad/corfu)
[^ag-cape]: Completion At Point Extensions, Minad. [https://github.com/minad/cape](https://github.com/minad/cape)
[^ag-eglot]: Eglot: The Emacs Client for Language Server Protocol. [https://github.com/joaotavora/eglot](https://github.com/joaotavora/eglot)
[^ag-flymake]: Emacs Flymake Error Checking. [https://www.gnu.org/software/emacs/manual/html_node/emacs/Flymake.html](https://www.gnu.org/software/emacs/manual/html_node/emacs/Flymake.html)
[^ag-treesit]: Tree-sitter major modes in Emacs. [https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser-based-Font-Lock.html](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser-based-Font-Lock.html)
[^ag-project]: Project Management in Emacs via project.el. [https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)

# A:1 (ChatGPT)

[^](#toc) [**_Emacs configuration audit: `site-pkgs.org`_**](https://chatgpt.com/share/6af74f29-2b5c-83ed-9906-8a9c601ba2db)


## **1. Executive summary**

The audit uses the attached `site-pkgs.org` as its authoritative source, with `emacs-keymap-scan.txt` as supplementary evidence for global keybindings. The target version is Emacs `32.0.50`, not the latest stable release.

The configuration is not simply an old configuration requiring wholesale replacement. It already contains a substantial modern foundation:

- A modern minibuffer stack: Vertico, Orderless, Marginalia, Consult and Embark.
- Corfu and Cape for in-buffer completion.
- Both `lsp-mode` and built-in Eglot.
- `project.el`, alongside Projectile and Consult integrations.
- Ruff formatting and basedpyright for Python.
- Tree-sitter integration.
- Modern Org facilities, including Denote and Citar.

The main problems are inconsistent activation policies, legacy configurations retained alongside modern ones, duplicated project and completion functionality, and some implementation errors in the Python LSP switching mechanism.

**The most important recommendation is to retain both LSP clients but make backend selection deterministic before either client starts.** The current notebook selection code does not reliably achieve that.

### **Audit priorities**

| Priority | Finding | Risk   | Main action                                          |
| -------- | ------- | ------ | ---------------------------------------------------- |
| P0       | ENH-01  | High   | Repair Python LSP selection                          |
| P0       | ENH-02  | High   | Make notebook detection reliable                     |
| P1       | DUP-01  | Medium | Resolve Python environment activation overlap        |
| P1       | DUP-02  | Medium | Consolidate completion frameworks                    |
| P1       | DUP-03  | Medium | Separate project.el from Projectile responsibilities |
| P1       | OBS-01  | Medium | Remove obsolete Python completion configurations     |
| P1       | ENH-03  | Medium | Clarify diagnostic ownership                         |
| P2       | DUP-04  | Low    | Consolidate model-line implementations               |
| P2       | ENH-04  | Low    | Reduce eager package loading                         |
| P2       | ENH-05  | Low    | Improve keybinding consistency                       |
| P2       | OBS-02  | Low    | Remove disabled legacy configurations                |
| P2       | ENH-06  | Low    | Simplify package bootstrap                           |
| P3       | OBS-03  | Low    | Review redundant navigation and folding packages     |

The identifiers below are the audit finding identifiers. The inventory in the appendix distinguishes package declarations from their actual activation status.

## **2. Obsolete features**

The following findings distinguish genuinely superseded configurations from merely older packages that can still be useful.

### **OBS-01: Legacy completion frontends**

- Location: org headings `Completion "at-point"`, `Company`, `Auto-Complete`, `IDO`
- Status: obsolete and partially disabled
- Evidence:
  - `company` remains declared, with `company-complete` bound to `C-c C-SPC`.
  - `company-posframe` enables its mode.
  - `auto-complete` is disabled.
  - `ido` and its extensions are disabled.
  - Corfu is enabled globally.
- Recommendation:
  - Retain Corfu as the sole in-buffer completion frontend.
  - Retain Cape and configure its additional CAPF backends.
  - Remove Company as an interactive completion frontend.
  - Retain Company-specific integration only where it is actually needed, or adapt its backends using `cape-company-to-capf`.
  - Remove Auto-Complete and the disabled IDO implementation.
- Risk: Low; Effort: Low; Confidence: High

The preferred Corfu/Cape combination is already present. It does not need replacing. The missing piece is configuring Cape, rather than introducing another completion framework.[^at-corfu]

### **OBS-02: Projectile as the primary project-management framework**

- Location: org heading `Workspace`, subsection `Project`
- Status: obsolete as the primary project framework; partially active
- Evidence:
  - `projectile` is configured.
  - `projectile-global-mode` is commented out.
  - `consult-projectile`, `treemacs-projectile` and Projectile-specific Hydra commands remain configured.
  - The built-in `project.el` is already used by Consult through `consult-project-buffer`.
  - Dashboard explicitly selects Projectile as its project backend.
- Recommendation:
  - Make `project.el` the canonical project abstraction.
  - Replace Projectile-specific commands with built-in project commands and Consult.
  - Retain Treemacs independently if its file-tree interface is useful.
  - Remove `consult-projectile` after migrating its bindings.
  - Avoid enabling Projectile globally merely to preserve legacy integrations.
- Risk: Medium; Effort: Medium; Confidence: High

`project.el` is built into the target Emacs distribution. It provides project discovery, project switching, file finding and project buffers without requiring Projectile.[^at-project]

### **OBS-03: External&#x20;**`which-key`**&#x20;dependency**

- Location: org heading `Utils`, subsection `Utils/Help`
- Status: obsolete dependency; healthy functionality
- Evidence: `which-key` is declared with `:ensure t` and `which-key-mode` is enabled.
- Recommendation: Keep `which-key-mode`, but use the built-in Emacs implementation and remove the unnecessary external installation requirement.
- Risk: Low; Effort: Low; Confidence: High

`which-key` has been included in GNU Emacs since Emacs 30.[^at-which-key]

### **OBS-04: Disabled legacy Python IDE**

- Location: org heading `Lang: Python`, subsection `Lang: Python/elpy`
- Status: obsolete and disabled
- Evidence:
  - `elpy` has `:disabled t`.
  - Its configuration still contains extensive Company, Flymake, navigation and refactoring keybindings.
  - Python is now configured around `lsp-pyright`, Eglot, Ruff and Corfu.
- Recommendation: Remove the Elpy declaration and its associated legacy keymap. Retain the existing Python LSP and notebook implementations.
- Risk: Low; Effort: Low; Confidence: High

### **OBS-05: Legacy Python notebook integration**

- Location: org heading `Lang: Python`, subsection `Lang: Python/ein`
- Status: partially obsolete and disabled
- Evidence:
  - `ein` is disabled.
  - `jupyter` and `code-cells` are active.
  - The notebook workflow is now based on percent-format Python files.
- Recommendation: Remove the disabled EIN configuration if no remaining workflow depends on it. Retain Jupyter and Code Cells.
- Risk: Low; Effort: Low; Confidence: High

### **OBS-06: Legacy navigation and completion infrastructure**

- Location: org headings `Utils/Jump`, `Utils/Search`, `Workspace`, and `Grep`
- Status: partially obsolete; partially duplicated
- Evidence:
  - `ace-jump-mode` and `avy` are both declared.
  - `helm-gtags` remains inside disabled `gtags` configuration.
  - `ag`, `rg`, `deadgrep`, built-in `grep` and Consult search commands coexist.
  - `etags` remains configured.
- Recommendation:
  - Prefer Avy over Ace Jump for character and line navigation.
  - Remove the disabled Helm Gtags implementation.
  - Standardise interactive project search on Consult/Ripgrep.
  - Keep built-in `grep` and Wgrep for workflows that need editable grep results.
  - Retain Etags only if you still use TAGS files independently of LSP.
- Risk: Low; Effort: Medium; Confidence: High

### **OBS-07: Disabled legacy folding implementations**

- Location: org heading `Utils/Misc`, subsection `folding`
- Status: partially obsolete and disabled
- Evidence:
  - `savefold`, `vimish-fold` and `kirigami` have active declarations.
  - `origami` and `folding` are commented out.
  - `outline-indent` and `treesit-fold` are also configured.
- Recommendation:
  - Retain `treesit-fold` for tree-sitter structural folding.
  - Retain `outline-indent` where indentation-based folding is required.
  - Evaluate whether `vimish-fold` is still needed for manual regions.
  - Remove unused legacy implementations rather than enabling more folding frameworks.
- Risk: Low; Effort: Medium; Confidence: Medium

### **OBS-08: Redundant external package bootstrap**

- Location: org heading `Starting up`, subsection `Use-Package`
- Status: obsolete bootstrap logic
- Evidence:
  - `use-package` is explicitly required.
  - The configuration contains numerous commented-out installation mechanisms, including Quelpa and Straight.
  - Emacs 32 already includes `use-package`.
- Recommendation:
  - Remove the historical bootstrap alternatives.
  - Retain one package manager: `package.el`.
  - Keep the MELPA and NonGNU ELPA archive declarations if required.
  - Do not enable `use-package-always-ensure` globally without reviewing the built-in packages and external dependencies.
- Risk: Low; Effort: Low; Confidence: High

The official `use-package` manual distinguishes package configuration from package installation and documents deferred loading.[^at-use-package]

### **OBS-09: Disabled historical Org integrations**

- Location: org heading `Org`, subsections `Org-mode`, `Org extras`, `Org ref`, and `Org roam`
- Status: disabled or commented out
- Evidence:
  - `org-ref`, `org-roam`, `org-brain`, `org-projectile`, `org-present` and several export backends are disabled.
  - Denote, Citar, Consult Notes and Org Modern are active.
- Recommendation:
  - Keep the active Denote/Citar workflow.
  - Remove disabled Org Roam and Org Ref configurations if no other loaded file calls their commands.
  - Remove duplicate export declarations after verifying which `ox-pandoc` configuration is intended.
- Risk: Low; Effort: Medium; Confidence: High

## **3. Duplicated or conflicting features**

### **DUP-01: Three Python environment activation mechanisms**

- Location: org heading `Lang: Python`, subsection `Lang: Python/env`
- Status: duplicate and potentially conflicting
- Evidence:
  - `uv-mode` activates the project virtual environment.
  - `pet` resolves the project interpreter and configures `lsp-pyright`.
  - `pyvenv-auto` independently activates virtual environments.
  - `with-venv` is also declared.
  - The comments explicitly describe `uv-mode` and PET as the intended architecture.
- Recommendation:
  - Retain `uv-mode` for environment activation.
  - Retain PET for interpreter discovery and basedpyright configuration.
  - Remove automatic activation by `pyvenv-auto` unless testing demonstrates a necessary independent use case.
  - Keep `with-venv` only if DAP or another tool demonstrably requires it.
- Risk: Medium; Effort: Low; Confidence: High

The important distinction is that environment activation and interpreter discovery are different responsibilities. PET and uv-mode can complement each other; a second automatic activation mechanism introduces unnecessary ambiguity.

### **DUP-02: Multiple search frontends and conflicting keybindings**

- Location: org headings `Completion "prompt"`, `Workspace/Grep`, and `Workspace/Project`
- Status: duplicate and conflicting
- Evidence:
  - Consult provides `consult-find`, `consult-grep`, `consult-git-grep` and `consult-ripgrep`.
  - The Grep section also binds `M-s d` to `find-grep-dired` and `M-s G` to `grep`.
  - Consult binds those same sequences to `consult-find` and `consult-git-grep`, respectively.
  - `ag`, `rg` and `deadgrep` are independently configured.
- Recommendation:
  - Use Consult as the primary interactive search interface.
  - Keep `rg` as the underlying external executable.
  - Keep Wgrep for editable results.
  - Remove conflicting `M-s` bindings from the legacy Grep declaration.
  - Retain `ag` or Deadgrep only if a specific workflow requires them.
- Risk: Medium; Effort: Low; Confidence: High

The conflict is particularly important because package declaration order and loading behaviour can determine which global binding wins.

### **DUP-03: Multiple mode-line implementations**

- Location: org heading `General/UI`
- Status: duplicate presentation functionality
- Evidence:
  - `mood-line-mode` is enabled.
  - `minions-mode` is enabled.
  - `delight` is loaded.
  - `minions-mode` is also redundantly enabled inside `hl-todo` configuration.
- Recommendation:
  - Retain Mood Line as the mode-line implementation.
  - Retain Minions if you want a compact minor-mode indicator.
  - Retain Delight for selective minor-mode suppression.
  - Remove the redundant `minions-mode` call from `hl-todo`.
- Risk: Low; Effort: Low; Confidence: High

### **DUP-04: Multiple grep and project navigation integrations**

- Location: org headings `Workspace/Project`, `Workspace/Grep`, and `Jump/Hydra`
- Status: duplicate
- Evidence:
  - Projectile, Consult-Projectile and Treemacs-Projectile coexist.
  - Built-in `project.el` is used by Consult.
  - The project Hydra contains both Projectile and project.el commands.
  - Dashboard uses Projectile as its backend.
- Recommendation: Consolidate project discovery and navigation around `project.el` and Consult. Retain Treemacs as a visual file-tree component, not as a second project-management authority.
- Risk: Medium; Effort: Medium; Confidence: High

### **DUP-05: Python diagnostics and completion ownership**

- Location: org headings `Lang: LSP.mode`, `Lang: Python/lsp`, `Lang: Python/tools`, and `Completion "at-point"`
- Status: partially conflicting
- Evidence:
  - Flycheck is enabled globally.
  - `lsp-mode` explicitly selects Flycheck.
  - Eglot uses Flymake.
  - Corfu is enabled globally.
  - Company remains configured with a manual completion binding.
  - Ruff formatting is enabled through `ruff-format`.
  - Ruff is also configured as an Eglot LSP server.
- Recommendation:
  - Use Flycheck for `lsp-mode` buffers.
  - Use Flymake for Eglot buffers.
  - Explicitly disable Flycheck in Eglot-managed Python buffers.
  - Keep Corfu as the completion frontend for both clients.
  - Use the LSP-provided CAPF as the primary completion source.
  - Keep Ruff formatting for ordinary Python buffers, not notebook buffers.
  - Do not add `flymake-ruff` while Ruff LSP diagnostics are already enabled.
- Risk: Medium; Effort: Medium; Confidence: High

This separation is consistent with Eglot's use of Flymake and the standard Emacs CAPF, Xref and ElDoc interfaces.[^at-eglot-features]

### **DUP-06: ESS keymap assignments overwrite earlier commands**

- Location: org heading `Lang: R`, subsection `Lang: R/ess`
- Status: duplicate keybindings
- Evidence:
  - `M-c M-j` is assigned first to `ess-eval-line` and subsequently to `ess-eval-line-and-go`.
  - `M-c M-r` is assigned to `ess-eval-region-and-go` and subsequently to `ess-use-this-dir`.
  - `M-c M-e` is assigned to `ess-goto-end-of-function-or-para` and subsequently to `ess-extra-map`.
- Recommendation: Give each command a unique binding. Prefer a consistent evaluation/navigation hierarchy and document the resulting map.
- Risk: Low; Effort: Low; Confidence: High

### **DUP-07: Folding frameworks overlap**

- Location: org heading `Utils/Misc`, subsections `folding` and `kirigami`
- Status: potentially duplicate
- Evidence:
  - `kirigami` configures indentation-based folding and `outline-minor-mode`.
  - `outline-indent` is also installed.
  - `treesit-fold` provides structural folding for tree-sitter modes.
  - `hs-minor-mode` is enabled for numerous language modes.
- Recommendation: Define explicit responsibilities:
  - `treesit-fold` for tree-sitter structural folding.
  - `outline-indent` for indentation-based folding.
  - `hs-minor-mode` for traditional syntax-based folding where required.
  - Remove `kirigami` if its functionality is already covered by these mechanisms.
- Risk: Low; Effort: Medium; Confidence: Medium

### **DUP-08: Disabled commands remain in the active buffer Hydra**

- Location: org heading `Jump`, subsection `Hydra`
- Status: stale references to disabled packages
- Evidence:
  - `ido-switch-buffer` is bound to `b` in `hydra-buffers`.
  - `helm-buffers-list` is bound to `s`.
  - The IDO configuration is disabled.
  - There is no active Helm configuration in the supplied file.
- Recommendation: Replace these two bindings with `consult-buffer` and `ibuffer`, respectively, or remove the redundant binding.
- Risk: Medium; Effort: Low; Confidence: High

The current Hydra can therefore expose commands whose implementing packages are not configured.

### **DUP-09: LSP client coexistence needs a single ownership policy**

- Location: org headings `Lang: LSP`, `Lang: Python/mode`, and `Lang: Python/lsp`
- Status: conflicting activation mechanisms
- Evidence:
  - `lsp-mode` starts automatically from both `python-mode-hook` and `python-ts-mode-hook`.
  - `python` sets `h7/python-use-eglot` to nil.
  - The notebook hook separately attempts to start Eglot.
  - `h7/python-lsp-switch` attempts to stop one client before starting the other.
  - The notebook hook attempts to remove `lsp-deferred` locally, but the configured LSP hooks are lambdas, not direct `lsp-deferred` entries.
  - The notebook hook is only attached to `python-ts-mode-hook`.
  - `h7/python-lsp-switch` checks `derived-mode-p 'python-mode`, which does not reliably include `python-ts-mode`; the common parent is `python-base-mode`.
  - The notebook hook's `when` condition is missing a closing parenthesis around its `and` expression. Its intended cleanup and switching calls are consequently parsed as additional condition forms rather than the body of `when`.
- Recommendation:
  - Remove automatic Python LSP startup from the `lsp-mode` hooks.
  - Use one dispatcher after file-local and directory-local variables have been processed.
  - Make the dispatcher the sole owner of Python LSP activation.
  - Use `python-base-mode` to identify both Python major modes.
  - Select Eglot for notebook buffers and `lsp-mode` for ordinary Python buffers.
  - Retain explicit manual switching commands as an override.
- Risk: High; Effort: Medium; Confidence: High

#### **Recommended ownership model**

This is the intended architecture, not a claim that the current configuration already implements it.

```
                  Python file opened
                         |
                         v
              Process directory locals
                         |
                         v
             Inspect notebook selection
                         |
             +-----------+-----------+
             |                       |
             v                       v
     Ordinary Python          Notebook Python
             |                       |
             v                       v
         lsp-mode                  Eglot
             |                       |
             v                       v
          Flycheck                Flymake
             |                       |
             +-----------+-----------+
                         |
                         v
                 Shared interfaces
              Corfu / CAPF / ElDoc
                    Xref
```

| Responsibility       | Ordinary Python      | Notebook Python                        |
| -------------------- | -------------------- | -------------------------------------- |
| LSP client           | `lsp-mode`           | Eglot                                  |
| Language server      | basedpyright         | Explicitly verify Eglot server command |
| Ruff diagnostics     | LSP integration      | Eglot server integration               |
| Diagnostics frontend | Flycheck             | Flymake                                |
| Completion frontend  | Corfu                | Corfu                                  |
| Completion source    | LSP CAPF             | Eglot CAPF                             |
| Formatting           | Ruff on save         | Explicit/manual formatting             |
| Documentation        | ElDoc / LSP UI       | ElDoc                                  |
| Navigation           | Xref                 | Xref                                   |
| Project navigation   | Consult / project.el | Consult / project.el                   |

This division is technically coherent because both clients use the standard Emacs interfaces for completion, documentation and navigation. Eglot's diagnostic backend is Flymake; `lsp-mode` supports Flycheck.[^at-lsp-mode]

**Important:** The actual Eglot server configuration still needs independent verification. Its comment describes `basedpyright-langserver` and Ruff, but the executable configuration currently specifies:

```
(python-ts-mode . ("rass"
                   "--" "ty" "server"
                   "--" "ruff" "server"))
```

That is not the server command described in the surrounding comments. I would not replace it blindly without confirming the intended `rass` executable and its multi-server protocol.

There is a second mismatch: `lsp-pyright` is configured to execute `basedpyright`, but `h7/lsp-setup` calls `lsp-install-server` for `pyright`. These should be reconciled.

### **Language support outside Python**

| Language | Configuration found              | Assessment                                                           |
| -------- | -------------------------------- | -------------------------------------------------------------------- |
| Python   | `lsp-mode`, `lsp-pyright`, Eglot | Both clients configured; activation needs repair                     |
| R        | ESS                              | No R LSP client configured                                           |
| Julia    | `eglot-jl`, `lsp-julia`          | Eglot is automatically started; LSP Julia has no active startup hook |

In particular, `lsp-julia` has no active `julia-mode` hook in the supplied configuration. Julia therefore uses Eglot by default, not both clients.

R uses ESS and its inferior process integration. There is no evidence of Eglot or `lsp-mode` configured for R.

## **4. Cleanup and enhancement changes**

The following changes are ordered by operational impact. Each is independently reviewable. Apply them in sequence, testing the configuration after each change.

### **ENH-01: Implement deterministic Python LSP dispatch**

- Location: org headings `Lang: Python/mode`, `Lang: Python/lsp`, and `Lang: Python/tools`
- Status: enhancement; replaces conflicting automatic activation
- Evidence: The current implementation starts LSP from major-mode hooks and attempts to switch clients later. The notebook hook also contains the malformed `when` condition described above.
- Recommendation: Select the backend once, after directory-local variables have been processed. Use the directory-local variable as an explicit override and the notebook path plus `# %%` markers as automatic detection.
- Risk: High; Effort: Medium; Confidence: High

**Change A — Add the dispatcher**

Target org heading: `Lang: Python/lsp`

Operation: Add the following code after the Eglot declaration. This introduces a single activation hook.

```
(defvar-local h7/python-lsp-backend nil
  "Python LSP backend: `lsp', `eglot', or nil for automatic selection.")

(put 'h7/python-lsp-backend 'safe-local-variable
     (lambda (value)
       (memq value '(lsp eglot))))

(defun h7/python-notebook-p ()
  "Return non-nil for percent-format Python notebook files."
  (and buffer-file-name
       (string-match-p
        "/notebooks/"
        (expand-file-name buffer-file-name))
       (save-excursion
         (goto-char (point-min))
         (re-search-forward
          "^# %%\\(?:[ \t].*\\)?$"
          (min (point-max) (+ (point-min) 65536))
          t))))

(defun h7/python-start-lsp ()
  "Start exactly one LSP client for the current Python buffer."
  (when (and buffer-file-name
             (derived-mode-p 'python-base-mode))
    (let ((backend
           (or h7/python-lsp-backend
               (if (h7/python-notebook-p)
                   'eglot
                 'lsp))))
      (pcase backend
        ('eglot
         (setq-local h7/python-use-eglot t)

         (when (bound-and-true-p lsp-mode)
           (lsp-disconnect)
           (lsp-mode -1))

         (when (bound-and-true-p flycheck-mode)
           (flycheck-mode -1))

         (when (fboundp #'ruff-format-on-save-mode)
           (ruff-format-on-save-mode -1))

         (eglot-ensure))

        ('lsp
         (when (and (fboundp #'eglot-managed-p)
                    (eglot-managed-p))
           (eglot-shutdown))

         (setq-local h7/python-use-eglot nil)

         (when (fboundp #'flycheck-mode)
           (flycheck-mode 1))

         (when (fboundp #'ruff-format-on-save-mode)
           (ruff-format-on-save-mode 1))

         (lsp-deferred))

        (_
         (user-error "Unknown Python LSP backend: %S" backend))))))

;; Run after file and directory-local variables have been processed.
(add-hook 'hack-local-variables-hook #'h7/python-start-lsp)
```

The use of `python-base-mode` is intentional: it covers both `python-mode` and `python-ts-mode`.

**Change B — Remove competing activation**

Target org heading: `Lang: LSP.mode`

Operation: Replace the Python-specific `lsp-mode` hooks with no automatic Python startup hooks.

Retain:

```
(lsp-mode . lsp-enable-which-key-integration)
```

Retain the `lsp-managed-mode-hook` tree-sitter workaround.

Remove the two Python-specific hook entries that invoke `lsp-deferred`.

Target org heading: `Lang: Python/mode`

Operation: Remove the two hooks that initialise `h7/python-use-eglot` to nil. The dispatcher now owns that decision.

Target org heading: `Lang: Python/tools`

Operation: Remove the automatic Python hooks from `ruff-format`. The dispatcher now enables formatting only for ordinary Python buffers.

**Change C — Provide an explicit directory-local override**

Target: `notebooks/.dir-locals.el` in each project.

Operation: Add this configuration where notebooks should always use Eglot.

```
((python-mode . ((h7/python-lsp-backend . eglot)))
 (python-ts-mode . ((h7/python-lsp-backend . eglot))))
```

The dispatcher gives this explicit setting precedence over automatic marker detection.

**Change D — Correct the manual switching guard**

Target org heading: `Lang: Python/lsp`

Operation: Replace the mode check in `h7/python-lsp-switch`.

```
(unless (derived-mode-p 'python-base-mode)
  (user-error "This command is intended for Python buffers"))
```

Also set `h7/python-lsp-backend` buffer-locally to the selected backend when a manual switch is requested.

Rollback: Restore the original three Python sections and remove the new directory-local files. Keep a backup of the existing implementation until both Python modes and notebook files have been tested.

### **ENH-02: Correct Python interpreter configuration**

- Location: org heading `Lang: Python`, subsection `Lang: Python/mode`
- Status: configuration error
- Evidence: An executable-selection `(cond ...)` expression appears inside `:custom`. This keyword is for variable customization, not arbitrary Lisp execution.
- Recommendation: Move interpreter selection to `:init`.
- Risk: Medium; Effort: Low; Confidence: High

Target: `Lang: Python/mode`

Operation: Replace the `python` declaration with the following corrected structure, retaining the existing hooks where needed.

```
(use-package python
  :ensure nil
  :init
  (cond
   ((executable-find "ipython")
    (setq python-shell-buffer-name "IPython"
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))

   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))

   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))

   (t
    (setq python-shell-interpreter "python")))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (treesit-font-lock-level 4))
```

Rollback: Restore the previous `python` declaration.

### **ENH-03: Reconcile the two Python language-server configurations**

- Location: org headings `Lang: LSP.setup` and `Lang: Python/lsp`
- Status: inconsistent server configuration
- Evidence:
  - `lsp-pyright` selects `basedpyright`.
  - `h7/lsp-setup` installs `pyright`.
  - Eglot uses `rass`, `ty` and Ruff in its actual command specification, despite comments describing basedpyright and Ruff.
  - Eglot's Python server registration currently covers `python-ts-mode`, not both Python major modes.
- Recommendation:
  - Decide whether Eglot notebooks use `ty` or basedpyright.
  - Verify the actual `rass` executable and its argument handling.
  - Register the validated Eglot server configuration for both `python-mode` and `python-ts-mode`.
  - Do not install a second type checker simply because its name appears in the comments.
  - Keep Ruff diagnostics and formatting responsibilities separate.
- Risk: High; Effort: Medium; Confidence: High for the mismatch, Low for the intended `rass` configuration

There is no safe replacement server command to provide from the supplied material alone.

Also review this global setting in the Eglot declaration:

```
(setq eglot-ignored-server-capabilities
      '(:documentLinkProvider))
```

It affects all Eglot clients, including Julia. Confirm that disabling document links globally is intentional.

Similarly, `eglot-connect-timeout` is configured to 300 in the Julia integration's `:init`; that is a global Eglot setting, not a Julia-only setting.

### **ENH-04: Consolidate in-buffer completion around Corfu and Cape**

- Location: org headings `Completion "at-point"`, `Corfu`, and `Company`
- Status: enhancement
- Evidence:
  - Corfu is globally enabled.
  - Cape is installed, but its example CAPF configuration is commented out.
  - Company is still configured with a manual completion binding.
  - Company-specific backends remain declared for AUCTeX, Ansible and Terraform.
- Recommendation:
  - Keep Corfu and Cape.
  - Remove `company-posframe` and the manual Company completion binding if Company is no longer used.
  - Convert useful Company backends using Cape's `cape-company-to-capf` adapter, or remove their integrations.
  - Add `cape-file` and `cape-dabbrev` selectively, not indiscriminately to every LSP buffer.
- Risk: Low; Effort: Medium; Confidence: High

Target org heading: `Completion "at-point"`

Operation: Add a small, targeted fallback configuration.

```
(use-package cape
  :ensure t
  :init
  (add-hook 'text-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'cape-dabbrev nil t))))
```

This gives ordinary text buffers an additional source without globally adding competing CAPF backends to LSP-managed Python buffers.

Rollback: Remove the additional hook.

### **ENH-05: Correct the completion and navigation keymaps**

- Location: org headings `Lang: LSP.mode`, `Workspace/Grep`, `Lang: R/ess`, and `Jump/Hydra`
- Status: enhancement
- Evidence:
  - `lsp-keymap-prefix` is explicitly set to `C-l`, which shadows the normal recenter command.
  - Consult and Grep both bind `M-s d` and `M-s G`.
  - ESS has three repeated assignments described in DUP-06.
  - The buffer Hydra references disabled IDO and Helm commands.
- Recommendation:
  - Give LSP its own prefix, preferably `C-c L`, avoiding the existing `C-c l` manual backend-switch bindings.
  - Keep `C-c c` for the Eglot commands.
  - Keep `M-.`, `M-,` and `M-?` for standard Xref navigation.
  - Remove the conflicting Grep bindings and use Consult.
  - Replace the Hydra's IDO and Helm commands with Consult and Ibuffer.
- Risk: Low; Effort: Low; Confidence: High

Target org heading: `Lang: LSP.mode`

Operation: Replace:

```
(setq lsp-keymap-prefix "C-l")
```

with:

```
(setq lsp-keymap-prefix "C-c L")
```

Target org heading: `Workspace/Grep`

Operation: Remove the two conflicting bindings from the `grep` declaration:

```
("M-s d" . find-grep-dired)
("M-s G" . grep)
```

Target org heading: `Jump/Hydra`

Operation: Replace:

```
("b" ido-switch-buffer)
("s" helm-buffers-list)
```

with:

```
("b" consult-buffer)
("s" ibuffer)
```

Target org heading: `Lang: R/ess`

Operation: Correct the three duplicate assignments individually. Do not simply delete the first or last occurrences without deciding which command should retain each sequence.

Rollback: Restore the previous bindings and prefix.

### **ENH-06: Repair malformed customization declarations**

- Location: org headings `General/UI` and `Workspace/Grep`
- Status: configuration correctness
- Evidence:
  - `hl-todo` groups two custom variables inside one `:custom` entry.
  - `ag` contains `ag-highligh-search`, which needs checking against the package's actual option name.
  - `wgrep-ag` configuration contains `twgrep-change-readonly-file`, which also needs checking.
- Recommendation:
  - Separate each `hl-todo` customization into its own declaration.
  - Verify the two suspicious variable names against the installed package versions before changing them.
- Risk: Medium; Effort: Low; Confidence: High for the `hl-todo` structure; Low for the other two names

Target org heading: `General/UI`

Operation: Replace the malformed `hl-todo` `:custom` block with:

```
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("HOLD"   . "#d0bf8f")
     ("TODO"   . "#cc9393")
     ("NEXT"   . "#dca3a3")
     ("THEM"   . "#dc8cc3")
     ("PROG"   . "#7cb8bb")
     ("OKAY"   . "#7cb8bb")
     ("DONT"   . "#5f7f5f")
     ("FAIL"   . "#8c5353")
     ("DONE"   . "#afd8af")
     ("NOTE"   . "#d0bf8f")
     ("MAYBE"  . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK"   . "#d0bf8f")
     ("TEMP"   . "#d0bf8f")
     ("FIXME"  . "#cc9393")
     ("XXXX*"  . "#cc9393")))
```

Rollback: Restore the previous `:custom` declaration.

### **ENH-07: Reduce unnecessary startup work**

- Location: org headings `Starting up/Use-Package` and `General/System`
- Status: startup optimisation
- Evidence:
  - `package-initialize` is called explicitly.
  - Package reinitialisation warnings are suppressed.
  - `use-package-verbose` is always enabled.
  - `gc-cons-threshold` is set to 100 MiB without restoration.
  - Numerous packages have no lazy-loading trigger or explicit defer policy.
- Recommendation:
  - Use Emacs 32's normal package activation rather than repeatedly initializing `package.el`.
  - Disable verbose package logging outside configuration development.
  - Restore the garbage-collection threshold after startup.
  - Defer genuinely optional packages using `:commands`, `:bind`, `:hook` or `:defer`.
  - Do not add `:defer t` to declarations that have no autoload trigger unless they are explicitly loaded later.
- Risk: Low; Effort: Low; Confidence: High

Target org heading: `Starting up/Use-Package`

Operation: Replace the explicit initialization and warning suppression with:

```
(setq use-package-verbose nil)

(require 'use-package)
```

This assumes the normal package startup mechanism has not been disabled elsewhere. Verify that MELPA and NonGNU packages are available before removing the explicit initialization.

Target org heading: `General/System`

Operation: Replace the permanent GC threshold assignment with:

```
(use-package emacs
  :init
  (setq gc-cons-threshold (* 100 1024 1024))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 16 1024 1024)))))
```

The 16 MiB value is a conservative starting point, not a mandatory setting. Benchmark it against your actual workload.

Rollback: Restore the previous GC assignment and package initialization.

### **ENH-08: Correct the tree-sitter remapping helper**

- Location: org heading `Lang: Treesitter.setup`
- Status: enhancement
- Evidence: `h7/treesitter-setup` assigns a new value to `major-mode-remap-alist`, containing only the TOML remapping. This can discard remappings established by other configuration.
- Recommendation: Add the TOML mapping without replacing the entire remapping list.
- Risk: Medium; Effort: Low; Confidence: High

Target org heading: `Lang: Treesitter.setup`

Operation: Replace the `setq major-mode-remap-alist` form with:

```
(add-to-list 'major-mode-remap-alist
             '(toml-mode . toml-ts-mode))
```

Rollback: Restore the previous assignment.

### **ENH-09: Simplify built-in package handling**

- Location: org headings `Starting up/Use-Package`, `Utils/Help`, and `Org/Org-mode`
- Status: enhancement; needs verification for Org
- Evidence:
  - Emacs 32 includes `use-package`, Eglot and Which Key.
  - `which-key` is still declared with `:ensure t`.
  - Org is declared with `:ensure t`.
  - Eglot correctly uses `:ensure nil`.
- Recommendation:
  - Remove unnecessary external installation requirements for built-in packages.
  - Keep Org's `:ensure t` only if intentionally tracking an external Org release instead of the bundled version.
  - Remove the `use-package-ensure-system-package` declaration if it is not used by another configuration file.
- Risk: Low; Effort: Low; Confidence: High for the built-ins; Medium for the intended Org version

Target org heading: `Utils/Help`

Operation: Change the `which-key` declaration to:

```
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 1))
```

Rollback: Restore the previous declaration.

## **5. Deferred items**

These areas are explicitly outside the scope of this audit.

### **DEF-01: Eshell and terminal integrations**

- Location: org heading `Shell`, subsections `vterm`, `eshell`, `Processes` and `Ranger`
- Status: deferred
- Evidence: Multiple terminal implementations and shell integrations are configured.
- Recommendation: Leave unchanged in this pass. Audit terminal lifecycle, shell environment propagation and process management separately.
- Risk: Not assessed; Effort: Not assessed; Confidence: High

Packages to revisit include:

- `ghostel` and its integrations.
- `vterm`, `multi-vterm` and `eat`.
- `eshell`, `eshell-toggle`, `eshell-vterm` and `xterm-color`.
- `proced` and `projector`.
- `ranger`.

### **DEF-02: TRAMP and container integrations**

- Location: org heading `VM/Docker`
- Status: deferred
- Evidence: `docker-tramp` and `tramp-container` appear in commented configuration.
- Recommendation: Do not change them in this pass.
- Risk: Not assessed; Effort: Not assessed; Confidence: High

### **DEF-03: Legacy files outside the boot sequence**

- Location: configuration layout; files `site-attic.org`, `site-config.org`, `site-custom.el`, `site-init.el`, `site-load.el` and `site-local.el`
- Status: apparently unused; requires verification
- Evidence: These files are reported as not loaded by `site-boot.el`.
- Recommendation: Verify whether any of them are loaded manually, referenced by local overrides or used by another startup mechanism before archiving or deleting them.
- Risk: Medium; Effort: Low; Confidence: Medium

`site-load.el` is particularly worth reviewing: the supplied keymap scan shows many commented historical bindings, while `site-keys.el` contains active versions of several of those bindings.

## **6. Additional notes**

### **6.1 Configuration and tangling**

The attached file contains:

| Item                                | Audit result |
| ----------------------------------- | ------------ |
| Org source blocks                   | 150          |
| `use-package` occurrences           | 368          |
| Distinct package symbols            | 316          |
| Explicit `:tangle no` directives    | 0            |
| Active declarations (syntactically) | 206          |
| Disabled declarations               | 63           |
| Commented declarations              | 99           |

The declaration counts include nested declarations, repeated package declarations and commented examples. They are not counts of installed packages or runtime-loaded libraries.

All source blocks inherit the file-wide `:tangle yes` setting. I found no tangle-disabled blocks.

The main configuration headings and significant blocks are:

| Org heading           | Significant configuration                                     |
| --------------------- | ------------------------------------------------------------- |
| Starting up           | Package archives and `use-package`                            |
| General               | Basic settings, autosave, GC, windows and UI                  |
| Utils                 | Libraries, navigation, search, help and miscellaneous tools   |
| Server                | Server and socket configuration                               |
| Fonts                 | Fontaine and ligatures                                        |
| Evil                  | Evil mode                                                     |
| Completion "at-point" | CAPF, Corfu, Company, Auto-Complete and IDO                   |
| Completion "prompt"   | Vertico, Marginalia, Consult, Orderless, Embark and Savehist  |
| Edit                  | Writer configuration                                          |
| Workspace             | Magit, Dired, Project and Grep                                |
| Shell                 | Ghostel, vterm and Eshell                                     |
| Text                  | Markdown and markup                                           |
| Lang                  | LSP, Tree-sitter, C/C++, R, Python, Julia and other languages |
| REST                  | JSON, YAML and HTTP clients                                   |
| OP                    | Ansible, Terraform and PowerShell                             |
| VM                    | Docker                                                        |
| Net                   | Web, news and Twitter                                         |
| AI                    | GPTel and OpenAI integrations                                 |
| TeX                   | LaTeX, spelling, bibliography and PDF                         |
| Org                   | Org-mode, Babel, Denote, Citar, Noter and Roam                |
| Jump                  | Dashboard, registers and Hydra                                |
| Setup                 | Manual first-time installation                                |

### **6.2 Additional configuration defects**

There are several issues worth correcting during the cleanup.

#### **ENH-10: Incorrect&#x20;**`:ensure`**&#x20;argument**

- Location: org heading `Starting up`, subsection `Use-Package`
- Status: configuration error
- Evidence: `use-package-ensure-system-package` is declared with `:ensure f`. The symbol `f` is not the Lisp constant `nil`. It is not a way of disabling installation.
- Recommendation: Remove the declaration because there are no uses of `:ensure-system-package` in the supplied file. If another configuration file requires it, use an appropriate installation declaration.
- Risk: Low; Effort: Low; Confidence: High

Target org heading: `Starting up/Use-Package`

Operation: Remove this declaration if no other loaded file uses it.

```
(use-package use-package-ensure-system-package
  :ensure f)
```

Rollback: Restore the declaration with the correct installation policy.

#### **ENH-11: Org package version policy**

- Location: org heading `Org`, subsection `Org-mode`
- Status: needs verification
- Evidence: Org is declared with `:ensure t`, even though Emacs includes Org.
- Recommendation: Decide explicitly whether the configuration should use bundled Org or an independently upgraded Org release. Use `:ensure nil` for the bundled implementation.
- Risk: Medium; Effort: Low; Confidence: Medium

This is especially important when maintaining a development Emacs distribution: mixing a bundled Org version with a separately installed version can complicate debugging.

#### **ENH-12: Eagerly enabled optional features**

- Location: org headings `General/UI`, `Utils`, `Workspace`, and `Net`
- Status: enhancement
- Evidence:
  - `mood-line-mode`, `minions-mode` and `solaire-global-mode` are enabled during configuration.
  - `magit` and several integrations are configured without an explicit loading policy.
  - `ef-themes` is declared but no theme activation is visible in the supplied file.
  - `regex-tool` is deferred without an explicit command or binding.
- Recommendation:
  - Make the loading policy explicit for optional packages.
  - Use `:commands` for interactive tools, `:hook` for mode integrations and `:bind` for command-driven packages.
  - Remove packages that have no actual use.
  - Check whether theme activation occurs in another configuration file.
- Risk: Low; Effort: Medium; Confidence: High

Do not apply `:defer t` indiscriminately. A deferred declaration without an autoload trigger can leave a package permanently unloaded.

### **6.3 External configuration dependencies**

Several identifiers referenced in `site-pkgs.org` are not defined in the supplied file:

- `h7/use-vertico`
- `h7/use-py-jupyter`
- `h7/org-mode-setup`
- `emacs-d`
- `ha-leader`

Their definitions and load order are not verifiable from the provided files.

This matters because `site-boot.el` loads `site-pkgs` before `site-func`, `site-cust` and the later modules.

Any helper evaluated immediately by `site-pkgs` must already be defined in `site-pref`, `site-conf` or the local overrides. A definition appearing only in `site-func` would be too late.

In particular, `h7/use-vertico` is called immediately in the Vertico configuration, so its definition must exist before `site-pkgs` is evaluated.

### **6.4 Global keymap audit limitations**

The supplied `emacs-keymap-scan.txt` provides useful evidence from:

- `site-keys.el`
- `site-pkgs.el`
- `site-load.el`

The active `site-keys.el` bindings include:

- `C-,` for `ctl-h7-map`.
- `C-SPC` for `completion-at-point`.
- `M-.`, `M-,` and related navigation conventions as documented in the configuration.
- Several global frame, buffer and folding shortcuts.

The `site-load.el` results are predominantly commented historical bindings.

I have not treated the scan as proof that all keymaps are conflict-free. It does not establish the complete runtime state of minor-mode maps, Hydra maps, package maps or bindings introduced by the other loaded configuration files.

Global keybinding conflicts outside the supplied scan are therefore *not verifiable from the provided files*.

### **6.5 Recommended validation sequence**

After applying the changes, validate the configuration in a clean Emacs process before making further changes.

```
emacs --debug-init
```

Useful interactive checks:

```
M-x describe-variable RET package-archives
M-x describe-variable RET major-mode-remap-alist
M-x describe-variable RET completion-at-point-functions
M-x describe-variable RET lsp-diagnostics-provider
M-x describe-variable RET eglot-server-programs
M-x describe-variable RET flycheck-checkers
M-x describe-variable RET gc-cons-threshold
```

For Python buffers, verify:

```
(list major-mode
      (derived-mode-p 'python-base-mode)
      h7/python-lsp-backend
      h7/python-use-eglot
      (bound-and-true-p lsp-mode)
      (and (fboundp 'eglot-managed-p)
           (eglot-managed-p))
      (bound-and-true-p flycheck-mode)
      (bound-and-true-p flymake-mode)
      (bound-and-true-p ruff-format-on-save-mode))
```

The important invariants are:

- Exactly one LSP client per Python buffer.
- Flycheck for ordinary Python buffers.
- Flymake for notebook buffers.
- No Ruff formatting-on-save for notebook buffers.
- Corfu remains the only interactive completion frontend.
- Xref and ElDoc work independently of the LSP client.

For Julia, verify that Eglot connects successfully and that `lsp-julia` does not start a second client.

For R, verify that ESS remains functional independently of the Python LSP changes.

## **7. Appendix: Package inventory**

The following inventory covers all distinct package symbols declared in the attachment, including commented and disabled declarations.

The status is the status of the declaration in the Org source, not the package manager's installed/available/built-in status.

- *Active*: an uncommented declaration without `:disabled`.
- *Disabled*: an uncommented declaration containing `:disabled`.
- *Commented*: the declaration is commented out.
- *Mixed*: the package has multiple declarations with different statuses.

A healthy verdict means no removal is recommended by this audit. It does not certify that the package is currently installed or working.

### **7.1 Active declarations**

| Package                           | Status | Verdict                                                 |
| --------------------------------- | ------ | ------------------------------------------------------- |
| ace-jump-mode                     | Active | Obsolete; prefer Avy                                    |
| ag                                | Active | Duplicate; consolidate with Consult                     |
| all-the-icons                     | Active | Healthy                                                 |
| all-the-icons-completion          | Active | Healthy                                                 |
| ansible                           | Active | Healthy                                                 |
| ansible-doc                       | Active | Healthy                                                 |
| auctex                            | Active | Healthy                                                 |
| auctex-latexmk                    | Active | Healthy                                                 |
| autorevert                        | Active | Healthy                                                 |
| avy                               | Active | Healthy                                                 |
| biblio                            | Active | Healthy                                                 |
| bind-key                          | Active | Healthy                                                 |
| calc                              | Active | Built-in                                                |
| cape                              | Active | Healthy; configure CAPF sources                         |
| capf-autosuggest                  | Active | Healthy; shell integration                              |
| cdlatex                           | Active | Healthy                                                 |
| citar                             | Active | Healthy                                                 |
| citar-denote                      | Active | Healthy                                                 |
| citar-embark                      | Active | Healthy                                                 |
| code-cells                        | Active | Healthy                                                 |
| comint                            | Active | Built-in                                                |
| company-ansible                   | Active | Review; Company integration                             |
| company-auctex                    | Active | Review; Company integration                             |
| company-posframe                  | Active | Duplicate                                               |
| company-terraform                 | Active | Review; Company integration                             |
| consult                           | Active | Healthy                                                 |
| consult-dir                       | Active | Healthy                                                 |
| consult-lsp                       | Active | Healthy                                                 |
| consult-notes                     | Active | Healthy                                                 |
| consult-projectile                | Active | Duplicate                                               |
| consult-todo                      | Active | Healthy                                                 |
| corfu                             | Active | Healthy                                                 |
| cov                               | Active | Healthy                                                 |
| crux                              | Active | Healthy                                                 |
| css-mode                          | Active | Built-in                                                |
| dap-mode                          | Active | Healthy                                                 |
| dash                              | Active | Unused candidate                                        |
| deadgrep                          | Active | Duplicate                                               |
| delight                           | Active | Healthy                                                 |
| denote                            | Active | Healthy                                                 |
| denote-menu                       | Active | Healthy                                                 |
| denote-refs                       | Active | Healthy                                                 |
| diminish                          | Active | Healthy                                                 |
| dired                             | Active | Built-in                                                |
| dired-preview                     | Active | Healthy                                                 |
| dired-x                           | Active | Built-in                                                |
| djvu                              | Active | Healthy                                                 |
| docker                            | Active | Healthy                                                 |
| dockerfile-mode                   | Active | Healthy                                                 |
| eat                               | Active | Deferred; terminal audit                                |
| ef-themes                         | Active | Verify theme activation                                 |
| eglot                             | Active | Built-in; retain                                        |
| eglot-jl                          | Active | Healthy                                                 |
| elfeed-org                        | Active | Review dependency                                       |
| emacs                             | Active | Built-in configuration namespace                        |
| engine-mode                       | Active | Healthy                                                 |
| esh-autosuggest                   | Active | Deferred; Eshell                                        |
| eshell                            | Active | Built-in; deferred                                      |
| eshell-syntax-highlighting        | Active | Deferred; Eshell                                        |
| eshell-toggle                     | Active | Deferred; Eshell                                        |
| eshell-vterm                      | Active | Deferred; Eshell                                        |
| ess                               | Active | Healthy                                                 |
| ess-R-data-view                   | Active | Healthy                                                 |
| ess-R-object-popup                | Active | Healthy                                                 |
| etags                             | Active | Built-in                                                |
| evil                              | Active | Healthy                                                 |
| f                                 | Active | Unused candidate                                        |
| fd-dired                          | Active | Healthy                                                 |
| find-dired                        | Active | Built-in                                                |
| flycheck                          | Active | Healthy; LSP diagnostics                                |
| flycheck-haskell                  | Active | Healthy                                                 |
| flyspell                          | Active | Built-in                                                |
| general                           | Active | Healthy                                                 |
| gherkin-mode                      | Active | Healthy                                                 |
| ghostel-comint                    | Active | Deferred; terminal                                      |
| ghostel-compile                   | Active | Deferred; terminal                                      |
| ghostel-eshell                    | Active | Deferred; Eshell                                        |
| google-c-style                    | Active | Healthy                                                 |
| google-this                       | Active | Healthy                                                 |
| goto-chg                          | Active | Healthy                                                 |
| gptel-ask                         | Active | Verify dependency on GPTel                              |
| grep                              | Active | Built-in; duplicate bindings                            |
| haskell-mode                      | Active | Healthy                                                 |
| helm-gtags                        | Active | Unreachable inside disabled Gtags configuration         |
| helpful                           | Active | Healthy                                                 |
| hilit-chg                         | Active | Healthy                                                 |
| hl-todo                           | Active | Correct malformed customization                         |
| hydra                             | Active | Healthy                                                 |
| ido-hacks                         | Active | Unreachable inside disabled IDO configuration           |
| inferior-python-mode              | Active | Built-in                                                |
| jinja2-mode                       | Active | Healthy                                                 |
| jinx                              | Active | Healthy                                                 |
| json-mode                         | Active | Healthy                                                 |
| json-reformat                     | Active | Healthy                                                 |
| jupyter                           | Active | Conditional; verify activation                          |
| kirigami                          | Active | Folding overlap                                         |
| ls-lisp                           | Active | Built-in                                                |
| lsp-mode                          | Active | Healthy; retain                                         |
| lsp-pyright                       | Active | Healthy; basedpyright adapter                           |
| lsp-treemacs                      | Active | Healthy                                                 |
| lsp-ui                            | Active | Healthy                                                 |
| magit                             | Active | Healthy                                                 |
| magit-backup                      | Active | Verify actual use                                       |
| markdown-mode                     | Active | Healthy                                                 |
| markdown-ts-mode                  | Active | Verify Emacs 32 mode availability                       |
| maxima                            | Active | Healthy                                                 |
| minions                           | Active | Healthy                                                 |
| mood-line                         | Active | Healthy                                                 |
| multi-vterm                       | Active | Deferred; terminal                                      |
| nov                               | Active | Healthy                                                 |
| ob-async                          | Active | Healthy                                                 |
| ob-dot                            | Active | Healthy                                                 |
| olivetti                          | Active | Healthy                                                 |
| org-context                       | Active | Verify actual use                                       |
| org-modern                        | Active | Healthy                                                 |
| org-noter                         | Active | Healthy                                                 |
| org-noter-pdftools                | Active | Healthy                                                 |
| org-pdftools                      | Active | Healthy                                                 |
| org-tree-slide                    | Active | Healthy                                                 |
| outline-indent                    | Active | Folding overlap                                         |
| pandoc-mode                       | Active | Healthy                                                 |
| pcmpl-args                        | Active | Deferred; Eshell                                        |
| pdf-view-restore                  | Active | Healthy                                                 |
| pdfgrep                           | Active | Healthy                                                 |
| pet                               | Active | Healthy; Python interpreter tracking                    |
| plantuml-mode                     | Active | Healthy                                                 |
| poly-ansible                      | Active | Verify overlap with Ansible modes                       |
| poly-markdown                     | Active | Healthy                                                 |
| polymode                          | Active | Healthy                                                 |
| popper                            | Active | Healthy                                                 |
| pos-tip                           | Active | Unreachable inside disabled Auto-Complete configuration |
| proced                            | Active | Built-in                                                |
| projectile                        | Active | Duplicate; prefer project.el                            |
| python                            | Active | Built-in                                                |
| python-pytest                     | Active | Healthy                                                 |
| pyvenv-auto                       | Active | Conflicts with uv-mode/PET policy                       |
| recentf                           | Active | Built-in                                                |
| regex-tool                        | Active | Review; deferred without visible trigger                |
| restart-emacs                     | Active | Healthy                                                 |
| restclient                        | Active | Healthy                                                 |
| restclient-jq                     | Active | Healthy                                                 |
| rg                                | Active | Healthy; preferred search backend                       |
| ruby-mode                         | Active | Built-in                                                |
| ruff-format                       | Active | Healthy; format-on-save                                 |
| s                                 | Active | Unused candidate                                        |
| savehist                          | Active | Built-in                                                |
| sbt-mode                          | Active | Healthy                                                 |
| server                            | Active | Built-in                                                |
| sh-script                         | Active | Built-in                                                |
| simple-httpd                      | Active | Verify actual use                                       |
| solaire-mode                      | Active | Healthy                                                 |
| tex                               | Active | Built-in                                                |
| treemacs                          | Active | Healthy                                                 |
| treemacs-all-the-icons            | Active | Healthy                                                 |
| treemacs-icons-dired              | Active | Healthy                                                 |
| treemacs-magit                    | Active | Healthy                                                 |
| treemacs-projectile               | Active | Duplicate integration                                   |
| treesit-auto                      | Active | Healthy; optional grammar automation                    |
| treesit-fold                      | Active | Folding overlap                                         |
| undo-tree                         | Active | Healthy; compare with native undo-redo                  |
| use-package-ensure-system-package | Active | Remove or correct `:ensure f`                           |
| use-package-hydra                 | Active | Healthy                                                 |
| uv-mode                           | Active | Healthy; Python environment activation                  |
| vdiff                             | Active | Healthy                                                 |
| verb                              | Active | Healthy                                                 |
| visible-mark                      | Active | Healthy                                                 |
| visual-regexp                     | Active | Healthy                                                 |
| vterm                             | Active | Deferred; terminal                                      |
| w3m-type-ahead                    | Active | Verify disabled W3M dependency                          |
| websocket                         | Active | Verify actual use                                       |
| wgrep                             | Active | Healthy                                                 |
| wgrep-ag                          | Active | Verify Ag dependency                                    |
| which-key                         | Active | Built-in; remove external ensure                        |
| with-venv                         | Active | Verify DAP dependency                                   |
| xterm-color                       | Active | Deferred; Eshell                                        |
| yaml-mode                         | Active | Healthy                                                 |
| yaml-ts-mode                      | Active | Built-in                                                |
| zoom-window                       | Active | Healthy                                                 |
| ztree                             | Active | Healthy                                                 |

### **7.2 Disabled declarations**

| Package                     | Status   | Verdict                           |
| --------------------------- | -------- | --------------------------------- |
| ack                         | Disabled | Obsolete candidate                |
| auto-complete               | Disabled | Obsolete; Corfu                   |
| chatgpt                     | Disabled | Legacy AI integration             |
| chatgpt-shell               | Disabled | Legacy AI integration             |
| citar-org-roam              | Disabled | Unused with disabled Org Roam     |
| codegpt                     | Disabled | Legacy AI integration             |
| dall-e                      | Disabled | Legacy AI integration             |
| doi-utils                   | Disabled | Legacy Org Ref integration        |
| elpy                        | Disabled | Obsolete Python IDE               |
| ess-R-data-smart-equals     | Disabled | Unused                            |
| ess-R-data-smart-underscore | Disabled | Unused                            |
| flx-ido                     | Disabled | Obsolete completion integration   |
| fontaine                    | Disabled | Optional; verify whether intended |
| git-gutter-fringe           | Disabled | Optional Git UI                   |
| gptel                       | Disabled | AI configuration inactive         |
| gtags                       | Disabled | Legacy tag navigation             |
| ido                         | Disabled | Obsolete completion framework     |
| ido-vertical-mode           | Disabled | Obsolete completion integration   |
| idris-mode                  | Disabled | Disabled language integration     |
| magit-review                | Disabled | Optional                          |
| magit-zzz                   | Disabled | Optional                          |
| ob-chatgpt-shell            | Disabled | Legacy AI integration             |
| openai                      | Disabled | Legacy AI integration             |
| org-brain                   | Disabled | Legacy Org workflow               |
| org-present                 | Disabled | Optional presentation             |
| org-projectile              | Disabled | Duplicate project integration     |
| org-ref                     | Disabled | Prefer Citar                      |
| org-ref-bibtex              | Disabled | Prefer Citar                      |
| org-roam                    | Disabled | Verify before removal             |
| org-roam-ui                 | Disabled | Depends on Org Roam               |
| org-static-blog             | Disabled | Optional publishing               |
| ox-ipynb                    | Disabled | Optional export                   |
| ox-reveal                   | Disabled | Optional presentation             |
| ox-rss                      | Disabled | Optional export                   |
| ox-tufte                    | Disabled | Optional export                   |
| paperless                   | Disabled | Optional                          |
| pocket-reader               | Disabled | Optional                          |
| poetry                      | Disabled | Replaced by uv/PET policy         |
| poly-R                      | Disabled | Verify Polymode dependency        |
| projector                   | Disabled | Optional process integration      |
| puppet-mode                 | Disabled | Optional language integration     |
| savefold                    | Disabled | Folding overlap                   |
| saveplace-pdf-view          | Disabled | Optional PDF integration          |
| scala-mode2                 | Disabled | Legacy Scala integration          |
| setup-reading               | Disabled | Optional reading integration      |
| sh-toggle                   | Disabled | Optional                          |
| vdiff-magit                 | Disabled | Optional integration              |
| vimish-fold                 | Disabled | Folding overlap                   |
| w3m                         | Disabled | Optional browser                  |
| yasnippet                   | Disabled | Optional snippets                 |

### **7.3 Commented declarations**

| Package               | Status    | Verdict                                      |
| --------------------- | --------- | -------------------------------------------- |
| ac-math               | Commented | Historical                                   |
| ack-and-a-half        | Commented | Historical                                   |
| add-node-modules-path | Commented | Historical                                   |
| ansible-vault         | Commented | Historical                                   |
| biblithek             | Commented | Historical                                   |
| bs                    | Commented | Historical                                   |
| cl-lib                | Commented | Built-in; unnecessary declaration            |
| company-lsp           | Commented | Legacy integration                           |
| company-restclient    | Commented | Historical                                   |
| docker-tramp          | Commented | Deferred                                     |
| doom-modeline         | Commented | Alternative model-line                       |
| dot-org               | Commented | Historical                                   |
| edit-server           | Commented | Optional                                     |
| esh-toggle            | Commented | Deferred                                     |
| ess-julia.el          | Commented | Historical Julia integration                 |
| flymake-ruff          | Commented | Do not enable alongside Ruff LSP diagnostics |
| flymake-shellcheck    | Commented | Optional                                     |
| folding               | Commented | Historical                                   |
| ghc                   | Commented | Historical Haskell integration               |
| guide-key             | Commented | Superseded by Which Key                      |
| julia-repl            | Commented | Alternative Julia REPL                       |
| jumpc                 | Commented | Historical                                   |
| latex-mode            | Commented | Built-in alternative                         |
| lua-block             | Commented | Historical                                   |
| memoize               | Commented | Historical                                   |
| multi-term            | Commented | Deferred                                     |
| ob-core               | Commented | Historical Org Babel                         |
| org-superstar         | Commented | Alternative Org styling                      |
| origami               | Commented | Alternative folding                          |
| ox-man                | Commented | Optional export                              |
| pass                  | Commented | Optional password integration                |
| peep-dired            | Commented | Optional Dired integration                   |
| powerline             | Commented | Alternative model-line                       |
| powershell            | Commented | Optional language integration                |
| pretty-lambdada       | Commented | Historical                                   |
| pretty-symbols        | Commented | Historical                                   |
| preview               | Commented | Historical LaTeX integration                 |
| puppet-ext            | Commented | Historical                                   |
| pyvenv                | Commented | Alternative Python environment integration   |
| quelpa                | Commented | Historical package manager                   |
| quelpa-use-package    | Commented | Historical package manager                   |
| rainbow-delimiters    | Commented | Optional                                     |
| rainbow-mode          | Commented | Optional                                     |
| sunrise-commander     | Commented | Alternative Dired                            |
| tex-site              | Commented | Historical                                   |
| toc-mode              | Commented | Optional                                     |
| tramp-container       | Commented | Deferred                                     |
| use-package-ensure    | Commented | Historical bootstrap                         |
| vterm-toggle          | Commented | Deferred                                     |
| vundo                 | Commented | Alternative undo interface                   |
| web-mode              | Commented | Optional markup integration                  |
| whitespace            | Commented | Built-in                                     |
| windmove              | Commented | Built-in                                     |
| yari                  | Commented | Historical Ruby documentation                |

### **7.4 Packages with mixed declarations**

These packages have more than one declaration, with different activation statuses.

| Package         | Declaration status            | Verdict                                  |
| --------------- | ----------------------------- | ---------------------------------------- |
| company         | Active + commented            | Duplicate completion frontend            |
| dashboard       | Active + commented            | Remove historical example                |
| ein             | Disabled + commented          | Disabled notebook integration            |
| elfeed          | Active + commented            | Remove obsolete examples                 |
| elfeed-goodies  | Disabled + commented          | Disabled integration                     |
| embark          | Active + commented            | Healthy                                  |
| embark-consult  | Active + commented            | Healthy                                  |
| ensime          | Active + disabled + commented | Verify obsolete Scala configuration      |
| ghostel         | Active + commented            | Deferred                                 |
| git-timemachine | Active + commented            | Healthy                                  |
| hide-mode-line  | Active + commented            | Healthy; check usage                     |
| julia-mode      | Active + commented            | Healthy                                  |
| ligature        | Active + commented            | Healthy                                  |
| lsp-julia       | Active + commented            | No active LSP startup hook               |
| lua-mode        | Active + commented            | Healthy                                  |
| marginalia      | Active + commented            | Healthy                                  |
| nxml-mode       | Active + commented            | Built-in                                 |
| ob-http         | Disabled + commented          | Disabled                                 |
| ob-restclient   | Disabled + commented          | Disabled                                 |
| orderless       | Active + commented            | Healthy                                  |
| org             | Active + commented            | Built-in; verify external version policy |
| org-autolist    | Disabled + commented          | Disabled                                 |
| org-roam-bibtex | Disabled + commented          | Depends on disabled Org Roam             |
| ox-beamer       | Active + commented            | Healthy                                  |
| ox-latex        | Active + commented            | Healthy                                  |
| ox-md           | Active + commented            | Healthy                                  |
| ox-pandoc       | Active + disabled             | Duplicate declarations                   |
| pdf-tools       | Active + commented            | Healthy                                  |
| ranger          | Disabled + commented          | Deferred                                 |
| req-package     | Disabled + commented          | Legacy bootstrap                         |
| twittering-mode | Disabled + commented          | Legacy integration                       |
| vertico         | Active + commented            | Healthy                                  |
| zmq             | Two active + two commented    | Conditional; consolidate declarations    |

### **7.5 Built-in Emacs features relevant to the inventory**

The following functionality should be treated as Emacs 32 functionality rather than additional external package dependencies:

| Feature        | Configuration role      |
| -------------- | ----------------------- |
| `use-package`  | Package configuration   |
| `eglot`        | LSP client              |
| `project.el`   | Project management      |
| `which-key`    | Keybinding discovery    |
| `treesit`      | Tree-sitter integration |
| `flymake`      | Diagnostics             |
| `xref`         | Symbol navigation       |
| `eldoc`        | Documentation           |
| `recentf`      | Recent files            |
| `savehist`     | Minibuffer history      |
| `dired`        | File management         |
| `comint`       | Interactive processes   |
| `sh-script`    | Shell scripts           |
| `python`       | Python editing          |
| `yaml-ts-mode` | YAML editing            |

This distinction matters when deciding whether to install an ELPA package or simply enable the built-in functionality.

## **8. Complete finding register**

This is the consolidated register, including findings introduced after the initial executive summary.

| ID     | Classification                          | Risk         | Priority |
| ------ | --------------------------------------- | ------------ | -------- |
| OBS-01 | Obsolete completion frontends           | Low          | P1       |
| OBS-02 | Projectile as primary project framework | Medium       | P1       |
| OBS-03 | External Which Key dependency           | Low          | P2       |
| OBS-04 | Elpy                                    | Low          | P2       |
| OBS-05 | EIN                                     | Low          | P2       |
| OBS-06 | Legacy navigation and search            | Low          | P2       |
| OBS-07 | Folding overlap                         | Low          | P3       |
| OBS-08 | Historical package bootstrap            | Low          | P2       |
| OBS-09 | Historical Org integrations             | Low          | P2       |
| DUP-01 | Python environment activation           | Medium       | P1       |
| DUP-02 | Search frontends and bindings           | Medium       | P1       |
| DUP-03 | Model-line implementations              | Low          | P2       |
| DUP-04 | Project navigation                      | Medium       | P1       |
| DUP-05 | Python diagnostics and completion       | Medium       | P1       |
| DUP-06 | ESS keymap overwrites                   | Low          | P2       |
| DUP-07 | Folding implementations                 | Low          | P3       |
| DUP-08 | Stale Hydra commands                    | Medium       | P2       |
| DUP-09 | LSP client activation                   | High         | P0       |
| ENH-01 | Deterministic Python LSP dispatcher     | High         | P0       |
| ENH-02 | Python interpreter selection            | Medium       | P0       |
| ENH-03 | Language-server configuration           | High         | P1       |
| ENH-04 | Corfu and Cape completion               | Low          | P1       |
| ENH-05 | Keybinding ergonomics                   | Low          | P2       |
| ENH-06 | Malformed customization declarations    | Medium       | P1       |
| ENH-07 | Startup optimisation                    | Low          | P1       |
| ENH-08 | Tree-sitter remapping                   | Medium       | P2       |
| ENH-09 | Built-in package handling               | Low          | P2       |
| ENH-10 | Incorrect `:ensure` argument            | Low          | P1       |
| ENH-11 | Org version policy                      | Medium       | P2       |
| ENH-12 | Eager package loading                   | Low          | P2       |
| DEF-01 | Eshell and terminal integrations        | Not assessed | Deferred |
| DEF-02 | TRAMP and containers                    | Not assessed | Deferred |
| DEF-03 | Legacy configuration files              | Medium       | Deferred |

## **9. References**

The following documentation supports the recommendations. Package-specific availability and behaviour should still be checked against the versions actually installed in your Emacs 32 distribution.

[^at-use-package]: [GNU Emacs — use-package manual](https://www.gnu.org/software/emacs/manual/html_node/use-package/?utm_source=chatgpt.com).

[^at-corfu]: [Corfu — package documentation and complementary packages](https://github.com/minad/corfu?utm_source=chatgpt.com).

[^at-project]: [GNU Emacs — Projects](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html?utm_source=chatgpt.com).

[^at-which-key]: [GNU Emacs — What's New in Emacs 30](https://www.gnu.org/software/emacs/manual/html_node/efaq/New-in-Emacs-30.html?utm_source=chatgpt.com).

[^at-eglot-features]: [GNU Emacs — Eglot Features](https://www.gnu.org/software/emacs/manual/html_node/eglot/Eglot-Features.html?utm_source=chatgpt.com).

[^at-lsp-mode]: [lsp-mode — official project documentation](https://github.com/emacs-lsp/lsp-mode?utm_source=chatgpt.com).

[^at-vertico]: [Vertico — official project documentation](https://github.com/minad/vertico?utm_source=chatgpt.com).

**Final assessment:** I would not undertake a wholesale rewrite. The existing configuration already has most of the components needed for a modern Emacs development environment.

I would implement the Python LSP ownership changes first, then remove the legacy completion and project-management overlaps, and finally address startup and keymap cleanup.

One important limitation remains: `site-boot.el`, `site-keys.el` and the other loaded modules were not supplied as source files. Their actual runtime interactions, helper definitions and load-order dependencies cannot be certified by this audit. The keymap scan provides supplementary evidence, but it is not a substitute for inspecting the complete configuration.

The safest next step is to make the Python LSP changes in isolation, tangle `site-pkgs.org`, and test ordinary `.py` files and percent-format notebooks under both `python-mode` and `python-ts-mode` before proceeding with the broader cleanup.

# A:1 (Perplexity)

[^](#toc) [**Emacs configuration audit: `site-pkgs.org`**](https://www.perplexity.ai/session/c6ac3400-9e37-477a-8cfc-c4b2298a57f5)


This audit finds a broadly modern core—Vertico/Consult/Orderless/Embark plus Corfu—but several actively conflicting completion, diagnostics, formatting, and mode-selection paths. The highest-impact fixes are to make Corfu the single in-buffer completion UI, make Flymake the single diagnostics UI, and make the Python LSP-client choice deterministic before either client starts. The analysis below is based on the attached `site-pkgs.org`; items not visible in the indexed attachment are explicitly marked _needs verification_. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)

## Executive summary

| ID | Area | Status | Risk | Priority |
|---|---|---|---|---|
| OBS-01 | `ido`, `ido-hacks`, `ido-vertical-mode`, `flx-ido` | Obsolete / disabled | Low | Low |
| OBS-02 | `auto-complete` | Obsolete / disabled | Low | Low |
| OBS-03 | `helm-company` | Obsolete / disabled | Low | Low |
| OBS-04 | `company-lsp` | Obsolete | Medium | High |
| OBS-05 | `elpy` | Obsolete / disabled | Low | Low |
| OBS-06 | `ensime`, `scala-mode2` | Obsolete / disabled | Low | Low |
| OBS-07 | `twittering-mode` | Obsolete / disabled | Low | Low |
| OBS-08 | `ranger`, `sunrise-commander` | Obsolete / disabled | Low | Low |
| OBS-09 | Legacy dead files | Unused | Low | Medium |
| DUP-01 | Corfu versus Company | Conflicting | High | High |
| DUP-02 | Flycheck versus Flymake | Conflicting | High | High |
| DUP-03 | Ruff diagnostics | Conflicting | Medium | High |
| DUP-04 | Markdown major modes | Conflicting | Medium | High |
| DUP-05 | YAML major modes | Conflicting | Medium | Medium |
| DUP-06 | JSON Tree-sitter remap | Potential conflict | Medium | Medium |
| DUP-07 | `lsp-keymap-prefix` | Conflicting | Medium | High |
| DUP-08 | Python LSP selection | Race / ambiguity | High | High |
| ENH-01 | Deterministic Python backend | Enhancement | Medium | High |
| ENH-02 | Defer heavy packages | Enhancement | Low | Medium |
| ENH-03 | Built-in modern Emacs features | Enhancement | Low | Medium |
| ENH-04 | Directory-local notebook policy | Enhancement | Medium | High |
| DEF-01 | Eshell | Deferred | Low | Low |
| DEF-02 | TRAMP | Deferred | Low | Low |

## Obsolete features

#### OBS-01: Ido family is disabled legacy completion

- Location: org heading “Completion”, package blocks `ido`, `ido-hacks`, `ido-vertical-mode`, `flx-ido`
- Status: obsolete and unused
- Evidence: the `ido` block is marked `:disabled t`; `ido-vertical-mode` and `flx-ido` are likewise disabled. The active minibuffer stack is Vertico, Marginalia, Consult, Orderless, and Embark. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete the four Ido-related blocks rather than retaining them as commented/disabled code. Keep `consult-buffer` for `C-x b` and Consult’s existing minibuffer bindings. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-02: Auto-Complete is obsolete

- Location: org heading “Completion”, package `auto-complete`
- Status: obsolete and unused
- Evidence: the `auto-complete` block is marked `:disabled t` and configures the legacy `ac-*` API, including `ac-config-default`, `ac-sources`, and `ac-completing-map`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete the block. Corfu plus Cape is the intended in-buffer completion design; Company should be removed or disabled rather than retained as a second active frontend. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-03: Helm-Company is obsolete

- Location: org heading “Completion”, package `helm-company`
- Status: obsolete and unused
- Evidence: the block is marked `:disabled t`; Helm is not shown as an active completion framework in the visible completion configuration. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete it. Corfu is the intended replacement for in-buffer completion. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-04: `company-lsp` is unmaintained

- Location: org heading “Lang LSP.mode”, package `company-lsp`
- Status: obsolete
- Evidence: `company-lsp` is loaded with `:ensure t` and `:after lsp`, while Corfu is separately enabled through `global-corfu-mode`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: remove `company-lsp`. With Corfu, use the standard `completion-at-point` integration provided by `lsp-mode`; do not add another Company backend. This is a package-maintenance judgement: mark _needs verification_ for the exact current upstream status, Confidence: Medium. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: Medium

#### OBS-05: Elpy is disabled legacy Python tooling

- Location: org heading “Lang Python.elpy”, package `elpy`
- Status: obsolete and unused
- Evidence: the block is marked `:disabled t`; the active Python configuration uses `lsp-mode`, Eglot, `pet`, `uv-mode`, and Ruff. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete the Elpy block. It cannot coexist cleanly with the active LSP-based setup and adds maintenance surface without being loaded. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-06: Ensime and Scala legacy tooling

- Location: org heading “Lang Scala”, packages `ensime` and `scala-mode2`
- Status: obsolete and unused
- Evidence: both blocks are marked `:disabled t`; the configuration also references the obsolete Ensime SBT plugin workflow. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete both blocks. If Scala support is still wanted, evaluate a current Scala LSP setup separately; do not restore Ensime. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-07: Twittering-mode is disabled legacy networking

- Location: org heading “Net Twitter.twittering”, package `twittering-mode`
- Status: obsolete and unused
- Evidence: two `twittering-mode` blocks are marked `:disabled t`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: delete both blocks. The package is legacy and the configuration has no active use shown. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-08: Ranger and Sunrise Commander are disabled alternatives to Dired

- Location: org headings “Shell Ranger”, packages `ranger` and `sunrise-commander`
- Status: obsolete and unused
- Evidence: `ranger` is marked `:disabled t`; `sunrise-commander` is configured with extensive legacy key and mode code but is not shown as active in the indexed configuration. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: remove both. The active Dired configuration already includes `dired-x`, `fd-dired`, `dired-preview`, and `peep-dired`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-09: Legacy site files are not loaded

- Location: `site-boot.el` load order; repository files `site-attic.org`, `site-config.org`, `site-custom.el`, `site-init.el`, `site-load.el`, `site-local.el`
- Status: unused
- Evidence: `site-boot.el` loads `site-pref`, `site-conf`, local overrides, `site-pkgs`, `site-func`, `site-cust`, `site-misc`, `site-edit`, `site-view`, `site-keys`, and `site-user`; the six files above are not in that sequence. The public repository confirms that all of these files exist. [github](https://github.com/hute37/emacs-site)
- Recommendation: move `site-attic.org`, `site-config.org`, `site-init.el`, and `site-load.el` into an `attic/` directory or delete them after a repository-wide search for references. Keep `site-custom.el` and `site-local.el` only if they are intentionally reserved for generated or unversioned local configuration; otherwise rename or remove them to avoid confusion with the loaded `site-cust.el` and local-override path. [github](https://github.com/hute37/emacs-site)
- Risk: Low; Effort: Low; Confidence: High

## Duplicated or conflicting features

#### DUP-01: Corfu and Company are both active completion frontends

- Location: org headings “Completion” / `comp-ap-company`, packages `corfu`, `company`, `company-posframe`
- Status: duplicate and conflicting
- Evidence: the Corfu configuration enables `global-corfu-mode`; a Company configuration enables `global-company-mode`; a separate Company configuration sets `global-company-mode` to `nil`; and `company-posframe` is enabled with `company-posframe-mode 1`. Corfu binds `TAB`, `RET`, `C-n`, `C-p`, and `C-g` in `corfu-map`, while Company binds `TAB` in `company-mode-map`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: choose Corfu as the sole in-buffer completion UI. Remove or disable Company, `company-posframe`, and `company-lsp`; retain Corfu plus Cape. If Company must remain for a specific mode, enable it only through that mode’s hook and ensure `global-company-mode` is never enabled. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: High; Effort: Medium; Confidence: High

```elisp
;; Target heading: "Completion" / `comp-ap-company`
;; Replaces: active Company configuration and `company-posframe`
;; Rollback: restore the previous `use-package company` block.
(use-package company
  :ensure t
  :disabled t
  :diminish company-mode)
```

#### DUP-02: Flycheck and Flymake are both active diagnostics systems

- Location: org headings “Lang LSP.mode” / `lang-lsp.mode`, packages `flycheck`, `lsp-mode`, `eglot`, `flymake-ruff`
- Status: duplicate and conflicting
- Evidence: `global-flycheck-mode 1` is enabled, while the configuration states that Eglot uses Flymake. `lsp-mode` is configured with `lsp-diagnostics-provider flycheck`, and `flymake-ruff` is hooked into both `python-mode` and `python-ts-mode`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: standardise on Flymake. Disable `global-flycheck-mode`, remove the `lsp-diagnostics-provider flycheck` setting, and use Eglot/Flymake for Eglot-managed buffers. For `lsp-mode`, either retain Flycheck deliberately for those buffers or configure `lsp-diagnostics-provider :flymake`; do not run both systems over the same buffer. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: High; Effort: Medium; Confidence: High

```elisp
;; Target heading: "Lang LSP.mode" / `lang-lsp.mode`
;; Replaces: `global-flycheck-mode 1` and Flycheck-based LSP diagnostics
(use-package flycheck
  :ensure t
  :disabled t)
```

#### DUP-03: Ruff diagnostics can arrive twice

- Location: org headings “Lang Python.lsp” / `lang-python.lsp`, packages `eglot`, `flymake-ruff`, `ruff-format`
- Status: duplicate or conflicting
- Evidence: Eglot is configured with a multi-server specification containing both `basedpyright-langserver --stdio` and `ruff server`; separately, `flymake-ruff` is loaded and hooked into Python modes. The file itself says that `flymake-ruff` is intended to run “alongside Eglot”. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: choose one Ruff diagnostic channel per buffer. For Eglot notebook buffers, rely on the Eglot-managed `ruff server` and do not activate `flymake-ruff`; for ordinary `lsp-mode` buffers, use either the registered Ruff LSP add-on or `flymake-ruff`, but not both. The simplest policy is to remove `flymake-ruff` and let the LSP server provide Ruff diagnostics. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: High

```elisp
;; Target heading: "Lang Python.lsp" / `lang-python.lsp`
;; Replaces: active `flymake-ruff` hook configuration
(use-package flymake-ruff
  :ensure t
  :disabled t)
```

#### DUP-04: Two Markdown major modes claim `.md`

- Location: org heading “Text Markdown”, packages `markdown-mode` and `markdown-ts-mode`
- Status: duplicate and conflicting
- Evidence: `markdown-mode` registers `.md` through `:mode`, and `markdown-ts-mode` also registers `.md` through `:mode`. Both packages are marked `:ensure t`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: retain one owner of `.md`. If Tree-sitter is preferred, remove the `:mode` entry from `markdown-mode` and let `markdown-ts-mode` own `.md`; if the classical mode is preferred, remove the `markdown-ts-mode` `:mode` entry and retain it only as an explicit command. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: High

```elisp
;; Target heading: "Text Markdown"
;; Replaces: duplicate `.md` registration in `markdown-ts-mode`
(use-package markdown-ts-mode
  :ensure t
  :mode "\\.markdown\\'"  ; optional: retain only for an explicit extension
  :config
  (add-to-list 'treesit-language-source-alist
               '(markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                             nil
                             "src/parser"
                             "src/scanner.c"))))
```

#### DUP-05: Two YAML major modes claim YAML files

- Location: org heading “Rest Yaml”, packages `yaml-ts-mode` and `yaml-mode`
- Status: duplicate and conflicting
- Evidence: `yaml-ts-mode` registers `.cff`, `.yml`, and `.yaml`; `yaml-mode` registers the same three extensions. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: retain `yaml-ts-mode` as the owner of YAML files and remove the `:mode` registrations from legacy `yaml-mode`, or delete `yaml-mode` if nothing else requires it. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: High

```elisp
;; Target heading: "Rest Yaml"
;; Replaces: duplicate `:mode` registration in `yaml-mode`
(use-package yaml-mode
  :ensure t
  :disabled t)
```

#### DUP-06: JSON Tree-sitter remap may fight `json-mode`

- Location: org headings “Lang Treesitter” / `lang-treesitter.setup`, packages `treesit-auto`, `json-mode`
- Status: potential conflict
- Evidence: `treesit-auto` remaps `json-mode` to `json-ts-mode`, while `json-mode` is separately configured with `:mode ".json"`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: retain `json-ts-mode` as the effective major mode and remove the `json-mode` `:mode` registration, or remove the JSON remap if the legacy mode is intentionally preferred. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: Medium

#### DUP-07: `lsp-keymap-prefix` is set twice

- Location: org heading “Lang LSP.mode” / `lang-lsp.mode`, package `lsp-mode`
- Status: conflicting
- Evidence: the configuration contains `setq lsp-keymap-prefix "C-c l"` followed by `setq lsp-keymap-prefix "C-l"`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: retain one prefix. `C-c l` is safer because `C-l` is normally `recenter-top-bottom`; the file itself notes that `C-l` shadows it. Remove the second `setq`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: High

```elisp
;; Target heading: "Lang LSP.mode" / `lang-lsp.mode`
;; Replaces: the second `setq lsp-keymap-prefix "C-l"`
(setq lsp-keymap-prefix "C-c l")
```

#### DUP-08: Python LSP-client selection has an ordering race

- Location: org headings “Lang R.ess” / `lang-r.ess`, “Lang Python.mode” / `lang-python.mode`, “Lang Python.lsp” / `lang-python.lsp`; functions `h7python-lsp-switch`, `h7python-use-eglot`, `h7python-use-lsp`
- Status: conflicting
- Evidence: `lsp-mode` hooks `python-mode` and `python-ts-mode` to call `lsp-deferred` unless `h7python-use-eglot` is non-nil. A separate `python-ts-mode-hook` checks whether the buffer filename contains `notebooks`, removes the `lsp-deferred` hook locally, and calls `h7python-use-eglot`. The buffer-local flag is reset to `nil` in the Python mode hooks. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: make the decision in exactly one hook, before either client starts. Use a filename/directory test plus an explicit directory-local override; do not rely on hook ordering or on removing a hook after `lsp-deferred` may already have scheduled startup. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: High; Effort: Medium; Confidence: High

```elisp
;; Target heading: "Lang Python.lsp" / `lang-python.lsp`
;; Adds: deterministic backend selection; replaces the notebook-specific
;; `python-ts-mode-hook` lambda and the `unless` guard in the lsp-mode hooks.
(defun h7python-notebook-buffer-p ()
  "Return non-nil for Python buffers beneath a `notebooks/' directory."
  (and buffer-file-name
       (string-match-p "/notebooks/" (expand-file-name buffer-file-name))))

(defun h7python-lsp-backend ()
  "Return `eglot' or `lsp' for the current Python buffer."
  (cond
   ((and (boundp 'h7python-use-eglot) h7python-use-eglot) 'eglot)
   ((h7python-notebook-buffer-p) 'eglot)
   (t 'lsp)))

(defun h7python-lsp-maybe-start ()
  "Start exactly one Python LSP client for the current buffer."
  (when (derived-mode-p 'python-base-mode)
    (pcase (h7python-lsp-backend)
      ('eglot
       (setq-local h7python-use-eglot t)
       (when (fboundp 'ruff-format-on-save-mode)
         (ruff-format-on-save-mode -1))
       (eglot-ensure))
      ('lsp
       (setq-local h7python-use-eglot nil)
       (when (fboundp 'ruff-format-on-save-mode)
         (ruff-format-on-save-mode 1))
       (lsp-deferred)))))

;; Target heading: "Lang LSP.mode" / `lang-lsp.mode`
;; Replaces: the existing `python-mode` and `python-ts-mode` lsp hooks.
(use-package lsp-mode
  :ensure t
  :hook ((python-mode python-ts-mode) . h7python-lsp-maybe-start))
```

- Rollback hint: restore the original `lsp-deferred` hooks and the notebook-specific `python-ts-mode-hook` lambda.
- LSP interaction: with this design, `lsp-mode` owns ordinary Python buffers and Eglot owns notebook buffers. Diagnostics should be Flymake-only under Eglot; `lsp-mode` should use one selected diagnostics provider, not both Flycheck and Flymake. Eldoc, Xref, and `completion-at-point` are provided by the active client; avoid loading `company-lsp` because Corfu is the intended CAPF frontend. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)

#### ENH-01: Make notebook policy explicit and directory-local

- Location: org heading “Lang Python.lsp” / `lang-python.lsp`
- Status: enhancement
- Evidence: the current notebook rule matches the substring `notebooks` anywhere in the buffer filename, rather than requiring a `notebooks` directory directly below the project root. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: use a project-root-relative check and allow a `.dir-locals.el` override for exceptional projects.

```elisp
;; Target heading: "Lang Python.lsp" / `lang-python.lsp`
;; Adds: a safer notebook-directory predicate.
;; Rollback: delete this function and restore the substring test.
(defun h7python-in-notebooks-dir-p ()
  "Return non-nil when the buffer is beneath a project-local `notebooks/' directory."
  (and buffer-file-name
       (when-let* ((project (project-current))
                   (root (project-root project)))
         (string-prefix-p
          (expand-file-name "notebooks/" root)
          (expand-file-name buffer-file-name)))))
```

```elisp
;; Optional per-project override in `.dir-locals.el':
((python-base-mode
  . ((h7python-use-eglot . t))))
```

- Risk: Low; Effort: Low; Confidence: High

## Cleanup and enhancement changes

#### ENH-02: Defer non-essential packages

- Location: org headings “Workspace Magit”, “Dired”, “Processes”, “Lang LSP.mode”, packages `magit`, `dired-preview`, `proced`, `ztree`, `lsp-ui`, `consult-lsp`, `lsp-treemacs`
- Status: enhancement
- Evidence: several packages use `:ensure t` without `:defer t` or `:commands`, including `magit`, `proced`, `ztree`, `lsp-ui`, `consult-lsp`, and `lsp-treemacs`; `dired-preview-global-mode 1` is enabled eagerly. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: defer command-oriented packages and enable their modes only in relevant buffers. This reduces startup work without changing daily behaviour.

```elisp
;; Target heading: "Workspace Magit" / `magit`
;; Replaces: the eager `use-package magit` declaration.
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch)
  :config
  (setenv "GIT_EDITOR" "emacsclient"))
```

```elisp
;; Target heading: "Dired"
;; Replaces: eager `dired-preview-global-mode 1`.
(use-package dired-preview
  :ensure t
  :defer t
  :hook (dired-mode . dired-preview-mode))
```

- Rollback hint: restore `:ensure t` without `:defer t` and the original global mode call.
- Risk: Low; Effort: Medium; Confidence: High

#### ENH-03: Prefer Emacs 32 built-ins

- Location: org headings “Completion”, “Lang LSP.mode”, “Lang Treesitter”
- Status: enhancement
- Evidence: the configuration already uses Eglot and Tree-sitter facilities; the task states that Emacs is version 32.0.50. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: verify that `use-package`, `eglot`, `project.el`, `which-key`, and `use-short-answers` are treated as built-ins rather than installed from ELPA/MELPA. Remove redundant installation declarations only after confirming that the built-in versions provide the required features. _Needs verification_ for the exact built-in status of each in this development build; Confidence: Low. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: Low

```elisp
;; Target heading: "Completion" or an early preferences block
;; Adds: concise yes/no prompts; do not enable if site-custom already does.
(customize-set-variable 'use-short-answers t)
```

- Rollback hint: remove the `customize-set-variable` form.
- Risk: Low; Effort: Low; Confidence: Medium

#### ENH-04: Use one formatting path for Python

- Location: org headings “Lang Python.lsp” / `lang-python.lsp`, “Lang Python.env” / `lang-python.env`
- Status: enhancement
- Evidence: `ruff-format-on-save-mode` is enabled for both `python-mode` and `python-ts-mode`; Eglot’s Ruff server is configured with `format enable t`; the file says that Ruff is used both as an LSP server and as a formatter. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: use `ruff-format-on-save-mode` for ordinary `lsp-mode` buffers, and disable it for Eglot notebook buffers as the existing switcher already does. Do not additionally invoke Eglot formatting on save unless a deliberate project policy requires it. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Medium; Effort: Low; Confidence: High

## Deferred items

#### DEF-01: Eshell configuration

- Location: org headings “Project”, packages `eshell-vterm`, `pcmpl-args`, `eat`
- Status: deferred
- Evidence: `eshell-vterm`, `pcmpl-args`, and `eat` are all marked `:disabled t`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: defer analysis and cleanup of Eshell integration to a later pass. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

#### DEF-02: TRAMP configuration

- Location: org heading “Completion” / `comp-mb-ver-orderless`
- Status: deferred
- Evidence: the Orderless configuration explicitly retains `file` completion for TRAMP hostnames through `completion-category-overrides`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Recommendation: defer TRAMP-specific review, including remote completion and performance, to a later pass. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Risk: Low; Effort: Low; Confidence: High

## Additional notes

- The R configuration uses ESS, but the visible Eglot configuration includes an unclear entry that appears to associate a server command with `python-ts-mode`; the exact R server configuration is _needs verification_ from the full org source. Confidence: Low. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- Julia is configured for `lsp-mode` through `lsp-julia`, with `julia-mode` and `julia-repl` also active; `eglot-jl` is also present, creating a potential second client path that should be resolved in a later focused pass. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)
- The keymap scan attachment was supplied, but its detailed duplicate-binding analysis is not included here because the current audit could not complete a full line-by-line review of that file. The keybinding findings above are limited to conflicts directly visible in `site-pkgs.org`. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/aa94c2cc-14fd-427c-ad71-d5b68a96fbf3/emacs-keymap-scan.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=600&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=76dee4cbe0f9c5bd6d97ad4e9732fadc09459d6f2f491f18c85b90f2927a366b)
- Platform-specific behaviour is not sufficiently evidenced in the visible configuration to justify X11, Wayland, WSL, container, or terminal-versus-GUI changes in this pass. Any such changes should be guarded with `display-graphic-p`, `featurep` checks, or explicit environment tests only after the relevant `site-*` files are reviewed. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)

## Appendix: package inventory

This inventory covers packages and configuration blocks visible in the indexed attachment. It is not a claim that every package in the 323 KB source was individually inspected; unavailable sections are marked accordingly. [ppl-ai-file-upload.s3.amazonaws](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/attachments/49918696/94e5fa69-fc0d-40e0-8dd8-ba03a290c34f/site-pkgs.org?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Checksum-Mode=ENABLED&X-Amz-Credential=ASIA2F3EMEYESAZSP5BS%2F20261113%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20261113T175649Z&X-Amz-Expires=2852&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEML%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJIMEYCIQCybyGD8rnmEQYOrFEt%2F4NqMu1wwMpo5aB4XwhFNA1d1AIhAMgVYyTt7wp%2F82Ghu%2B82m7y2MQOK9QPZQHRpgkv1vgQ6KokFCIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQARoMNjk5NzUzMzA5NzA1Igxat05mV9d5NnK1T6Iq3QQ9rTXxiUDJMDmyDp9DLli5n2mcYS8GgO4V4xN5TZqSr3IIRsLUgLtVwFddz9bxNV4LklHCvKFeOc8iKClI7xVB5cXPawWSlG8sy7k54cijKqtUg07dIyWQa0hv5%2Fpg6BOZswU79iYKDHst%2BaH18SUDI6vflR%2BYu7r6FHybohZ92Wl8p%2BxxhdXIlY9TkCbBNUHvEVwqzjh3WMJqr%2BYHbdrFR7sNCHjSWbpLueDwsG8FZJhaiMNdF2PC7YfF8CaHWZu9%2F2w%2FoBzVibkvZJ63pVwMY8Mxn9qYojauAQw7v4p7u%2B4l0ownv3d3Qsm4TP%2BPL%2FAPKJKtNTlQ0ZwUQLeYXL%2FN26xTBXz0zHrkKPv4Lkktx8GFzgalwN0ymp4CpYXHMwXNb2Gy3H%2B8%2B4NiFiT9F9%2BeDXhlgKnRqCvoIs5zm2r830j14%2B243BHB%2BFlp5hZP5R%2F4QqQSKAlBNgIwOtaVzVAHIBqH%2FCIUg1LbaAyIF2TVNjf6aPsxay4a3%2FgVhBjlj1T3IxfvC8P9ONtu2IKNcT3A3AieIwemwmHTCn5gkrnQe2u43LY5Wq0a9giAbFiB02B2muDm64MhiHk1htj6bBAe60l0Pq%2FYr457%2F%2FT9jHH4pj9VusgJoVw7iVYbaQmBKpgqfWlkDB4syJxs8oZecMDqqjw9RjYEEePse6PKTcVU5180gk%2FClkhmE8hobmw0R6UjyugxYcsGRvL4Mr26DEv4TZIhp1bSt4acUi4nm3IOuEEoeXX1qLDTxKZbLNXkCaQAxU2Klz9aP1Ee%2B0dohwdOPJZRw1aiTSSW5wBDZDCymd3XBjqXAVDsllpSC52WzT7dOryHPkBma2%2Bk6Kgz%2BL4hF9%2F6cXX7jUEOqG4uBc3fTYmGqjibYehZclMxdSqKVj95iV0Fr6zEj7X2%2FclIjvpYq3mIM2OhztasLyajRqs3XR57DIngTuOsManYj8ewvWTrcqfp4VvT%2BvvEZsfehvu2ye0DKHThwCMmV%2B%2FqVtdhIIafHf0rvEC8sybMeGI%3D&X-Amz-SignedHeaders=host&x-id=GetObject&X-Amz-Signature=5b5be54612e6282559597af850ebaf98f801c40a845108e3cbea61da21f8a205)

| Package / block | Status | Verdict |
|---|---|---|
| `corfu` | Active | Healthy, but conflicts with Company |
| `company` | Active in one configuration; disabled in another | Duplicate/conflicting |
| `company-posframe` | Active | Duplicate with Corfu |
| `company-lsp` | Active | Obsolete |
| `helm-company` | Disabled | Obsolete |
| `auto-complete` | Disabled | Obsolete |
| `ido` | Disabled | Obsolete |
| `ido-hacks` | Disabled | Obsolete |
| `ido-vertical-mode` | Disabled | Obsolete |
| `flx-ido` | Disabled | Obsolete |
| `marginalia` | Active | Healthy |
| `all-the-icons-completion` | Active | Healthy |
| `vertico` | Active | Healthy |
| `consult` | Active | Healthy |
| `consult-dir` | Active | Healthy |
| `consult-todo` | Active | Healthy |
| `orderless` | Active | Healthy |
| `embark` | Active | Healthy |
| `embark-consult` | Active | Healthy |
| `savehist` | Active | Healthy |
| `recentf` | Active built-in | Healthy |
| `olivetti` | Active | Healthy |
| `magit` | Active | Healthy, but should be deferred |
| `magit-backup` | Active | Needs verification |
| `git-timemachine` | Active | Healthy |
| `vdiff` | Active | Healthy |
| `vdiff-magit` | Disabled | Unused |
| `git-gutter-fringe` | Disabled | Unused |
| `dired` | Active built-in | Healthy |
| `ls-lisp` | Active built-in | Healthy |
| `dired-x` | Active | Healthy |
| `find-dired` | Active built-in | Healthy |
| `fd-dired` | Active | Healthy |
| `dired-preview` | Active | Healthy, but should be deferred |
| `peep-dired` | Active | Potential overlap with `dired-preview` |
| `ztree` | Active | Healthy, but should be deferred |
| `proced` | Active | Healthy, but should be deferred |
| `sh-script` | Active | Healthy |
| `flymake-shellcheck` | Active | Healthy |
| `ranger` | Disabled | Obsolete |
| `sunrise-commander` | Active / legacy | Obsolete |
| `sh-toggle` | Disabled | Unused |
| `markdown-mode` | Active | Conflicts with `markdown-ts-mode` |
| `markdown-ts-mode` | Active | Conflicts with `markdown-mode` |
| `add-node-modules-path` | Active | Healthy |
| `pandoc-mode` | Active | Healthy |
| `css-mode` | Active built-in | Healthy |
| `rainbow-mode` | Disabled | Unused |
| `nxml-mode` | Active built-in | Healthy |
| `web-mode` | Disabled | Unused |
| `lsp-mode` | Active | Healthy, but has prefix and diagnostics conflicts |
| `lsp-ui` | Active | Healthy, but should be deferred |
| `consult-lsp` | Active | Healthy, but should be deferred |
| `company-lsp` | Active | Obsolete |
| `lsp-treemacs` | Active | Healthy, but should be deferred |
| `dap-mode` | Active | Healthy |
| `treesit-auto` | Active | Healthy |
| `yasnippet` | Disabled | Unused |
| `google-c-style` | Active | Healthy |
| `ess` | Active | Healthy |
| `eglot` | Active | Healthy, but requires deterministic Python policy |
| `ruff-format` | Active | Healthy, but needs formatting-policy review |
| `flymake-ruff` | Active | Conflicts with Eglot Ruff server |
| `python` | Active built-in | Healthy |
| `inferior-python-mode` | Active | Healthy |
| `hide-mode-line` | Active | Healthy |
| `uv-mode` | Active | Healthy |
| `pet` | Active | Healthy |
| `pyvenv-auto` | Active | Potential overlap with `uv-mode` |
| `pyvenv` | Active | Potential overlap with `uv-mode` |
| `with-venv` | Active | Healthy |
| `poetry` | Disabled | Unused |
| `verb` | Active | Healthy |
| `restclient` | Active | Healthy |
| `restclient-jq` | Active | Healthy |
| `company-restclient` | Active | Conflicts with Corfu policy |
| `ob-http` | Active | Healthy |
| `ansible` | Active | Healthy |
| `ansible-doc` | Active | Healthy |
| `ansible-vault` | Active | Healthy |
| `company-ansible` | Active | Conflicts with Corfu policy |
| `jinja2-mode` | Active | Healthy |
| `poly-ansible` | Active | Healthy |
| `company-terraform` | Active | Conflicts with Corfu policy |
| `powershell` | Active | Healthy |
| `gherkin-mode` | Active | Healthy |
| `plantuml-mode` | Active | Healthy |
| `lua-mode` | Active | Healthy |
| `julia-mode` | Active | Healthy |
| `julia-repl` | Active | Healthy |
| `lsp-julia` | Active | Healthy, but overlaps `eglot-jl` |
| `eglot-jl` | Active | Potential overlap with `lsp-julia` |
| `ess-julia` | Active | Potential overlap with `lsp-julia` |
| `maxima` | Active | Healthy |
| `haskell-mode` | Active | Healthy |
| `ghc` | Active | Needs verification |
| `flycheck-haskell` | Active | Conflicts with Flymake policy |
| `idris-mode` | Disabled | Unused |
| `json-mode` | Active | Potential conflict with JSON Tree-sitter remap |
| `json-reformat` | Active | Healthy |
| `yaml-ts-mode` | Active | Conflicts with `yaml-mode` |
| `yaml-mode` | Active | Conflicts with `yaml-ts-mode` |
| `elpy` | Disabled | Obsolete |
| `puppet-mode` | Disabled | Unused |
| `poly-R` | Disabled | Unused |
| `gptel` | Disabled | Unused |
| `twittering-mode` | Disabled | Obsolete |
| `ensime` | Disabled | Obsolete |
| `scala-mode2` | Disabled | Obsolete |
| `projectile` | Not visible in indexed extraction | Needs verification |
| `which-key` | Not visible in indexed extraction | Needs verification |
| `cape` | Not visible in indexed extraction | Needs verification |

## Footnote definitions

[^ap-eglot]: [Eglot manual, GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/eglot/index.html)

[^ap-use-package]: [`use-package` manual, GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/use-package/)

[^ap-vertico]: [Vertico repository](https://github.com/minad/vertico)

[^ap-corfu]: [Corfu repository](https://github.com/minad/corfu)

[^ap-consult]: [Consult repository](https://github.com/minad/consult)

[^ap-embark]: [Embark repository](https://github.com/karthink/embark)

[^ap-orderless]: [Orderless repository](https://github.com/oantolin/orderless)

[^ap-flymake]: [Flymake manual, GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Flymake.html)

[^ap-flycheck]: [Flycheck documentation](https://www.flycheck.org/en/latest/)

[^ap-lsp-mode]: [LSP Mode documentation](https://emacs-lsp.github.io/lsp-mode/)

[^ap-company]: [Company documentation](https://company-mode.github.io/)

[^ap-project]: [Projects manual, GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)

[^ap-which-key]: [which-key repository](https://github.com/justbur/emacs-which-key)


# A:1 (DeepSeek)

[^](#toc) [**Emacs Configuration Audit: `site-pkgs.org`**](https://chat.deepseek.com/share/bz21i3tzkfj7rvrj9r)

## Executive Summary

The configuration is a large, long-lived `use-package` codebase that has already adopted many modern tools (`vertico`, `consult`, `embark`, `corfu`, `cape`, `treesit-auto`, `eglot`). However, it still carries legacy layers that create real conflicts: **`company` and `corfu` compete for in-buffer completion**, **`flycheck` and `flymake` (via Eglot) compete for diagnostics**, and **both `lsp-mode` and `eglot` are configured for Python and Julia without a reliable per-buffer switch**. A notebook hook is syntactically broken. `ace-jump-mode`, `undo-tree`, `diminish`, and `flyspell` have modern replacements already present or clearly preferred. The global keymap scan shows extensive use of `global-set-key` in `site-keys.el`, but that file was not provided, so global keybinding conflicts cannot be fully verified.

### Summary of Findings

| ID | Status | Risk | Priority |
|----|--------|------|----------|
| OBS-01 | `ace-jump-mode` superseded by `avy` | Low | High |
| OBS-02 | `undo-tree` superseded by `vundo` (candidate) | Low | Medium |
| OBS-03 | `diminish` superseded by `delight` | Low | Low |
| OBS-04 | `flyspell` coexists with modern `jinx` | Medium | Medium |
| OBS-05 | `global-flycheck-mode` conflicts with Eglot’s Flymake | High | High |
| DUP-01 | `company` and `corfu` both active | High | High |
| DUP-02 | `lsp-mode` and `eglot` both configured for Python without reliable isolation | High | High |
| DUP-03 | `lsp-julia` and `eglot-jl` both active for Julia | High | Medium |
| DUP-04 | `projectile` and built-in `project.el` both active | Medium | Medium |
| ENH-01 | Notebook Eglot hook has a syntax error and fragile hook removal | High | High |
| ENH-02 | `use-package` is built-in; remove `:ensure t` from built-ins | Low | Medium |
| ENH-03 | `which-key` is built-in; remove `:ensure t` | Low | Low |
| ENH-04 | Global keybindings cannot be fully audited without `site-keys.el` | — | — |
| DEF-01 | `eshell` / `tramp` deferred | — | — |

---

## Obsolete Features

#### OBS-01: `ace-jump-mode` superseded by `avy`

- **Location:** `site-pkgs.org`, heading `Utils/Jump`, `use-package ace-jump-mode` and `use-package avy`
- **Status:** obsolete and duplicate
- **Evidence:** `(use-package ace-jump-mode :ensure t :commands ace-jump-mode ...)` is active while `(use-package avy :ensure t)` is also loaded.
- **Recommendation:** Remove `ace-jump-mode`. Bind `avy-goto-char` or `avy-goto-word-1` to the key previously used for `ace-jump-mode`. `avy` is actively maintained and integrates with modern completion.
- **Risk:** Low; **Effort:** Low; **Confidence:** High.

#### OBS-02: `undo-tree` superseded by `vundo` (candidate)

- **Location:** `site-pkgs.org`, heading `Utils/Misc`, `use-package undo-tree`
- **Status:** obsolete (modern replacement preferred)
- **Evidence:** `(use-package undo-tree :config (global-undo-tree-mode) ...)` is active. `vundo` is built‑in since Emacs 28 and does not replace the undo system, avoiding `undo-tree`’s historical state‑management issues.
- **Recommendation:** If the visual tree is the only reason for `undo-tree`, replace it with `vundo` (`M-x vundo`). Keep `undo-tree` only if its diff or branching features are specifically required; in that case document the exception.
- **Risk:** Low; **Effort:** Low; **Confidence:** Medium (undo‑tree is still maintained; “obsolete” is a judgement call).

#### OBS-03: `diminish` superseded by `delight`

- **Location:** `site-pkgs.org`, heading `Starting up → Use-Package`, `use-package diminish`; heading `UI`, `use-package delight`
- **Status:** duplicate / obsolete
- **Evidence:** Both `(use-package diminish :ensure t)` and `(use-package delight :ensure t)` are present. `delight` is the newer, `use-package`‑friendly replacement.
- **Recommendation:** Remove `diminish`. Use only `delight` for hiding minor‑mode lighter strings.
- **Risk:** Low; **Effort:** Low; **Confidence:** High.

#### OBS-04: `flyspell` coexists with modern `jinx`

- **Location:** `site-pkgs.org`, heading `TeX → Spell`, `use-package flyspell` and `use-package jinx`
- **Status:** duplicate / obsolete candidate
- **Evidence:** `flyspell` is active with hooks for `markdown-mode`, `markdown-ts-mode`, and `prog-mode`, while `jinx` is also active with `jinx-correct` and `jinx-languages` bindings. `jinx` is a modern, faster spell‑checker that uses the same `hunspell` backend but avoids `flyspell`’s overhead.
- **Recommendation:** Choose one. If `jinx` is preferred, remove the `flyspell` hooks and keep `jinx`. If `flyspell`’s `prog-mode` behaviour is still needed, keep `flyspell` only there and disable `jinx` globally. Do not run both on the same buffers.
- **Risk:** Medium (spell‑checking is user‑visible); **Effort:** Low; **Confidence:** Medium.

#### OBS-05: `global-flycheck-mode` conflicts with Eglot’s Flymake

- **Location:** `site-pkgs.org`, heading `Lang: LSP → Lang: LSP.mode`, `use-package flycheck` with `(global-flycheck-mode 1)`; `use-package eglot` also active.
- **Status:** duplicate / conflicting
- **Evidence:** `(global-flycheck-mode 1)` enables Flycheck globally. Eglot uses Flymake internally for diagnostics. In Eglot‑managed buffers, the two systems can both try to report errors, or Flycheck may override Flymake, producing inconsistent diagnostics.
- **Recommendation:** Either (a) use `flycheck-eglot` to let Flycheck consume Eglot diagnostics, or (b) disable `flycheck` in Eglot‑managed buffers via a hook. If `lsp-mode` uses `:flycheck` (as configured), keep Flycheck only for `lsp-mode` and use Flymake for Eglot. A deterministic per‑buffer hook is provided in ENH‑01.
- **Risk:** High (diagnostics may be duplicated or missing); **Effort:** Medium; **Confidence:** High.

---

## Duplicated or Conflicting Features

#### DUP-01: `company` and `corfu` both active

- **Location:** `site-pkgs.org`, heading `Completion "at-point" → Company` (`use-package company`, `company-posframe`) and heading `Completion "at-point" → Corfu` (`use-package corfu`).
- **Status:** duplicate / conflicting
- **Evidence:** `corfu` is configured with `global-corfu-mode` and keybindings. `company` is also loaded, and `company-posframe-mode` is enabled. `company` is not globally enabled (`(global-company-mode nil)`), but it is still available and can be triggered, creating two in‑buffer completion UIs.
- **Recommendation:** Remove the `company`, `company-posframe`, `company-auctex`, `company-ansible`, and `company-terraform` blocks. `corfu` with `cape` is the target design. If a specific `company` backend is still needed, use its `cape` equivalent (e.g., `cape-dabbrev`, `cape-file`).
- **Risk:** High (two completion popups can appear); **Effort:** Medium; **Confidence:** High.

#### DUP-02: `lsp-mode` and `eglot` both configured for Python without reliable isolation

- **Location:** `site-pkgs.org`, heading `Lang: Python → Lang: Python/lsp`, `h7/python-lsp-switch`, and the notebook hook at the end of the same heading; also `use-package lsp-mode` with `python-mode` / `python-ts-mode` hooks.
- **Status:** duplicate / conflicting
- **Evidence:** `lsp-mode` hooks start `lsp-deferred` in `python-mode` and `python-ts-mode` unless `h7/python-use-eglot` is set. `eglot` is started manually via `h7/python-lsp-switch` and automatically via a `python-ts-mode-hook` that checks for `"notebooks"` in the path. The hook is syntactically broken (missing a closing parenthesis in the `and`), and it tries to remove `lsp-deferred` from hooks that were never added in that buffer. This does not guarantee exactly one client per buffer.
- **Recommendation:** Implement a single, deterministic buffer‑local function that:
  1. Checks whether the buffer is a notebook (`notebooks` in the path or `# %%` marker).
  2. Shuts down the other client if it is running (`lsp-disconnect` / `eglot-shutdown`).
  3. Starts exactly one client.
  Provide corrected `h7/` helpers (see ENH‑01). Also remove `lsp-deferred` from `python-mode-hook` / `python-ts-mode-hook` and let the buffer‑local function decide.
- **Risk:** High (two LSP servers may attach, causing duplicate diagnostics and completion); **Effort:** Medium; **Confidence:** High.

#### DUP-03: `lsp-julia` and `eglot-jl` both active for Julia

- **Location:** `site-pkgs.org`, heading `Lang: Julia`, `use-package eglot-jl` and `use-package lsp-julia`.
- **Status:** duplicate / conflicting
- **Evidence:** `eglot-jl` hooks `julia-mode` to `eglot-ensure`. `lsp-julia` is also loaded, though its hook is commented out. Both register Julia language‑server configurations. If a user manually starts `lsp` or `eglot`, or if a hook is added later, two clients can attach.
- **Recommendation:** Choose one client for Julia. `eglot-jl` is lighter and consistent with the Eglot‑for‑notebooks design; `lsp-julia` may be preferred if `lsp-mode`’s UI is required. Disable the other (`:disabled t`). Report in the audit which one is chosen.
- **Risk:** High (two Julia servers); **Effort:** Low; **Confidence:** High.

#### DUP-04: `projectile` and built-in `project.el` both active

- **Location:** `site-pkgs.org`, heading `Workspace → Project`, `use-package projectile`; also `consult-projectile`.
- **Status:** duplicate (overlapping functionality)
- **Evidence:** `projectile` is configured with `:config` and caches. `project.el` is built in and used by `project-*` commands (e.g., in `hydra-projects` and `ghostel` `project-prefix-map`). Both provide project switching, file finding, and search.
- **Recommendation:** If `projectile`’s extra features (e.g., caching, `projectile-commander`) are not essential, migrate the project‑level bindings to `project.el` (`C-x p` prefix) and remove `projectile`, `treemacs-projectile`, `consult-projectile`. If `projectile` is kept, remove redundant `project.el` bindings from the hydras to avoid confusion. At minimum, document which one is authoritative.
- **Risk:** Medium (two project systems); **Effort:** Medium; **Confidence:** High.

---

## Cleanup and Enhancement Changes

Changes are ordered by priority. Each snippet states the target org heading and whether it replaces or adds code.

### ENH-01: Fix the Python LSP‑switch hook (replaces the broken notebook hook)

- **Target heading:** `Lang: Python → Lang: Python/lsp`
- **Action:** Replace the final `add-hook 'python-ts-mode-hook` block (which contains a syntax error) with a clean, buffer‑local function using `h7/` naming.

```elisp
;; Replace the final add-hook in "Lang: Python/lsp"
(defun h7/python-notebook-p ()
  "Return non-nil when the current buffer is a Jupytext notebook.
A notebook is either inside a `notebooks' directory or contains
a `# %%' cell marker in the first 2000 characters."
  (and buffer-file-name
       (or (string-match-p "/notebooks/" buffer-file-name)
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "^# %%" (min (point-max) 2000) t)))))

(defun h7/python-apply-lsp-backend ()
  "Start exactly one LSP client for the current Python buffer.
Eglot for notebooks, lsp-mode otherwise.  This function is
idempotent and safe to call from a mode hook."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (if (h7/python-notebook-p)
        (h7/python-use-eglot)
      (h7/python-use-lsp))))

(add-hook 'python-mode-hook #'h7/python-apply-lsp-backend)
(add-hook 'python-ts-mode-hook #'h7/python-apply-lsp-backend)
```

**Rollback:** remove the new `h7/` functions and the two `add-hook` calls; restore the original (broken) block if needed for reference.

### ENH-02: Remove `:ensure t` from built-in packages (`use-package`, `which-key`, `project`, `eglot`)

- **Target headings:** `Starting up → Use-Package` (for `use-package` itself, if `:ensure t` is present), `Utils/Help` (`which-key`), `Workspace → Project` (`project` is not explicitly `use-package`d, but `project.el` is built in), `Lang: LSP → Lang: LSP.mode` (`eglot`).
- **Action:** `use-package` is built into Emacs 32. `which-key` is built in since Emacs 30. `project.el` and `eglot` are built in. Using `:ensure t` forces `package.el` to look for a newer version on ELPA, adding startup work and potentially pulling a different version than the one shipped with Emacs.

```elisp
;; Example for which-key in "Utils/Help"
(use-package which-key
  ;; :ensure t  ; built-in since Emacs 30
  :delight
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))
```

**Rollback:** re‑add `:ensure t` if a specific ELPA version is required.

### ENH-03: Replace `ace-jump-mode` with `avy` (cleanup)

- **Target heading:** `Utils/Jump`
- **Action:** Delete the `use-package ace-jump-mode` block and add an `avy` binding.

```elisp
;; Replace the ace-jump-mode block
(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-char-timer)   ; or the key previously used for ace-jump
         ("M-g w" . avy-goto-word-1)))
```

**Rollback:** restore the `ace-jump-mode` block.

### ENH-04: Use `flycheck-eglot` or disable Flycheck in Eglot buffers

- **Target heading:** `Lang: LSP → Lang: LSP.mode` (`flycheck` block).
- **Action:** If keeping Flycheck for `lsp-mode`, prevent it from interfering with Eglot. The simplest deterministic approach is to disable `flycheck-mode` in Eglot‑managed buffers.

```elisp
;; Add after the flycheck use-package
(defun h7/disable-flycheck-in-eglot ()
  "Disable Flycheck when Eglot manages the buffer."
  (when (bound-and-true-p eglot--managed-mode)
    (flycheck-mode -1)))

(add-hook 'eglot-managed-mode-hook #'h7/disable-flycheck-in-eglot)
```

Alternatively, use `flycheck-eglot` so Flycheck reports Eglot diagnostics; then remove Flymake’s role.

**Rollback:** remove the hook and the helper.

### ENH-05: Remove `company` and related backends (consolidate on Corfu)

- **Target headings:** `Completion "at-point" → Company`, and any `company-*` blocks (`company-auctex`, `company-ansible`, `company-terraform`).
- **Action:** Delete the `use-package company`, `use-package company-posframe`, and related blocks. Ensure `corfu` and `cape` cover the same use cases with `cape-dabbrev`, `cape-file`, `cape-keyword`, etc.

**Rollback:** re‑add the `company` blocks if a specific backend is missed.

### ENH-06: Improve startup by deferring heavy modes

- **Target headings:** `UI` (`all-the-icons`), `Workspace → Magit` (`magit`), `Org → Org-mode` (`org`).
- **Action:** `all-the-icons` loads icon fonts at startup. Use `:defer t` or `:commands` where possible. `magit` is already deferred implicitly by `use-package` unless `:demand t`. `org` could be deferred with `:defer t` if the agenda is not needed immediately. However, these are optional; test startup impact with `emacs-init-time`.

**Rollback:** remove the `:defer` keywords.

---

## Deferred Items (`eshell`, `tramp`)

#### DEF-01: `eshell` and `tramp` findings deferred

- **Location:** `site-pkgs.org`, headings `Shell → eshell` and mentions of `tramp` in comments and `eshell-there`.
- **Status:** deferred
- **Evidence:** The `eshell` block is large and contains many custom functions, keybindings, and completion settings. `tramp` is referenced in `eshell-there` and `ha/eshell-host-regexp`.
- **Recommendation:** Do not audit in this pass. The `eshell` configuration is tightly coupled to `consult-history`, `corfu`, and `cape`, so changes to the completion stack (ENH‑05) may affect it; re‑visit after the main cleanup.
- **Risk:** —; **Effort:** —; **Confidence:** —.

---

## Additional Notes

1. **Global keybindings are not fully verifiable.** The file `emacs-keymap-scan.txt` shows extensive `global-set-key` calls in `site-keys.el` (e.g., `C-f` → `consult-line`, `M-up` → `dired`, `f9` → `eshell-toggle`). Because `site-keys.el` was not provided, conflicts between those bindings and the `:bind` forms in `site-pkgs.org` cannot be confirmed. The scan does, however, show that `site-keys.el` and `site-load.el` / `site-init.el` contain many overlapping bindings, which suggests dead or competing keymaps. Treat `site-load.el` and `site-init.el` as dead code: they are not loaded by `site-boot.el` (which loads `site-pref`, `site-conf`, `site-pkgs`, `site-func`, `site-cust`, `site-misc`, `site-edit`, `site-view`, `site-keys`, `site-tool`, `site-user`).

2. **The notebook hook syntax error.** The final `add-hook` in `Lang: Python/lsp` is missing a closing parenthesis in the `and` form:

   ```elisp
   (when (and buffer-file-name 
              (string-match-p "notebooks" (buffer-file-name))
     ;; ^ missing one closing paren for `and`
   ```

   This will signal `wrong-type-argument` when the hook runs. ENH‑01 replaces it.

3. **R has no LSP client configured.** `ess` is configured for R, but neither `lsp-mode` nor `eglot` is hooked into `ess-mode` or `R-mode`. If R LSP is desired, add a hook for `ess-r-mode` or `R-mode`. Currently, R relies on ESS’s built‑in completion and help, which is acceptable but should be an explicit choice.

4. **Julia has two LSP clients configured** (`eglot-jl` and `lsp-julia`). This is a conflict (DUP‑03). Choose one and disable the other.

5. **`use-package` is built‑in** in Emacs 32, but the configuration still contains commented‑out bootstrap code (Straight, Quelpa, manual `package-install` of `use-package`) under `Starting up → Use-Package`. That code is dead (commented out) and can be removed to reduce confusion.

6. **`req-package` is disabled** (`:disabled t`). It is obsolete; `use-package` alone is sufficient.

7. **`elpy`, `ein`, `auto-complete`, `ido`, `gtags`, `w3m`, `puppet-mode`, `ranger`, `twittering-mode`, `gptel` are disabled** or commented out. They do not affect the running configuration but add noise. Consider moving them to `site-attic.org` or deleting them.

---

## Appendix: Package Inventory

| Package / Block | Status | Verdict |
|----------------|--------|---------|
| `bind-key` | active | healthy (dependency of `use-package`) |
| `diminish` | active | obsolete; use `delight` |
| `use-package-ensure-system-package` | active | healthy |
| `req-package` | disabled | obsolete; remove |
| `general` | active | healthy |
| `emacs` (basic, config, window) | active | healthy |
| `zoom-window` | active | healthy |
| `ef-themes` | active | healthy |
| `minions` | active | healthy |
| `mood-line` | active | healthy |
| `delight` | active | healthy (preferred over `diminish`) |
| `all-the-icons` | active | healthy |
| `solaire-mode` | active | healthy |
| `hl-todo` | active | healthy |
| `dash`, `f`, `s` | active | healthy |
| `hydra`, `use-package-hydra` | active | healthy |
| `ace-jump-mode` | active | obsolete; use `avy` |
| `avy` | active | healthy |
| `regex-tool`, `visual-regexp` | active | healthy |
| `which-key` | active | built‑in; remove `:ensure t` |
| `helpful` | active | healthy |
| `undo-tree` | active | candidate for `vundo` |
| `recentf` | active | healthy |
| `popper` | active | healthy |
| `comint` | active | healthy |
| `restart-emacs` | active | healthy |
| `autorevert` | active | healthy |
| `visible-mark` | active | healthy |
| `hilit-chg`, `goto-chg` | active | healthy |
| `outline-indent`, `treesit-fold` | active | healthy |
| `kirigami` | active | healthy |
| `savefold`, `vimish-fold` | disabled | unused |
| `calc`, `crux` | active | healthy |
| `pass` | commented | unused |
| `server` | active | healthy |
| `websocket`, `simple-httpd` | active | healthy |
| `fontaine` | disabled | unused |
| `ligature` | active | healthy |
| `evil` | active | healthy |
| `cape`, `capf-autosuggest`, `esh-autosuggest` | active | healthy |
| `corfu` | active | healthy |
| `company`, `company-posframe` | active | conflict with `corfu` |
| `auto-complete`, `ido` | disabled | obsolete |
| `vertico`, `marginalia`, `consult`, `orderless`, `embark`, `embark-consult` | active | healthy (modern stack) |
| `savehist` | active | healthy |
| `olivetti` | active | healthy |
| `magit` | active | healthy |
| `git-timemachine` | active | healthy |
| `vdiff` | active | healthy |
| `ls-lisp`, `dired`, `dired-x` | active | healthy |
| `fd-dired`, `dired-preview` | active | healthy |
| `ztree` | active | healthy |
| `projectile`, `consult-projectile` | active | conflict with `project.el` |
| `treemacs` (+ extensions) | active | healthy |
| `etags` | active | healthy |
| `gtags` | disabled | unused |
| `wgrep`, `ag`, `rg`, `deadgrep`, `grep` | active | healthy |
| `ghostel`, `vterm`, `multi-vterm` | active | healthy |
| `eshell` (+ extensions) | active | deferred |
| `proced` | active | healthy |
| `sh-script` | active | healthy |
| `flymake-shellcheck` | commented | unused |
| `ranger` | disabled | unused |
| `markdown-mode`, `markdown-ts-mode` | active | both active; see note |
| `pandoc-mode` | active | healthy |
| `css-mode`, `nxml-mode` | active | healthy |
| `lsp-mode`, `lsp-ui`, `consult-lsp`, `lsp-treemacs` | active | healthy for `lsp-mode` |
| `dap-mode` | active | healthy |
| `flycheck` | active | conflict with Eglot’s Flymake |
| `eglot` | active | healthy (built‑in) |
| `julia-mode` | active | healthy |
| `eglot-jl` | active | conflict with `lsp-julia` |
| `lsp-julia` | active | conflict with `eglot-jl` |
| `ess` | active | healthy (no LSP) |
| `polymode`, `poly-markdown` | active | healthy |
| `poly-R` | disabled | unused |
| `python` (mode) | active | healthy |
| `uv-mode`, `pet`, `pyvenv-auto`, `with-venv` | active | healthy |
| `poetry` | disabled | unused |
| `lsp-pyright` | active | healthy |
| `ruff-format` | active | healthy |
| `python-pytest` | active | healthy |
| `cov` | active | healthy |
| `gherkin-mode` | active | healthy |
| `elpy` | disabled | obsolete |
| `ein` | disabled | unused |
| `zmq`, `jupyter`, `code-cells` | active (conditional) | healthy |
| `maxima` | active | healthy |
| `ruby-mode` | active | healthy |
| `puppet-mode` | disabled | unused |
| `lua-mode` | active | healthy |
| `scala-mode2`, `ensime` | disabled | unused |
| `haskell-mode` | active | healthy |
| `idris-mode` | disabled | unused |
| `drools-mode` | active | healthy |
| `plantuml-mode` | active | healthy |
| `json-mode`, `json-reformat` | active | healthy |
| `yaml-ts-mode`, `yaml-mode` | active | healthy |
| `verb`, `restclient`, `restclient-jq` | active | healthy |
| `ansible`, `ansible-doc`, `company-ansible` | active | `company-ansible` conflicts with Corfu |
| `jinja2-mode`, `poly-ansible` | active | healthy |
| `company-terraform` | active | conflicts with Corfu |
| `docker`, `dockerfile-mode` | active | healthy |
| `w3m` | disabled | unused |
| `google-this` | active | healthy |
| `pocket-reader` | disabled | unused |
| `elfeed-org`, `elfeed` | active | healthy |
| `twittering-mode` | disabled | unused |
| `gptel`, `gptel-ask` | disabled | unused |
| `openai`, `chatgpt`, etc. | disabled | unused |
| `auctex`, `auctex-latexmk`, `cdlatex` | active | healthy |
| `company-auctex` | active | conflicts with Corfu |
| `tex` | active | healthy |
| `biblio` | active | healthy |
| `nov` | active | healthy |
| `pdf-tools`, `pdf-view-restore` | active | healthy |
| `pdfgrep`, `djvu` | active | healthy |
| `flyspell` | active | duplicate with `jinx` |
| `jinx` | active | healthy |
| `org` (+ many extensions) | active | healthy |
| `dashboard` | active | healthy |
| `engine-mode` | active | healthy |
| `hydra` (many hydras) | active | healthy |

---

## Footnotes

[^ad-use-package]: `use-package` is built into Emacs since version 29. See the Emacs manual, “Package Installation”: <https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html>.

[^ad-eglot]: Eglot is built into Emacs since version 29. See the Eglot manual: <https://www.gnu.org/software/emacs/manual/html_node/eglot/index.html>.

[^ad-project-el]: `project.el` is built into Emacs since version 25. See the Emacs manual, “Projects”: <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>.

[^ad-which-key]: `which-key` is built into Emacs since version 30. See the Emacs 30 NEWS file: <https://www.gnu.org/software/emacs/news/NEWS.30>.

[^ad-vundo]: Vundo is available from GNU ELPA and is described as a simpler alternative to `undo-tree`: <https://elpa.gnu.org/packages/vundo.html>.

[^ad-flycheck-flymake]: For the evolving comparison between Flycheck and Flymake, see the Flycheck manual’s “Flycheck versus Flymake”: <https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html>.

[^ad-corfu]: Corfu is a minimal in‑buffer completion package built on the standard `completion-at-point` API: <https://github.com/minad/corfu>.

[^ad-vertico]: Vertico is the recommended minibuffer completion UI in the modern stack: <https://github.com/minad/vertico>.


# A:1 (Kimi)

[^](#toc) [**Audit of `site-pkgs.org`: Modernisation Report_**](https://www.kimi.ai/share/1a1d6177-3c42-8ea3-8000-00002cc5b596))


## Executive Summary /k

The configuration is in better shape than its age suggests: the vertico/consult/embark/corfu stack is present and mostly wired correctly, and the lsp-mode/eglot coexistence scaffolding already exists. The main problems are: (1) several _active_ legacy packages that duplicate or fight the target stack (company, undo-tree, projectile, treemacs suite, flyspell+jinx); (2) one genuinely dangerous bug — a `cond` form inside a `:custom` block in `lang-python.mode`; (3) a broken eglot activation path (`derived-mode-p 'python-mode` fails in `python-ts-mode`, so `h7/python-use-eglot` errors in exactly the buffers it targets); (4) non-deterministic notebook detection (substring `"notebooks"`); and (5) a large tail of disabled/commented blocks from the 1990s–2000s and five files that `site-boot.el` never loads, two of which (`site-init.el`) contain live-looking duplicate keybindings.

Highest priorities: ENH-01 (broken `:custom`), ENH-02 (LSP router), ENH-06/ENH-07 (broken bindings and typos), DUP-02 (three packages claiming `.md`), ENH-08 (startup hygiene).

### Summary Table

| ID | Status | Risk | Priority |
|----|--------|------|----------|
| OBS-01 | obsolete | Low | P2 |
| OBS-02 | obsolete/unused | Low | P3 |
| OBS-03 | obsolete (built-in exists) | Low | P3 |
| OBS-04 | duplicate | Low | P3 |
| OBS-05 | obsolete, needs verification | Low | P3 |
| OBS-06 | unused | Low | P3 |
| OBS-07 | unused (commented) | Low | P3 |
| OBS-08 | unused (:disabled) | Low | P3 |
| OBS-09 | unused (:disabled) | Low | P3 |
| OBS-10 | unused | Low | P3 |
| OBS-11 | unused | Low | P3 |
| OBS-12 | unused / needs verification | Low | P3 |
| OBS-13 | unused, needs verification | Low | P3 |
| OBS-14 | dead files | Medium | P2 |
| OBS-15 | obsolete candidate | Low | P3 |
| OBS-16 | unused duplicate client | Low | P2 |
| DUP-01 | conflicting | Medium | P1 |
| DUP-02 | conflicting | Medium | P1 |
| DUP-03 | duplicate | Low | P2 |
| DUP-04 | duplicate | Low | P2 |
| DUP-05 | duplicate | Low | P2 |
| DUP-06 | conflicting | Medium | P1 |
| DUP-07 | duplicate | Low | P3 |
| DUP-08 | conflicting + dormant | Medium | P2 |
| DUP-09 | duplicate declarations | Low | P2 |
| ENH-01 | bug | High | P1 |
| ENH-02 | bug/design | High | P1 |
| ENH-03 | cleanup | Low | P1 |
| ENH-04 | cleanup | Medium | P1 |
| ENH-05 | conflict fix | Low | P1 |
| ENH-06 | bug fixes | Medium | P1 |
| ENH-07 | bug fixes | Medium | P1 |
| ENH-08 | startup | Low | P2 |
| ENH-09 | portability | Low | P2 |
| ENH-10 | ergonomics | Low | P2 |
| ENH-11 | decision point | Medium | P2 |
| ENH-12 | consistency | Medium | P2 |
| ENH-13 | consistency | Low | P3 |
| ENH-14 | needs verification | Low | P3 |
| ENH-15 | enhancement | Low | P3 |
| ENH-16 | bug | Low | P2 |
| DEF-01 | deferred | — | — |
| DEF-02 | deferred | — | — |

### Obsolete Features/k

#### OBS-01: undo-tree still active

- Location: org heading "Utils/Misc", `use-package undo-tree`
- Status: obsolete
- Evidence: `(global-undo-tree-mode)` is enabled; Emacs 28+ ships `undo-redo` and `undo-redo-only`, and the file already targets Emacs 32
- Recommendation: remove undo-tree; rely on built-in `undo`/`undo-redo`. Keep the `user-cache-directory` undo history directory removal as cleanup
- Risk: Low; Effort: Low; Confidence: High (see [^ak-undo])

#### OBS-02: ace-jump-mode installed; avy installed but never bound

- Location: org heading "Utils/Jump", `use-package ace-jump-mode`, `use-package avy`
- Status: obsolete and unused
- Evidence: ace-jump-mode is superseded by avy; its only binding is commented out. avy has no `:bind` and no `avy-*` call anywhere in the file
- Recommendation: delete ace-jump-mode; either bind avy (e.g. `M-g w` → `avy-goto-word-1`) or delete it too
- Risk: Low; Effort: Low; Confidence: High (see [^ak-avy])

#### OBS-03: which-key installed from MELPA although bundled

- Location: org heading "Utils/Help", `use-package which-key`
- Status: obsolete (built-in supersedes)
- Evidence: `:ensure t` for which-key; which-key has been bundled with Emacs since version 30, so on this Emacs 32.0.50 snap it is built-in
- Recommendation: change to `:ensure nil` (or drop `:ensure`) to use the bundled copy
- Risk: Low; Effort: Low; Confidence: High (see [^ak-whichkey])

#### OBS-04: diminish and delight both loaded

- Location: org headings "Starting up/Use-Package" (`diminish`) and "UI" (`delight`, plus `(require 'delight)`)
- Status: duplicate
- Evidence: both modeline-hiding libraries are required; `undo-tree` uses `:diminish`, `which-key` uses `:delight`, `recentf` uses `:diminish`
- Recommendation: keep delight (used by which-key), convert the three `:diminish` uses to `:delight`, delete diminish
- Risk: Low; Effort: Low; Confidence: High

#### OBS-05: esh-autosuggest possibly archived, overlapping capf-autosuggest

- Location: org heading "Completion at-point/Capf", `use-package esh-autosuggest`
- Status: obsolete candidate, needs verification
- Evidence: esh-autosuggest's upstream repository was archived by its author (needs verification; Low confidence); capf-autosuggest is the cape-based successor and is already installed
- Recommendation: if archived status confirmed, remove esh-autosuggest and keep capf-autosuggest (eshell analysis deferred, see DEF-01)
- Risk: Low; Effort: Low; Confidence: Low

#### OBS-06: websocket and simple-httpd installed with no consumer

- Location: org heading "Server/Server Sockets"
- Status: unused
- Evidence: both packages are installed with no configuration; their only plausible consumer in this file is `org-roam-ui` (`:disabled t`)
- Recommendation: remove both
- Risk: Low; Effort: Low; Confidence: High

#### OBS-07: package bootstrap graveyard in "Starting up"

- Location: org heading "Starting up/Use-Package"
- Status: unused (commented out)
- Evidence: large commented blocks for straight.el, quelpa, marmalade, melpa-stable, `req-package` (also active-but-`:disabled t`), and multiple bootstrap variants
- Recommendation: delete the commented blocks; delete the `req-package` form (obsolete package manager)
- Risk: Low; Effort: Low; Confidence: High

#### OBS-08: disabled-package graveyard

- Location: passim (Ido, Auto-Complete, Elpy, EIN, Ensime/Scala, Puppet, Idris, Ranger, Sunrise Commander, w3m, Twittering, Pocket, gptel + gptel-ask, openai/chatgpt/codegpt/dall-e, chatgpt-shell, org-ref family, org-roam family, ox-ipynb, savefold, vimish-fold, fontaine, Powershell, projector, paperless, saveplace-pdf-view, git-gutter-fringe, vdiff-magit, poly-R, yasnippet, ack, ess smart-equals/smart-underscore)
- Status: unused (:disabled t or commented)
- Evidence: every form listed carries `:disabled t`, `:disabled`, or is inside comment blocks; several (org-ref) even carry the note `;; deps: helm`, and helm is not installed anywhere in the file
- Recommendation: delete these blocks outright; they remain in git history. Note the AI section is entirely disabled while the use-case list says AI tooling is being evaluated — keep at most one disabled gptel block as the evaluation placeholder
- Risk: Low; Effort: Medium (volume); Confidence: High

#### OBS-09: gtags and magit-zzz dead blocks reference dead packages

- Location: org headings "Workspace/Project" (`use-package gtags :disabled t`) and "Workspace/Magit" (`use-package magit-zzz :disabled t`)
- Status: unused (:disabled), with stale references
- Evidence: gtags block configures `helm-gtags`, but helm is not installed; magit-zzz references `magit-backup` and `magit-review`, the latter long gone from MELPA (needs verification; Low confidence)
- Recommendation: delete both blocks
- Risk: Low; Effort: Low; Confidence: Medium

#### OBS-10: regex-tool, google-this, ztree installed but never used

- Location: org headings "Utils/Search" (regex-tool), "Net/Web/search" (google-this), "Workspace/Dired" (ztree)
- Status: unused
- Evidence: regex-tool is `:defer t` with no binding; google-this has all bindings commented; ztree is bare `:ensure t` with no key and no hook (dired already covers directory comparison via `dired-diff` family)
- Recommendation: remove all three
- Risk: Low; Effort: Low; Confidence: High

#### OBS-11: haskell-mode and flycheck-haskell outside stated use cases

- Location: org heading "Lang/Haskell"
- Status: unused; maintenance status needs verification
- Evidence: neither Haskell nor Idris appears in the stated use cases (Python/R/Julia, docs, DevOps); haskell-mode upstream activity has been minimal for years (archived status needs verification; Low confidence)
- Recommendation: remove `haskell-mode` and `flycheck-haskell`; if Haskell returns, prefer a tree-sitter mode
- Risk: Low; Effort: Low; Confidence: Medium

#### OBS-12: drools autoload block likely broken

- Location: org heading "Lang/Drools"
- Status: unused; needs verification
- Evidence: `(autoload 'drools-mode "drools-mode")` assumes a file on `load-path`, but no drools package is installed by this file, and Drools is not in the use cases
- Recommendation: remove the block; if Drools editing is needed, verify a source first (Low confidence)
- Risk: Low; Effort: Low; Confidence: Low

#### OBS-13: legacy files not loaded by site-boot.el

- Location: `~/.emacs-site/` (from the configuration layout; keymap scan corroborates)
- Status: unused / dead
- Evidence: `site-boot.el` loads only site-pref, site-conf, local, site-pkgs, site-func, site-cust, site-misc, site-edit, site-view, site-keys, site-tool, site-user. `site-load.el` is the pre-24.3 legacy branch (all keybindings commented, per scan). `site-init.el` is _not_ loaded but contains uncommented bindings duplicating site-keys (`[C-mouse-4]`, `M-i` → `ido-goto-symbol`, `C-.` → `ido-find-file-in-tag-files`, `C-f` → `occur`) — a trap if it is ever required
- Recommendation: delete `site-load.el`, `site-init.el`, `site-attic.org`, `site-config.org`, `site-local.el` (all recoverable from git). Keep `site-custom.el` only if it is the target of `custom-file` — not verifiable from the provided files
- Risk: Medium (site-init.el could resurrect ido bindings if loaded); Effort: Low; Confidence: High

#### OBS-14: all-the-icons in maintenance mode

- Location: org heading "UI", `use-package all-the-icons`; also `all-the-icons-completion`, `treemacs-all-the-icons`, citar indicator using `all-the-icons-material`
- Status: healthy but obsolescence candidate
- Evidence: all-the-icons is mature/low-churn; nerd-icons is the actively maintained successor and is what newer packages (e.g. dashboard) prefer
- Recommendation: no action now (migration touches dashboard, treemacs theme, citar); plan a nerd-icons migration when touching those areas
- Risk: Low; Effort: High; Confidence: Medium (see [^ak-nerdicons])

#### OBS-15: lsp-julia installed while eglot-jl is the active Julia client

- Location: org heading "Lang/Julia", `use-package lsp-julia`
- Status: unused duplicate client
- Evidence: `eglot-jl` is active (`:hook (julia-mode . eglot-ensure)`); the lsp-julia hook is commented out and `lsp-julia` is never started
- Recommendation: remove lsp-julia; document Julia = eglot
- Risk: Low; Effort: Low; Confidence: High (see [^ak-eglotjl])

### Duplicated or Conflicting Features /k

#### DUP-01: company stack coexists with corfu/cape

- Location: org heading "Completion at-point/Company" (`use-package company`, `use-package company-posframe`); backends in "TeX" (`company-auctex`), "OP/Ansible" (`company-ansible`), "OP/Terraform" (`company-terraform`)
- Status: duplicate and conflicting
- Evidence: corfu is the in-buffer UI (`global-corfu-mode`), yet company is installed and `company-posframe-mode` is enabled in `:init` although `(global-company-mode nil)` — a popup UI for a backend framework that is never turned on. The three backend packages configure `company-backends` for company
- Recommendation: remove `company`, `company-posframe`, `company-ansible` (ansible-ls already installed via `h7/lsp-setup`), `company-terraform` (see ENH-15), and `company-auctex` (cdlatex + citar-capf cover the main needs; LaTeX completion can be re-added via cape if missed)
- Risk: Medium (LaTeX/Ansible completion nicety); Effort: Low; Confidence: High (see [^ak-corfu], [^ak-company])

#### DUP-02: three major modes claim `.md` files

- Location: org headings "Text/Markdown" (`markdown-mode` with `:mode ("\\.md\\'" . gfm-mode)`, `markdown-ts-mode` with `:mode ("\\.md\\'" . markdown-ts-mode)`) and "Lang/R/polymode" (`poly-markdown` with `:mode ("\\.md" . poly-markdown-mode)`)
- Status: conflicting
- Evidence: `use-package :mode` prepends to `auto-mode-alist`; the last one evaluated wins, so `.md` files actually open in `poly-markdown-mode` (a non-tree-sitter mode), while hl-todo, pandoc-mode and flyspell hooks target `markdown-mode`/`markdown-ts-mode`. The `poly-markdown` regexp also lacks a `\\'` anchor
- Recommendation: decide once — recommended: `markdown-ts-mode` for `.md` (matches the treesitter investment elsewhere), keep `markdown-mode` only if gfm-specific export is required, and restrict poly-markdown to `.Rmd`/`.rmd`. Apply in ENH-03
- Risk: Medium (behaviour change for `.md`); Effort: Low; Confidence: High (see [^ak-polymode])

#### DUP-03: flyspell and jinx both installed

- Location: org heading "TeX/Spell"
- Status: duplicate
- Evidence: flyspell is active (`turn-on-flyspell` in markdown modes, `flyspell-prog-mode` in prog modes, hunspell `en_US,it_IT`); jinx is installed with keys but `global-jinx-mode` commented out
- Recommendation: pick jinx (faster, no hunspell process, better capf integration) and remove flyspell, or delete jinx. One spellchecker only
- Risk: Low; Effort: Low; Confidence: High (see [^ak-jinx])

#### DUP-04: three terminal emulators active

- Location: org heading "Shell/vterm" (`vterm`, `multi-vterm`, `ghostel` and its eshell/compile/comint integration blocks)
- Status: duplicate
- Evidence: vterm (`C-<F9>` via site-keys), multi-vterm, and ghostel (`C-x m`, project integration) are all installed and bound; keys differ so nothing fights, but three terminal stacks triple the maintenance and build surface (vterm compiles a module)
- Recommendation: keep ghostel (Wayland/GTK friendly) + vterm as fallback on hosts where the ghostel module cannot build; drop `multi-vterm` (ghostel/vterm both cover multi-terminal). Final choice is a user decision
- Risk: Low; Effort: Low; Confidence: Medium

#### DUP-05: five grep/ag front-ends

- Location: org heading "Workspace/Grep" (`ag`, `rg`, `deadgrep`, `grep` with custom commands) plus consult's `M-s r`/`M-s g`
- Status: duplicate
- Evidence: ag, rg.el, deadgrep, built-in grep, and consult-ripgrep all present; the toolchain provides `rg` on PATH, not the silver searcher
- Recommendation: standardise on consult-ripgrep (`M-s r`) and consult-grep; remove `ag` (and its misspelled customs, see ENH-07), keep `rg`/`deadgrep` only if their extra UIs are genuinely used — otherwise remove
- Risk: Low; Effort: Low; Confidence: High (see [^ak-consult])

#### DUP-06: flycheck vs flymake across the two LSP clients

- Location: org heading "Lang: LSP.mode" (`flycheck` with `(global-flycheck-mode 1)`, `lsp-mode` with `lsp-diagnostics-provider :flycheck`) and "Lang: Python/lsp" (`eglot`)
- Status: conflicting
- Evidence: lsp-mode reports through flycheck; eglot reports through flymake. Global flycheck-mode is also active in eglot-managed buffers where it has no checker, producing noise and two diagnostic UIs on `M-g f` (`consult-flymake` vs `consult-lsp-diagnostics`)
- Recommendation: keep flycheck only where lsp-mode owns the buffer: add an `eglot-managed-mode-hook` that does `(flycheck-mode -1)` (ENH-05); keep `consult-flymake` for eglot buffers and `consult-lsp` for lsp-mode buffers
- Risk: Low; Effort: Low; Confidence: High (see [^ak-flycheck], [^ak-eglotman])

#### DUP-07: restclient and verb overlap

- Location: org heading "REST/Request"
- Status: duplicate
- Evidence: verb is bound into org (`:bind-keymap ("C-c C-r" . verb-command-map)`) and restclient claims `.http`; restclient-jq also loaded
- Recommendation: keep verb (org-native, already integrated); remove restclient/restclient-jq (and the disabled ob-restclient)
- Risk: Low; Effort: Low; Confidence: High (see [^ak-verb])

#### DUP-08: projectile and project.el both half-used

- Location: org heading "Workspace/Project" (`projectile`, `consult-projectile`), "Jump/Hydra" `hydra-projects` (both a `p` projectile column and a `P` project.el column), "Workspace/Dired" `treemacs-projectile`, "Jump/Dashboard" (`dashboard-projects-backend 'projectile`)
- Status: conflicting, and partially dormant
- Evidence: projectile is configured (`:config` with cache paths) but `projectile-mode` is never enabled — the commented `;;(projectile-global-mode)` is the only trace. Yet dashboard, hydras, `consult-projectile`, `python-behave--project-root`, and `eshell-toggle-find-project-root-package t` all assume a working projectile. Meanwhile project.el commands are bound side-by-side in the same hydra
- Recommendation: see ENH-11 (short-term enable `projectile-mode`; long-term migrate to project.el per the target design)
- Risk: Medium (dashboard "projects" section is silently empty today); Effort: Low; Confidence: High (see [^ak-projectile])

#### DUP-09: repeated and double declarations

- Location: "Completion prompt/Consult" and "Vertico*/Savehist" (`completion-ignore-case` twice, `(recentf-mode)` twice); "Org/Org-mode" (`org-CUA-compatible`, `org-support-shift-select`, `org-replace-disputed-keys` set in both `:init` and `:config`); "Lang: LSP.mode" `eglot` (`eglot-events-buffer-size` twice); "Workspace/Dired" (`ls-lisp-use-insert-directory-program`, `ls-lisp-dirs-first`, `ls-lisp-ignore-case` in both `ls-lisp` and `dired`); "UI" `hl-todo` `:config` calls `(minions-mode 1)` again; "Org/Org-mode" declares `ox-pandoc` twice (once `:disabled`, once active)
- Status: duplicate
- Evidence: as listed; harmless but noisy, and the ox-pandoc double declaration is confusing
- Recommendation: keep one declaration per variable; keep only the active `ox-pandoc` block
- Risk: Low; Effort: Low; Confidence: High

### LSP/Eglot Coexistence Design

Both clients are required: lsp-mode for ordinary Python, eglot for `# %%` notebooks. The file already contains the right _parts_ — a buffer-local flag `h7/python-use-eglot`, a switch function `h7/python-lsp-switch`, and guard lambdas on the lsp-mode hooks — but the activation path has four defects:

1. Broken guard: `h7/python-lsp-switch` checks `(derived-mode-p 'python-mode)`. Since Emacs 29, `python-ts-mode` derives from `python-base-mode`, not `python-mode`, so the guard signals `user-error` in exactly the `python-ts-mode` notebook buffers the feature targets (needs verification against your exact build; High confidence on the parent-mode change).
2. Non-deterministic detection: the auto hook matches the substring `"notebooks"` anywhere in the path — it fires for `notebooks-backup/`, any file named `*notebooks*`, and non-notebook files inside such directories, and never checks the `# %%` marker that defines the actual format.
3. Ineffective `remove-hook` calls: removing a hook that was added as an anonymous lambda by `use-package :hook` cannot match (lambda identity); those two lines are no-ops.
4. Eglot is registered for `python-ts-mode` only; a notebook opened in `python-mode` has no server program.

The replacement design (ENH-02) makes the decision deterministic: a single router on `python-base-mode-hook`, driven by (a) an explicit buffer-local/directory-local flag, (b) the `notebooks/` directory component, or (c) a `# %%` marker scan in the first 200 lines. Interaction matrix after the fix:

- Diagnostics: flycheck in lsp-mode buffers (unchanged), flymake in eglot buffers; `consult-lsp` vs `consult-flymake` chosen by client; no checker duplication after ENH-05.
- Completion-at-point: both clients provide a CAPF; `C-SPC` (site-keys) and corfu work identically; cape remains the extension point.
- Formatting: `ruff-format-on-save-mode` stays lsp-mode-only; notebooks rely on eglot (ruff is in the eglot server set), optionally `eglot-format-on-save`.
- Eldoc: lsp-ui-doc (lsp-mode) vs `eldoc-doc-buffer` on `C-h .` (eglot) — unchanged, matches the REFS table.
- Xref: both clients plug into xref; the global `consult-xref` configuration serves both.
- Key prefixes: see ENH-10 (unify under `C-l`).

R and Julia: Julia = eglot via eglot-jl (active; remove lsp-julia, OBS-15). R has _no_ LSP client configured — ESS only. Optional: eglot for R using the REditorSupport language server, consistent with the Julia choice (ENH-15, needs verification of server availability inside renv containers).

### Cleanup and Enhancement Changes /k

Ordered by priority. Each snippet is valid Emacs Lisp, states its target org heading, and whether it replaces or adds code.

#### ENH-01: `cond` inside `:custom` in the python block

- Location: org heading "Lang: Python/mode", `use-package python`
- Status: bug
- Evidence: `:custom` receives `(cond ((executable-find "ipython") ...))`; `:custom` expects `(VAR VALUE DOC?)` triples, so the interpreter-selection cond is malformed and the IPython preference silently does not take effect (use-package may also error at expansion — needs verification for your use-package version)
- Change (replaces the `:custom` block; the interpreter selection moves to `:config`):

```elisp
(use-package python
  :ensure nil
  :hook
  ((python-mode . (lambda () (setq-local h7/python-use-eglot nil)))
   (python-ts-mode . (lambda () (setq-local h7/python-use-eglot nil))))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (treesit-font-lock-level 4)
  :config
  (cond
   ((executable-find "ipython")
    (setq python-shell-buffer-name "IPython"
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))
```

- Rollback: revert to the previous block from git; nothing else depends on it
- Risk: High if unfixed (IPython REPL selection broken); Effort: Low; Confidence: High

#### ENH-02: deterministic single-client router for Python

- Location: org headings "Lang: LSP.mode" (lsp-mode `:hook`) and "Lang: Python/lsp" (trailing notebook hook)
- Status: bug/design
- Evidence: see the four defects in the LSP design section above; also `(string-match-p "notebooks" (buffer-file-name))` matches any path segment
- Change (replaces the two python entries in `lsp-mode :hook` and the `add-hook 'python-ts-mode-hook` "auto eglot" block; keep the `h7/python-use-eglot` defvar, `h7/python-lsp-switch`, and the manual `C-c l e`/`C-c l l` bindings):

```elisp
(defun h7/python-notebook-p ()
  "Return t when the current buffer is a percent-format notebook.
Notebooks live under a notebooks/ directory and carry # %% cells."
  (and buffer-file-name
       (or (string-match-p "/notebooks/\\|/notebooks\\'" buffer-file-name)
           (save-restriction
             (widen)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^# %%" nil t 200))))))

(defun h7/python-lsp-ensure ()
  "Start exactly one Python LSP client for the current buffer."
  (if (or (bound-and-true-p h7/python-use-eglot)
          (h7/python-notebook-p))
      (h7/python-use-eglot)
    (lsp-deferred)))

(add-hook 'python-base-mode-hook #'h7/python-lsp-ensure)
```

- Also fix the guard inside `h7/python-lsp-switch` (replaces one line): change `(unless (derived-mode-p 'python-mode) ...)` to `(unless (derived-mode-p 'python-base-mode) ...)`.
- Optional directory-local override (adds a file, not elisp): a `notebooks/.dir-locals.el` containing `((python-ts-mode . ((h7/python-use-eglot . t))))` — note directory-locals are applied after mode hooks, so it only reinforces, never contradicts, the router
- Rollback: restore the two removed hook forms from git
- Risk: High if unfixed; Effort: Low; Confidence: High

#### ENH-03: give `.md` to one mode

- Location: org heading "Lang/R/polymode", `use-package poly-markdown`
- Status: cleanup (implements DUP-02)
- Evidence: poly-markdown currently wins `.md` by load order
- Change (replaces the `:mode` line; keep poly-R associations):

```elisp
(use-package poly-markdown
  :ensure t
  :mode ("\\.Rmd\\'" . poly-markdown+r-mode))
```

- And in "Text/Markdown" (replaces the markdown-mode `:mode` line): `:mode ("\\.markdown\\'" . gfm-mode)` so plain `.md` goes to `markdown-ts-mode` unambiguously
- Rollback: restore the two `:mode` lines
- Risk: Low; Effort: Low; Confidence: High

#### ENH-04: remove the company stack

- Location: org headings "Completion at-point/Company", "TeX", "OP/Ansible", "OP/Terraform"
- Status: cleanup (implements DUP-01)
- Evidence: company is never globally enabled; corfu is the target
- Change (deletes four `use-package` forms: `company`, `company-posframe`, `company-ansible`, `company-auctex`; in org heading "Org/Org-mode" `h7/org-mode-setup` also deletes the `company-backends` local setup and the `(add-to-list 'company-backends 'company-ispell)` line)
- Rollback: reinstall from git history if LaTeX/Ansible completion is missed; cape-based replacements can be added later
- Risk: Medium (loses LaTeX completion); Effort: Low; Confidence: High

#### ENH-05: keep flycheck out of eglot buffers

- Location: org heading "Lang: LSP.mode", `use-package flycheck`
- Status: conflict fix (implements DUP-06)
- Change (adds one form inside flycheck `:config`):

```elisp
(add-hook 'eglot-managed-mode-hook
          (lambda () (flycheck-mode -1)))
```

- Rollback: delete the added form
- Risk: Low; Effort: Trivial; Confidence: High

#### ENH-06: repair broken keybindings and void references

- Location: org heading "Jump/Hydra" (`hydra-fold`, `hydra-buffers`, `hydra-window`), "Net/News/elfeed", "Completion prompt/Consult"
- Status: bug
- Evidence: (a) `hydra-fold` and site-keys `f4` call `vimish-fold-*` but vimish-fold is `:disabled t`; (b) `hydra-buffers` offers `ido-switch-buffer` and `helm-buffers-list` — neither ido nor helm is installed; (c) `hydra-window` h/j/k/l call `buf-move-*` — buffer-move is not installed anywhere in the file; (d) elfeed binds `h7/efleed-show-eww-open`/`h7/efleed-search-eww-open` but the functions are named `h7/elfeed-*`; (e) consult binds `M-g r` to `consult-grep-match`, which does not exist in consult (needs verification; Low confidence — likely meant `consult-grep`)
- Change: re-point `hydra-fold` and `f4` at kirigami (installed) or re-enable vimish-fold — recommended minimal edit: in `hydra-fold`, replace the `vimish-fold` family with the already-loaded `kirigami` commands and delete the now-dead `C-c z` kirigami `:bind` to avoid a second prefix; in `hydra-buffers`, replace `ido-switch-buffer`/`helm-buffers-list` with `consult-buffer`; in `hydra-window`, replace `buf-move-*` with built-in `windmove-swap-states-left/up/down/right`; fix the two `efleed` typos; change `consult-grep-match` to `consult-grep` after verifying with `M-x describe-function`
- Rollback: restore hydra definitions from git
- Risk: Medium (hydras currently error at runtime); Effort: Medium; Confidence: High for (a)–(d), Low for (e)

#### ENH-07: fix malformed `:custom` entries and misspelled variables

- Location: org headings "Workspace/Dired" (`dired-preview`), "Workspace/Grep" (`wgrep`, `ag`), "Lang: Python/mode" (covered by ENH-01)
- Status: bug
- Evidence: `dired-preview` puts `(setq ...)` forms and a `(defun my-dired-preview-to-the-right ...)` inside `:custom` (misplaced; move to `:config`); `wgrep` sets `twgrep-change-readonly-file` (typo — real variable is `wgrep-change-readonly-file`) and a one-element `(wgrep-auto-save-buffer)`; `ag` sets `ag-highligh-search` (typo), `tag-reuse-buffers`, `tag-reuse-window` (both typos — real variables are `ag-reuse-buffers`, `ag-reuse-window`)
- Change (replaces the `:custom` blocks):

```elisp
(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-enable-key "e")
  (wgrep-change-readonly-file t))
```

- Rollback: restore from git
- Risk: Medium (settings silently ineffective today); Effort: Low; Confidence: High on typos, Medium on use-package strictness

#### ENH-08: startup hygiene

- Location: org headings "Starting up/Use-Package", "General/System", "General/Config"
- Status: enhancement
- Evidence: `(setq gc-cons-threshold (* 100 1024 1024))` is never lowered after init; `(package-initialize)` runs despite Emacs ≥ 27 doing it before init (the `warning-suppress-log-types` `package reinitialization` hack is the symptom of double initialization on the snap); `use-package-verbose t` costs time; `(setq internal--get-default-lexical-binding-function nil)` pokes an internal variable although every tangled file carries a `lexical-binding` cookie
- Change (replaces the three lines; add to "General/System"):

```elisp
;; tune gc during init, restore a sane value afterwards
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
```

- Delete `(package-initialize)` together with the `warning-suppress-log-types` workaround, set `use-package-verbose nil` once this audit is done, and delete the `internal--get-default-lexical-binding-function` line
- Rollback: restore the three lines; startup only gets slower
- Risk: Low; Effort: Low; Confidence: Medium-High (double `package-initialize` on the alexmurray snap is the documented cause of the reinitialization warning)

#### ENH-09: platform guards for GUI-only settings

- Location: org headings "General/Config" (`x-gtk-use-native-input`), "Fonts/Ligatures" (`global-ligature-mode`)
- Status: enhancement
- Evidence: `x-gtk-use-native-input` is set unconditionally although the config also runs on WSL, containers and non-GTK builds; `global-ligature-mode` composes display properties that do not exist on tty frames
- Change (replaces the two settings):

```elisp
(when (featurep 'gtk)
  (setq x-gtk-use-native-input t))
```

```elisp
(when (display-graphic-p)
  (global-ligature-mode t))
```

- Rollback: restore unconditional settings
- Risk: Low; Effort: Trivial; Confidence: High

#### ENH-10: unify LSP key prefixes and align with the REFS key table

- Location: org headings "Lang: Python/lsp" (`eglot` `:bind` uses `C-c c`), "Lang: LSP.mode" (`lsp-keymap-prefix "C-l"`), "Utils/Misc" (`recentf` binds `C-x C-r`), REFS "GLOBAL" table
- Status: ergonomics
- Evidence: REFS documents `C-x C-r` → `consult-recent-file` but the config binds `recentf-open-files`; eglot actions live on `C-c c` while lsp-mode lives on `C-l`
- Change: drop the `recentf` `:bind` (consult covers recent files via `consult-recent-file` on `C-x C-r`); change eglot `:bind` to `("C-l a" . eglot-code-actions)` and `("C-l r" . eglot-rename)`; keep `C-c l e`/`C-c l l` for the manual switch
- Rollback: restore bindings
- Risk: Low; Effort: Low; Confidence: High

#### ENH-11: resolve the projectile/project.el decision

- Location: org heading "Workspace/Project"
- Status: decision point (implements DUP-08)
- Evidence: `projectile-mode` never enabled, yet dashboard/hydras/consult-projectile depend on it
- Change, short term (adds one line to the projectile `:config`): `(projectile-mode 1)`. Long term (per target design): migrate hydra-projects to the `P` column only, switch `dashboard-projects-backend` away from projectile, drop `consult-projectile`/`treemacs-projectile`, and remove projectile
- Rollback: comment the added line; long-term path requires re-learning project keys
- Risk: Medium; Effort: Low now / Medium later; Confidence: High (see [^ak-project])

#### ENH-12: align the eglot Python server program with the documented design

- Location: org heading "Lang: Python/lsp", `use-package eglot :config`
- Status: consistency
- Evidence: comments promise `basedpyright-langserver --stdio` + `ruff server` as a multi-server spec, but the active entry is `(python-ts-mode . ("rass" "--" "ty" "server" "--" "ruff" "server"))` — a single command through the `rass` multiplexer running _ty_, not basedpyright — and `eglot-workspace-configuration` still sets a `:basedpyright` key that ty/ruff will ignore. Also eglot has no `python-mode` entry
- Change (replaces the `add-to-list 'eglot-server-programs` form; the exact rass argument order needs verification — Low confidence):

```elisp
(dolist (mode '(python-mode python-ts-mode))
  (setf (alist-get mode eglot-server-programs)
        '("rass" "--" "basedpyright-langserver" "--stdio"
          "--" "ruff" "server")))
```

- Alternatively drop rass and register basedpyright alone (ruff already formats via `ruff-format`). Either way, delete the `:basedpyright` workspace configuration or keep it only if basedpyright is the registered server
- Rollback: restore the single `add-to-list` from git
- Risk: Medium; Effort: Low; Confidence: Medium

#### ENH-13: point the markdown tree-sitter grammar at the split parser

- Location: org heading "Lang: Treesitter/setup", `h7/treesitter-setup-sources`
- Status: consistency
- Evidence: `treesit-language-source-alist` maps markdown to `ikatyang/tree-sitter-markdown` (monolithic, unmaintained), while `markdown-ts-mode` requires the split `markdown` + `markdown-inline` grammars — the correct URLs already exist, commented, in the `markdown-ts-mode` block
- Change (replaces the markdown entry):

```elisp
(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
          "split_parser" "tree-sitter-markdown/src")
(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 "split_parser" "tree-sitter-markdown-inline/src")
```

- Rollback: restore the ikatyang entry; then `markdown-ts-mode` silently lacks folding
- Risk: Low; Effort: Low; Confidence: High (see [^ak-tsmarkdown])

#### ENH-14: actually wire markdownlint-cli2 into flycheck

- Location: org heading "Text/Markdown", `with-eval-after-load 'flycheck` inside `markdown-mode`
- Status: needs verification
- Evidence: the code sets `flycheck-markdown-markdownlint-cli-executable` to `markdownlint-cli2` and registers mode `markdown-markdownlint-cli` for `markdown-ts-mode`; if flycheck's cli2 checker is named `markdown-markdownlint-cli2` with executable variable `flycheck-markdown-markdownlint-cli2-executable` (needs verification; Low confidence), the current settings target the wrong checker and markdownlint-cli2 never runs
- Change: after verifying with `M-x describe-variable flycheck-checkers`, set the cli2-specific executable variable and add `markdown-ts-mode` to the cli2 checker's modes; otherwise install `markdownlint-cli2` support via the checker definition
- Rollback: restore the two lines
- Risk: Low (linting silently absent today); Effort: Low; Confidence: Low

#### ENH-15: language-client tidy-up (R and Julia)

- Location: org headings "Lang/Julia" (delete `lsp-julia`), optionally add an R block near "Lang/R/ess"
- Status: enhancement
- Evidence: Julia already standardises on eglot; R has no client. Toolchain notes R exists only inside Podman containers, so a server there needs verification
- Change (adds, only if wanted; eglot is already loaded):

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(ess-r-mode . ("R" "--slave" "-e"
                               "languageserver::run()"))))
```

- Rollback: delete the block; remove lsp-julia from git history if needed
- Risk: Low; Effort: Low; Confidence: Medium (see [^ak-rls])

#### ENH-16: dashboard projects backend

- Location: org heading "Jump/Dashboard"
- Status: bug
- Evidence: `dashboard-projects-backend 'projectile` while `projectile-mode` is off, so the projects section is empty; also the `:custom` entry `(initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))` can return an empty buffer if it runs before `dashboard-setup-startup-hook`
- Change: after ENH-11 short-term, the backend works as-is; otherwise switch the backend to the project.el option (exact symbol needs verification against your dashboard version; Low confidence). Simpler: drop the custom `initial-buffer-choice` and let `dashboard-setup-startup-hook` manage the startup buffer
- Rollback: restore
- Risk: Low; Effort: Low; Confidence: Medium

### Deferred Items

#### DEF-01: eshell block

- Location: org heading "Shell/eshell" and adjacent headings
- Status: deferred (out of scope per instructions)
- Evidence (listed without analysis): the eshell `:preface` runs `(setq-local completion-styles ...)` and `(corfu-mode)` at load time in whatever buffer is current; `(add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))` adds `t` as a hook function; `M-r` is bound twice identically; `eshell-vterm`, `eat-eshell-visual-command-mode`, and `ghostel-eshell-visual-command-mode` are three overlapping visual-command backends; `capf-autosuggest` and `esh-autosuggest` overlap (see OBS-05)
- Recommendation: none in this pass

#### DEF-02: tramp-related configuration

- Location: org heading "VM/Docker" (commented `docker-tramp`, `tramp-container`), "Shell/eshell" (`eshell-there`, `ha/eshell-host-regexp`)
- Status: deferred (out of scope per instructions)
- Evidence (listed without analysis): tramp-container and docker-tramp are commented out; eshell/tramp helper functions are unverifiable from the provided files
- Recommendation: none in this pass

## Additional Notes /k

- The pasted file ends with `Wrong type argument: integer-or-marker-p, nil`. This cannot be attributed from the provided files. Most plausible candidates given this audit: the malformed `:custom` entries (ENH-01, ENH-07), the `hydra-of-hydras`/`hydra-*` definitions, or dashboard startup. Diagnose with `emacs --debug-init` and, if needed, bisect by tangling halves of `site-pkgs.org`.
- Load-order hazard: `site-boot.el` loads `site-pkgs` _before_ `site-func` and `site-keys`, yet `site-pkgs` calls at load time `h7/use-vertico` (twice, in "Vertico*/begin/end"), `h7/use-pdf-tools`, `h7/use-py-jupyter`, `h7/var-*`, `emacs-d`, `ha-leader`, and references `joe-*`, `scroll-down-one`, `xah-*`, `toggle-*`. These must be defined in `site-pref`/`site-conf` to be safe — not verifiable from the provided files. Consider moving all `h7/*` helper definitions into the "General" section of this file so the dependency direction is self-contained.
- `:no-require` appears in `use-package citar` and `use-package citar-embark`; it is not a standard use-package keyword (needs verification how your build treats unknown keywords — ignored or error). Replace with `:defer t` if in doubt.
- `(setq org-element-use-cache nil)` disables org-element caching globally for an imenu issue that is likely stale on Emacs 32 — re-test with it removed; org startup will get noticeably faster on large files.
- `server-use-tcp t` with a fixed port 60321 is fine on one machine but will collide if two Emacsen start on the same host (notably WSL + Windows, or Podman containers sharing the network namespace). Consider a per-UID port or unix sockets where available.
- The REFS "GLOBAL" table documents intended bindings; after ENH-06/ENH-10 the live config matches it (modulo eshell, deferred).

## Appendix: Package Inventory /k

Status legend: A = active, D = :disabled/commented, U = active but unused/unbound.

| Package | Status | Verdict |
|---------|--------|---------|
| bind-key | A | keep (bundled with use-package) |
| diminish | A | remove (OBS-04) |
| delight | A | keep |
| use-package-ensure-system-package | A | keep |
| general | A | keep (bindings depend on it) |
| req-package | D | delete (OBS-07) |
| zoom-window | A | keep |
| ef-themes | A | keep |
| minions | A | keep |
| mood-line | A | keep |
| all-the-icons | A | keep for now (OBS-14) |
| solaire-mode | A | keep |
| hl-todo | A | keep (drop re-minions call, DUP-09) |
| dash / f / s | A | keep (used) |
| hydra / use-package-hydra | A | keep (repair bodies, ENH-06) |
| ace-jump-mode | A | remove (OBS-02) |
| avy | A | bind or remove (OBS-02) |
| regex-tool | U | remove (OBS-10) |
| visual-regexp | A | keep (verify `ha-leader` exists) |
| which-key | A | :ensure nil (OBS-03) |
| helpful | A | keep |
| undo-tree | A | remove (OBS-01) |
| recentf | A | keep (drop :bind, ENH-10) |
| popper | A | keep (fix eldoc matcher, ENH-06) |
| comint | A | keep |
| restart-emacs | A | keep |
| visible-mark | A | keep (niche) |
| hilit-chg / goto-chg | A | keep |
| outline-indent | A | keep |
| treesit-fold | A | keep |
| kirigami | A | keep (receives hydra-fold keys) |
| savefold | D | delete (OBS-08) |
| vimish-fold | D | delete or re-enable (ENH-06) |
| calc | A | keep |
| crux | A | keep (dashboard uses it) |
| server | A | keep (note DEF/multi-instance) |
| websocket / simple-httpd | U | remove (OBS-06) |
| fontaine | D | delete (OBS-08) |
| ligature | A | keep, guard tty (ENH-09) |
| evil | A | unused as configured — decide (see note) |
| cape | A | keep |
| capf-autosuggest | A | keep |
| esh-autosuggest | A | remove if archived confirmed (OBS-05) |
| corfu | A | keep |
| company / company-posframe | A | remove (DUP-01/ENH-04) |
| auto-complete | D | delete (OBS-08) |
| ido (+hacks/flx/vertical) | D | delete (OBS-08) |
| marginalia | A | keep |
| all-the-icons-completion | A | keep (or nerd-icons later) |
| vertico | A | keep |
| consult | A | keep (fix `consult-grep-match`, ENH-06) |
| consult-dir / consult-todo | A | keep |
| orderless | A | keep |
| embark / embark-consult | A | keep |
| savehist | A | keep |
| olivetti | A | keep |
| magit | A | keep |
| magit-zzz | D | delete (OBS-09) |
| git-timemachine | A | keep |
| git-gutter-fringe | D | delete (OBS-08) |
| vdiff | A | keep |
| vdiff-magit | D | delete (OBS-08) |
| ls-lisp / dired / dired-x / find-dired | A | keep (dedupe vars, DUP-09) |
| fd-dired | A | keep |
| dired-preview | A | keep (fix :custom, ENH-07) |
| ztree | U | remove (OBS-10) |
| peep-dired | C | already commented — delete |
| projectile / consult-projectile | A | enable mode now, migrate later (ENH-11) |
| treemacs (+projectile/icons-dired/all-the-icons/magit) | A | decision (ENH-11 note); keep short-term |
| lsp-treemacs | A | keep only if treemacs stays |
| etags | A | keep (bound `M-T`) |
| gtags (+helm-gtags) | D | delete (OBS-09) |
| wgrep (+wgrep-ag) | A | keep (fix typos, ENH-07) |
| ag | A | remove (DUP-05) |
| rg / deadgrep | A | keep only if used (DUP-05) |
| grep | A | keep |
| ack | D | delete (OBS-08) |
| ghostel (+eshell/compile/comint) | A | keep (DUP-04) |
| vterm / multi-vterm | A | keep vterm; drop multi-vterm (DUP-04) |
| eshell and family | A | deferred (DEF-01) |
| eshell-toggle | A | deferred (DEF-01) |
| eshell-syntax-highlighting / xterm-color / eshell-vterm / pcmpl-args / eat | A | deferred (DEF-01) |
| proced | A | keep (custom filter is good) |
| projector | D | delete (OBS-08) |
| sh-script | A | keep |
| ranger / sh-toggle / multi-term | D | delete (OBS-08) |
| sunrise-commander | C | delete (OBS-08) |
| markdown-mode | A | keep, scope `.markdown` (ENH-03) |
| markdown-ts-mode | A | primary `.md` (ENH-03, ENH-13) |
| pandoc-mode | A | keep |
| add-node-modules-path | C | delete |
| css-mode / nxml-mode | A | keep (built-in) |
| web-mode / rainbow-mode | C | delete |
| flycheck | A | keep; eglot guard (ENH-05) |
| lsp-mode / lsp-ui / consult-lsp | A | keep (router change, ENH-02) |
| company-lsp | C | delete |
| dap-mode | A | keep |
| treesit-auto | A | keep (fix grammar source, ENH-13) |
| yasnippet | D | delete (lsp-enable-snippet nil) |
| google-c-style | A | keep |
| ess (+data-view/object-popup) | A | keep |
| polymode / poly-markdown | A | keep; scope to `.Rmd` (ENH-03) |
| poly-R / poly-ansible | D/A | poly-R delete; poly-ansible keep |
| python / inferior-python-mode / hide-mode-line | A | keep (fix :custom, ENH-01) |
| uv-mode / pet / pyvenv-auto / with-venv | A | keep |
| poetry / pyvenv | D/C | delete (comments say removed) |
| lsp-pyright | A | keep (basedpyright) |
| eglot | A | keep (fix guard + servers, ENH-02/12) |
| ruff-format | A | keep |
| python-pytest | A | keep |
| cov | A | keep |
| gherkin-mode | A | keep |
| elpy | D | delete (OBS-08) |
| ein | D | delete (OBS-08) |
| zmq / jupyter / code-cells | A | keep (jupyter conditional) |
| julia-mode / eglot-jl | A | keep (Julia = eglot) |
| lsp-julia | U | remove (OBS-15) |
| maxima | A | keep (org babel uses it) |
| ruby-mode | A | keep (built-in) |
| puppet-mode | D | delete |
| lua-mode | A | optional prune (unused) |
| ensime / scala-mode2 | D | delete (OBS-08) |
| haskell-mode / flycheck-haskell | A | remove (OBS-11) |
| idris-mode | D | delete |
| drools (autoload) | A | remove (OBS-12) |
| plantuml-mode | A | keep (org babel) |
| json-mode / json-reformat | A | keep (or move to json-ts later) |
| yaml-ts-mode / yaml-mode | A | keep |
| verb | A | keep |
| restclient / restclient-jq | A | remove (DUP-07) |
| ob-restclient / ob-http | D | delete |
| ansible / ansible-doc / jinja2-mode / poly-ansible | A | keep (drop company-ansible) |
| ansible-vault | C | delete |
| company-ansible | A | remove (DUP-01) |
| company-terraform | A | remove; add terraform-mode + terraform-ls |
| powershell | C | delete |
| docker / dockerfile-mode | A | keep |
| docker-tramp / tramp-container | C | deferred (DEF-02) |
| w3m | D | delete (OBS-08) |
| google-this | U | remove (OBS-10) |
| pocket-reader / twittering-mode | D | delete (OBS-08) |
| elfeed-org / elfeed | A | keep (fix efleed typo, ENH-06) |
| elfeed-goodies | D | delete |
| gptel / gptel-ask | D | keep at most one as placeholder |
| openai/chatgpt/codegpt/dall-e/chatgpt-shell/ob-chatgpt-shell | D | delete (OBS-08) |
| auctex / auctex-latexmk / cdlatex / tex | A | keep (drop company-auctex) |
| company-auctex | A | remove (DUP-01) |
| flyspell / jinx | A | pick one (DUP-03) |
| biblio | A | keep |
| nov | A | keep |
| pdf-tools / pdf-view-restore / pdfgrep / djvu | A | keep |
| saveplace-pdf-view / paperless | D | delete |
| org (+ built-in exporters) | A | keep (dedupe, DUP-09; re-test element cache) |
| ox-pandoc | A | keep single declaration (DUP-09) |
| org-autolist / org-present / org-ref family / org-roam family / ox-ipynb / ox-rss / ox-reveal / ob-http / org-brain / org-projectile / ox-tufte / org-static-blog | D | delete (OBS-08) |
| org-tree-slide / org-context / org-modern / ob-async | A | keep |
| denote / denote-menu / denote-refs | A | keep |
| org-noter / org-pdftools / org-noter-pdftools | A | keep |
| citar / citar-embark / citar-denote | A | keep (fix :no-require, Notes) |
| consult-notes | A | keep |
| dashboard | A | keep (fix backend, ENH-16) |
| engine-mode | A | keep |

## Footnotes /k

[^ak-undo]: GNU Emacs Manual, "Undo": <https://www.gnu.org/software/emacs/manual/html_node/emacs/Undo.html>
[^ak-avy]: avy repository: <https://github.com/abo-abo/avy>
[^ak-whichkey]: which-key on GNU ELPA: <https://elpa.gnu.org/packages/which-key.html>
[^ak-nerdicons]: nerd-icons.el repository: <https://github.com/rainstormstudio/nerd-icons.el>
[^ak-eglotjl]: eglot-jl repository: <https://github.com/non-Jedi/eglot-jl>
[^ak-corfu]: Corfu repository: <https://github.com/minad/corfu>
[^ak-company]: company-mode: <https://company-mode.github.io/>
[^ak-polymode]: polymode repository: <https://github.com/polymode/polymode>
[^ak-jinx]: Jinx repository: <https://github.com/minad/jinx>
[^ak-consult]: Consult repository: <https://github.com/minad/consult>
[^ak-flycheck]: Flycheck: <https://www.flycheck.org/>
[^ak-eglotman]: Eglot manual: <https://www.gnu.org/software/emacs/manual/html_mono/eglot.html>
[^ak-verb]: Verb repository: <https://github.com/federicotdn/verb>
[^ak-projectile]: Projectile repository: <https://github.com/bbatsov/projectile>
[^ak-project]: Emacs manual, "Projects": <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>
[^ak-tsmarkdown]: tree-sitter-markdown (split parser): <https://github.com/tree-sitter-grammars/tree-sitter-markdown>
[^ak-rls]: R language server: <https://github.com/REditorSupport/languageserver>

# A:1 (Vibe)

[^](#toc) **_TODO:(a1-ref-vibe)_**

TODO:(a1-vibe) ...

<!-- }}} \\ %1. -->
<!-- ::{{{ #TAG: TODO:(q2-section) // -->

# Q:2

## Q:2 - **TODO:(q2-title)**

[^](#toc)

## Question Prompt 2

TODO:(q1-prompt) ...


# A:2 (Claude)

[^](#toc) **_TODO:(a1-ref-claude)_**

TODO:(a2-claude) ...

# A:2 (Gemini)

[^](#toc) **_TODO:(a2-ref-gemini)_**

TODO:(a2-gemini) ...

# A:2 (ChatGPT)

[^](#toc) **_TODO:(a2-ref-chatgpt)_**

TODO:(a2-chatgpt) ...

# A:2 (Perplexity)

[^](#toc) **_TODO:(a2-ref-perplexity)_**

TODO:(a2-perplexity) ...

# A:2 (DeepSeek)

[^](#toc) **_TODO:(a2-ref-deepseek)_**

TODO:(a2-deepseek) ...

# A:2 (Kimi)

[^](#toc) **_TODO:(a2-ref-kimi)_**

TODO:(a2-kimi) ...

# A:2 (Vibe)

[^](#toc) **_TODO:(a2-ref-vibe)_**

TODO:(a2-vibe) ...

<!-- }}} \\ %2. -->
<!-- ::{{{ #TAG: TODO:(aa-section) // -->
<details>
<summary></summary>

```{=latex}
\newpage
\clearpage
\appendix
```

</details>

# A:a

## A:a - **TODO:(aa-title)**

[^](#toc)

## Appendix a

TODO:(aa-text) ...

<!-- }}} \\ %a. -->
<!-- {{{ // %*
LocalWords:  GitLab CommonMark GFM GLFM YAML
vim: set foldmethod=marker :
}}} // %* -->
