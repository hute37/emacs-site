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
<!-- markdownlint-disable MD025 -->
<!-- markdownlint-disable MD033 -->
<!-- markdownlint-disable MD051 -->


# TOC

1. [Q:1 - TODO:(q1-ref)](#q1)
   - see: [TODO:(a1-ref-claude) (Claude)](#a1-claude)
   - see: [TODO:(a1-ref-gemini) (Gemini)](#a1-gemini)
   - see: [TODO:(a1-ref-chatgpt) (ChatGPT)](#a1-chatgpt)
   - see: [TODO:(a1-ref-perplexity) (Perplexity)](#a1-perplexity)
   - see: [TODO:(a1-ref-deepseek) (DeepSeek)](#a1-deepseek)
2. [Q:2 - TODO:(q2-ref)](#q2)
   - see: [TODO:(a2-ref-claude) (Claude)](#a2-claude)
   - see: [TODO:(a2-ref-gemini) (Gemini)](#a2-gemini)
   - see: [TODO:(a2-ref-chatgpt) (ChatGPT)](#a2-chatgpt)
   - see: [TODO:(a2-ref-perplexity) (Perplexity)](#a2-perplexity)
   - see: [TODO:(a2-ref-deepseek) (DeepSeek)](#a2-deepseek)
3. [A:a - TODO:(appendix-a)](#aa)
4. [A:b - Q1: Prompt distiller](#ab)
   - see: [Q1: Prompt distiller (Claude)](#ab-claude)
   - see: [Q1: Prompt distiller (Gemini)](#ab-gemini)
   - see: [Q1: Prompt distiller (ChatGPT)](#ab-chatgpt)
   - see: [Q1: Prompt distiller (Perplexity)](#ab-perplexity)
   - see: [Q1: Prompt distiller (DeepSeek)](#ab-deepseek)

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

## Q:1 - **TODO:(q1-title)**

[^](#toc)

>>> [!tip]

## Role

TODO:(q1-role) ...

## Context

TODO:(q1-context) ...

>>>

## Objective

TODO:(q1-prompt) ...




## Output Format

- Reply in clear formatted "GitLab Flavored Markdown (GLFM)" Markdown,
with precise (lint) validation:
  - codeblock delimiters ``` placed atline start). Avoid codeblock nesting.
  - use _underscore markup_ for emphasys
  - prefer nested headings to text markup with asterisks
  - use only "dash" for unordered lists, with correct indentation
  - insert appropriate blank line separation after headings, list and codeblocks

- Ignore document formatting markup, like:
  - <details><summary> HTML blocks
  - {=latex} codeblocks
  - [!tip] [!note] block quotes
  - code folding tags ("three curly braces pairs")
  - internal links: e.g. [^]

- At the end, provide, as Markdown footnotes, a list of references to
online documentation resources, linked to answer text where
appropriate. To avoid reference clashing with other part of the
document, prefix references with the string "rf-".

- Add any additional important information not explicitly required in
an "Additional Notes" section.


<details>
<summary></summary>

```{=latex}
\newpage
```

</details>

## Response Template

<details>
<summary>Example Markdown Structure:</summary>

TODO:(q1-template) ...

```markdown

## Overview

## Details

\`\`\`bash
#!/bin/bash

echo "$(date -isec) - (rc=${rc:-$?}) completed."

\`\`\`


```

</details>

# A:1 (Claude)

[^](#toc) **_TODO:(a1-ref-claude)_**

TODO:(a1-claude) ...

# A:1 (Gemini)

[^](#toc) **_TODO:(a1-ref-gemini)_**

TODO:(a1-gemini) ...

# A:1 (ChatGPT)

[^](#toc) **_TODO:(a1-ref-chatgpt)_**

TODO:(a1-chatgpt) ...

# A:1 (Perplexity)

[^](#toc) **_TODO:(a1-ref-perplexity)_**

TODO:(a1-perplexity) ...

## Q:1.2 (Perplexity)

[^](#toc) **_(=> continue)_**

TODO:(q1.2-perplexity) ...

---

## A:1.2 (Perplexity)

[^](#toc) **_(=> continue)_**

TODO:(a1.2-perplexity) ...

# A:1 (DeepSeek)

[^](#toc) **_TODO:(a1-ref-deepseek)_**

TODO:(a1-deepseek) ...

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

## Q:2.2 (Perplexity)

[^](#toc) **_(=> continue)_**

TODO:(q2.2-perplexity) ...

---

## A:2.2 (Perplexity)

[^](#toc) **_(=> continue)_**

TODO:(a2.2-perplexity) ...

# A:2 (DeepSeek)

[^](#toc) **_TODO:(a2-ref-deepseek)_**

TODO:(a2-deepseek) ...

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

# A:b

## A:b - **Q2: Prompt distiller**

[^](#toc)

## Appendix b

### User

Act as an expert Prompt Engineer and AI Optimisation Specialist. Your
objective is to analyse, critique, and significantly enhance the
user-provided prompt.

The prompt you need to refine begins immediately after the line
starting with /PROMPT/ marker.

### Your Process

#### Analysis & Evaluation

- Assess the original prompt for clarity, context, constraint
  definition, and logical flow.
- Identify specific weaknesses, such as ambiguity, grammatical errors,
  logic gaps, or lack of sufficient context.
- Determine if the prompt would benefit from specific engineering
  techniques (e.g., Chain-of-Thought, persona adoption, or few-shot
  examples).

#### Critique Presentation

- Provide a brief, professional evaluation of the original text.
- List specific issues found and explain why they are problematic.
- Propose concrete improvements to address these issues.

#### Optimisation

- Rewrite the prompt to be precise, fluent, and highly effective.
- Ensure the English style is professional and grammatically correct.
- Clarify all requirements and specifications to minimise the risk
of AI hallucination or misinterpretation.

### Note

- Ignore extra Markdown used in rendering pipelines: GitLab GLFM
  repository view and pandoc PDF transformations.
- Keep this Markdown unaltered in reply.

In particular, ignore:

- <details><summary> HTML blocks
- {=latex} codeblocks
- [!tip] [!note] block quotes
- code folding tags '{{{' and '}}}'
- internal links: e.g. [^]

### Important

> Generate 3 responses to this prompt, each with their probability.

### Output Format

- Present your analysis and critique first.
- Output the final, polished version of the prompt at the very end.
- Separate every refined prompt from the analysis using a horizontal line
  (`---`) and the line start marker `/PROMPT/`.

---
/PROMPT/

TODO:(a2-prompt) ...

# A:b (Claude)

[^](#toc) **_TODO:(ab-ref-claude)_**

TODO:(ab-claude) ...

# A:b (Gemini)

[^](#toc) **_TODO:(ab-ref-gemini)_**

TODO:(a2-gemini) ...

# A:b (ChatGPT)

[^](#toc) **_TODO:(ab-ref-chatgpt)_**

TODO:(ab-chatgpt) ...

# A:b (Perplexity)

[^](#toc) **_TODO:(ab-ref-perplexity)_**

TODO:(ab-perplexity) ...

# A:b (DeepSeek)

[^](#toc) **_TODO:(ab-ref-deepseek)_**

TODO:(ab-deepseek) ...



<!-- }}} \\ %a. -->
<!-- {{{ // %*
LocalWords:  GitLab CommonMark GFM GLFM YAML
vim: set foldmethod=marker :
}}} // %* -->
