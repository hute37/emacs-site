---
title: /todo(title)
subtitle: /todo(subtitle)
# ::{{{ // %+

category: LLM-Style
keywords: [documentation, template, pandoc, latex, example]
abstract: |
  /todo(abstract)
  example document with extended abstract and multiple paragraphs.

  this is for **bold** and _emphasis_ markdown formatting,
  this is for inline `code` and math: $V = \frac{4}{3} \pi r^3$,
  and this is for [pandoc lua filters](https://pandoc.org/lua-filters.html)
  markdown links.

doctype: md-report

# ::}}} // %+
---
<!-- ::{{{ #TAG: /todo(toc) // -->

<!-- markdownlint-disable MD012 -->
<!-- markdownlint-disable MD025 -->
<!-- markdownlint-disable MD033 -->
<!-- markdownlint-disable MD051 -->

# TOC

1. [Q:1 - /todo(q1-ref)](#q1)
   - see: [/todo(a1-ref-claude)  (Claude)](#a1-claude)
   - see: [/todo(a1-ref-gemini) (Gemini)](#a1-gemini)
   - see: [/todo(a1-ref-chatgpt) (ChatGPT)](#a1-chatgpt)
   - see: [/todo(a1-ref-perplexity)  (Perplexity)](#a1-perplexity)
   - see: [/todo(a1-ref-deepseek)  (DeepSeek)](#a1-deepseek)

<details>
<summary></summary>

```{=latex}
\begin{comment}
```

</details>

---

|                   |                        |
|-------------------|------------------------|
| [<<<<](README.md) | [PDF](/todo(file).pdf) |

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
<!-- ::{{{ #TAG: /todo(q1-section) // -->

# Q:1

## Q:1 - **/todo(q1-title)**

[|^|](#toc)

/todo(q1-intro) ...

### Raw LaTeX embedding example

By default, Pandoc will preserve raw LaTeX code in Markdown documents
when converting the document to LaTeX, so you can use LaTeX commands
or environments in Markdown. However, sometimes your LaTeX code might
be too complex for Pandoc to parse, in which case Pandoc will treat
the content as normal Markdown. The consequence is that special LaTeX
characters may be escaped, e.g., Pandoc may convert a backslash `\\` to
`\\textbackslash{}`.

To make sure that Pandoc does not touch the raw LaTeX code in your
Markdown document, you may wrap the code in a fenced code block with
the attribute =latex, e.g.,

```{=latex}
\begin{tabular}{ll}
A & B \\
A & B \\
\end{tabular}
```

Do not forget the equal sign before `latex`, i.e., it is `=latex` instead
of `latex`. This feature requires a Pandoc version higher than 2.0
(check `rmarkdown::pandoc_version()`).

test inline `++ aaa ++`, `// => -> >= <= !=`
> test quote `++ aaa ++`, `// => -> >= <= !=`

```cpp

// test ligatures
// ++ ++ -- || &&
// => -> >= <= !=

int main(char[] argv) {
      return 0;
}

```

read the f*g [manual](https://pandoc.org/MANUAL.html)...

as someone said[^101]

[^101]: RTFM [rtfm-1]

- [rtfm-1]: aaa -> [Wikipedia:RTFM](https://en.wikipedia.org/wiki/RTFM)

### pandoc YAML front matter processing issue for gfm input format

With `pandoc`, converting a markdown file to pdf show a different
behavior on treatment of YAML front matters depending on the input format specified:

1. `markdown`: YAML preamble is omitted by the final output, as
   desired, but "GitHub Flavored Markdown (GFM)" option are not
   active.
2. `gfm+yaml_metadata_block`: the pdf in produced with GFM options
   enabled, as requested, but with an unwanted effect: YAML preamble
   is included in final pdf and the initial "Table of Contents" is
   suppressed
3. `gfm`: with this option, that should indicate exactly what is
   requested: GFM markdown enabled, YAML omitted and Table of Content
   included, the generation failed, with no pdf generation

In case 3, the problem is related to the wrong interpretation of this
part of YAML preamble:

````yaml
---
# omitted ...

header-includes:
- |
  ```{=latex}
  \input{../_styles/_md-report_.tex}
  ```
  
# omitted ...

---
````

This get translated in LaTeX markup instead of verbatim inclusion in
TeX source, passed to `LuaLaTeX` pdf engine.

```latex

header-includes:

\begin{itemize}
\tightlist
\item
  \textbar{}

\begin{Shaded}
\begin{Highlighting}[]
\NormalTok{\textbackslash{}input\{../\_styles/\_md{-}report\_.tex\}}
\end{Highlighting}
\end{Shaded}
\end{itemize}


```

This is likely a bug in `gfm` format processing.
How to resolve the issue, even with some workaround?

<details>
<summary></summary>

```{=latex}
\newpage
```

</details>

## System Hints

>>> [!tip]

/todo(q1-system)

As ...

Your task ...

Reply in clear formatted "GitLab Flavored Markdown (GLFM)" markdown,
with precise (lint) validation (codeblock delimiters ``` placed at line start).

At the end, provide, as Markdown footnotes, a list of references to
online documentation resources, linked to answer as citation where
appropriate.

Add any additional important information not explicitly required in an
"Additional Notes" section

>>>

<details>
<summary></summary>

```{=latex}
\newpage
```

</details>

## Question Prompt

/todo(q1-prompt) ...

<details>
<summary></summary>

```{=latex}
\newpage
```

</details>

## Response Template

<details>
<summary>Example Markdown Structure:</summary>

/todo(q1-template) ...

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

[|^|](#toc) **_/todo(a1-ref-claude)_**

/todo(a1-claude) ...

# A:1 (Gemini)

[|^|](#toc) **_/todo(a1-ref-gemini)_**

/todo(a1-gemini) ...

# A:1 (ChatGPT)

[|^|](#toc) **_/todo(a1-ref-chatgpt)_**

/todo(a1-chatgpt) ...

# A:1 (Perplexity)

[|^|](#toc) **_/todo(a1-ref-perplexity)_**

/todo(a1-perplexity) ...

## Q:1.2 (Perplexity)

[|^|](#toc) **_(=> continue)_**

/todo(q1.2-perplexity) ...

---

## A:1.2 (Perplexity)

[|^|](#toc) **_(=> continue)_**

/todo(a1.2-perplexity) ...

# A:1 (DeepSeek)

[|^|](#toc) **_/todo(a1-ref-deepseek)_**

/todo(a1-deepseek) ...

<!-- ::}}} \\ %1. -->
<!-- ::{{{ // %*
LocalWords:  GitLab CommonMark GFM GLFM YAML
vim: set foldmethod=marker :
::}}} // %* -->
