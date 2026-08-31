# Tangled Bank Reader layer

The Reader layer implements the design direction developed from the reference
sites in the theme brief. It is deliberately separate from `strata.scss`:
STRATA supplies the type, colour tokens, and core Quarto components; Reader
adds focused reading and teaching behaviour.

## Files

- `reader.scss`: ocean global navigation, right-rail progress, teaching
  components, joined code and output, wide-figure utilities, compact cookies,
  and responsive combined contents.
- `reader-landing.scss`: course hero, actions, at-a-glance facts, and learning
  path.
- `../scripts/reader-experience.js`: creates the progress indicator and builds
  the mobile “Chapter contents” panel from Quarto's existing rails.

## Teaching components

Use a fenced div with `tb-teaching` and one semantic class:

```markdown
::: {.tb-teaching .tb-key-concept}
The content of the key concept.
:::
```

Available variants are:

- `tb-learning-objectives`
- `tb-required-material`
- `tb-key-concept`
- `tb-worked-example`
- `tb-interpretation`
- `tb-ecological-context`
- `tb-check-understanding`
- `tb-common-mistake`
- `tb-assessment-information`
- `tb-further-reading`

For existing Quarto callouts, add the semantic class alongside the callout
class, for example `{.callout-warning .tb-common-mistake}`.

## Wide figures and interactions

Add `tb-figure-wide` to a figure container when it should extend beyond the
reading measure. Use `tb-interactive-wide` for a larger interactive figure.
Both return to the normal content width below the desktop breakpoint.
