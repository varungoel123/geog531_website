# GEOG 531: Quarto Reveal.js lecture

This lecture has 49 slides: title, 43 other core slides, then five optional reference slides. It spans the same two 75-minute classes, with Lab 1 and Lab 2 pauses built into the sequence. Speaker notes contain timing and teaching prompts.

## Open or edit

Unzip this folder first. Open `01_lecture.html` in a browser to use the supplied preview. The preview embeds all four figures but loads Reveal.js from jsDelivr, so its slideshow features need internet access. If Reveal.js cannot load, the file displays the slides as a reading page.

To edit, open `01_lecture.qmd` in RStudio or another Quarto editor. Keep `slides.css` and `assets/` beside it. Its header specifies `format: revealjs`. The R snippets are static examples and the tables use the previously checked numerical results, so this slide source does not need R to render. Refitting the models and updating displayed values is necessary if the dataset or model specification changes.

For a local preview, run:

```sh
quarto preview 01_lecture.qmd
```

For an HTML render:

```sh
quarto render 01_lecture.qmd --to revealjs
```

The source sets `embed-resources: true` for embedded dependencies in a native Quarto render. Use the arrow keys or Space to advance, Esc for overview and S for speaker view. Browser popup policies or local-file restrictions can affect speaker view; local Quarto preview serves the presentation over HTTP.

Expanded lecture reading notes are in `01_lecture_notes.html`. The two labs and data remain in the full instructor/student packages, which have also been updated with this slide version.

## Verification

The source front matter, slide structure, speaker-note blocks, figure paths, HTML dependencies and JavaScript syntax were checked. All displayed model results use the independently verified teaching dataset. Quarto and browser slideshow execution were unavailable in the creation environment. The supplied HTML was generated with Pandoc's Reveal.js writer; it is not a native Quarto render. Test the native Quarto preview on the teaching computer before class.
