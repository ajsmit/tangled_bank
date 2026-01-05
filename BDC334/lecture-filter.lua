-- lecture-filter.lua
-- This filter adds LaTeX commands to change "Chapter" to "Lecture"

function Pandoc(doc)
  -- Add LaTeX commands at the very beginning of the document
  local latex_setup = pandoc.RawBlock('latex', [[\makeatletter
\renewcommand{\chaptername}{Lecture}
\renewcommand{\@chapapp}{Lecture}
\makeatother
]])

  -- Insert at the beginning of the document
  table.insert(doc.blocks, 1, latex_setup)

  return doc
end
