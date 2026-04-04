local function has_class(el, class)
  return el.classes and el.classes:includes(class)
end

local unicode_replacements = {
  {"→", "$\\rightarrow$"},
  {"≈", "$\\approx$"},
  {"▁", "."},
  {"▂", ":"},
  {"▃", "-"},
  {"▄", "="},
  {"▅", "+"},
  {"▆", "*"},
  {"▇", "#"},
  {"█", "@"},
  {"⁻¹", "$^{-1}$"},
  {"⁻²", "$^{-2}$"},
  {"⁻³", "$^{-3}$"},
  {"₀", "$_0$"},
  {"₁", "$_1$"},
  {"₂", "$_2$"},
  {"₃", "$_3$"},
  {"₄", "$_4$"},
  {"₅", "$_5$"},
  {"₆", "$_6$"},
  {"₇", "$_7$"},
  {"₈", "$_8$"},
  {"₉", "$_9$"},
  {"⁰", "$^0$"},
  {"¹", "$^1$"},
  {"²", "$^2$"},
  {"³", "$^3$"},
  {"⁴", "$^4$"},
  {"⁵", "$^5$"},
  {"⁶", "$^6$"},
  {"⁷", "$^7$"},
  {"⁸", "$^8$"},
  {"⁹", "$^9$"},
  {"⁻", "$^{-}$"},
}

local function markdown_inlines(text)
  local doc = pandoc.read(text, "markdown")
  if #doc.blocks == 1 and doc.blocks[1].t == "Para" then
    return doc.blocks[1].content
  end
  return {pandoc.Str(text)}
end

local function stringify_caption(el)
  local source = el.caption and el.caption.long
  if source and #source > 0 then
    return pandoc.utils.stringify(source):lower()
  end
  return pandoc.utils.stringify(el):lower()
end

local function is_panelled_image(el)
  local text = stringify_caption(el)
  return text:match("panel")
    or text:match("panels")
    or text:match("side by side")
    or text:match("clockwise from top left")
end

local function cap_image_width(el)
  local target = is_panelled_image(el) and 80 or 50
  local width = el.attributes["width"]
  if not width or width == "" then
    el.attributes["width"] = string.format("%d%%", target)
    return el
  end

  local pct = width:match("^(%d+%.?%d*)%%$")
  if pct and tonumber(pct) and tonumber(pct) > target then
    el.attributes["width"] = string.format("%d%%", target)
    return el
  end

  return el
end

local function is_decorative_image(el)
  local text = stringify_caption(el)
  return text:match("meme")
    or text:match("comic")
    or text:match("cartoon")
    or text:match("phd comics")
end

function Div(el)
  if has_class(el, "content-visible") then
    local when = el.attributes["when-format"]
    if when and when:match("html") then
      return {}
    end
  end

  if has_class(el, "column-margin") then
    return el.content
  end

  return nil
end

function Str(el)
  local text = el.text
  local changed = false

  for _, pair in ipairs(unicode_replacements) do
    local updated, count = text:gsub(pair[1], pair[2])
    if count > 0 then
      text = updated
      changed = true
    end
  end

  if changed then
    return markdown_inlines(text)
  end

  return nil
end

function Image(el)
  if FORMAT:match("latex") or FORMAT:match("pdf") then
    if is_decorative_image(el) then
      return {}
    end
    return cap_image_width(el)
  end

  return nil
end
