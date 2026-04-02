function Image(el)
  local attrs = el.attributes or {}
  local src = el.src or ""

  if attrs["height"] then
    return el
  end

  if src:match("^images/Faculty_logo%.png$") or src:match("^images/UWC%-logo%.jpg$") then
    return el
  end

  attrs["width"] = "90%"
  attrs["height"] = nil
  el.attributes = attrs
  return el
end
