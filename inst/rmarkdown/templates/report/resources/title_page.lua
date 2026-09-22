--[[
  title_page.lua

  Pandoc Lua filter for the BCTU trial report template. Builds the styled
  title page, anchors a metadata table to the bottom-left of page 1,
  closes the title-page section, optionally inserts a Word TOC, replaces
  `::: landscape` fenced divs with section breaks, and numbers figure and
  table captions ("Figure 1: ...", "Table 1: ...") with the caption above
  the figure or table. Targets pandoc 3.x.

  The BCTU logo lives in `reference.docx` (page-anchored drawing in
  `word/header2.xml`), not here.
]]

local utils = pandoc.utils

local function is_metalist(v) return utils.type(v) == "List" end

local function is_metamap(v)
  if type(v) ~= "table" then return false end
  local pt = utils.type(v)
  if pt == "List" or pt == "Inlines" or pt == "Blocks" then return false end
  if v[1] ~= nil then return false end
  for k in pairs(v) do
    if type(k) == "string" then return true end
  end
  return false
end

local function as_string(v)
  if v == nil then return nil end
  local s = utils.stringify(v)
  if s == "" then return nil end
  return s
end

local function as_bool(v)
  if type(v) == "boolean" then return v end
  local s = as_string(v)
  if s == nil then return false end
  s = s:lower()
  return s == "true" or s == "yes" or s == "1"
end

local function esc(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"))
end

-- ---------------------------------------------------------------------
-- Metadata-cell rendering. Each entry renders as one <w:p> reading
-- `Name (Role)` (or just `Name`). Separate paragraphs (not <w:br/>)
-- because some Word versions collapse intra-paragraph breaks in cells.
-- ---------------------------------------------------------------------

local META_PPR =
  '<w:pPr><w:pStyle w:val="TitlePageMeta"/>' ..
  '<w:spacing w:before="0" w:after="0" w:line="276" w:lineRule="auto"/>' ..
  '</w:pPr>'

local function meta_para(s)
  return string.format(
    '<w:p>%s<w:r><w:t xml:space="preserve">%s</w:t></w:r></w:p>',
    META_PPR, esc(s))
end

local ROLE_KEYS = { "role", "roles", "affiliation", "affiliations" }

local function entry_line(item)
  if not is_metamap(item) then return as_string(item) end
  local nm = as_string(item.name)
  if nm == nil then return as_string(item) end
  local roles = {}
  for _, key in ipairs(ROLE_KEYS) do
    local v = item[key]
    if is_metalist(v) then
      for _, r in ipairs(v) do
        local s = as_string(r); if s then table.insert(roles, s) end
      end
    else
      local s = as_string(v); if s then table.insert(roles, s) end
    end
  end
  if #roles == 0 then return nm end
  return nm .. " (" .. table.concat(roles, ", ") .. ")"
end

local function people_to_cell(v)
  if v == nil then return nil end
  local items = is_metalist(v) and v or { v }
  local paragraphs = {}
  for _, item in ipairs(items) do
    local line = entry_line(item)
    if line then table.insert(paragraphs, meta_para(line)) end
  end
  if #paragraphs == 0 then return nil end
  return table.concat(paragraphs)
end

-- ---------------------------------------------------------------------
-- Title-page paragraphs, metadata table, section breaks, TOC.
-- ---------------------------------------------------------------------

local function styled_para(text, style)
  if text == nil then return nil end
  return pandoc.Div(
    { pandoc.Para({ pandoc.Str(text) }) },
    pandoc.Attr("", {}, { ["custom-style"] = style })
  )
end

-- Hidden, empty paragraph in the ReportSubtype style. When no report subtype
-- is supplied we still emit this so the running header's
-- STYLEREF "ReportSubtype" resolves to empty text (the header's IF field then
-- omits the separator) instead of Word's "Error! No text of specified style
-- in document." `vanish` keeps it off the visible title page.
local function report_subtype_anchor()
  return pandoc.RawBlock("openxml",
    '<w:p><w:pPr><w:pStyle w:val="ReportSubtype"/>' ..
    '<w:rPr><w:vanish/></w:rPr></w:pPr></w:p>')
end

local function meta_row_xml(label, cell_paragraphs)
  return string.format(
    '<w:tr>' ..
    '<w:tc><w:tcPr><w:tcW w:w="3000" w:type="dxa"/></w:tcPr>' ..
    '<w:p><w:pPr><w:pStyle w:val="TitlePageMeta"/></w:pPr>' ..
    '<w:r><w:rPr><w:rStyle w:val="TitlePageMetaLabel"/></w:rPr>' ..
    '<w:t xml:space="preserve">%s</w:t></w:r></w:p></w:tc>' ..
    '<w:tc><w:tcPr><w:tcW w:w="6360" w:type="dxa"/></w:tcPr>%s</w:tc>' ..
    '</w:tr>',
    esc(label), cell_paragraphs)
end

local function rule_row_xml(side)
  return '<w:tr><w:trPr><w:trHeight w:val="160" w:hRule="exact"/></w:trPr>' ..
         '<w:tc><w:tcPr><w:tcW w:w="9360" w:type="dxa"/>' ..
         '<w:gridSpan w:val="2"/></w:tcPr>' ..
         '<w:p><w:pPr><w:pBdr><w:' .. side ..
         ' w:val="single" w:sz="12" w:space="0" w:color="C59A00"/></w:pBdr>' ..
         '<w:spacing w:before="0" w:after="0" w:line="20" w:lineRule="exact"/>' ..
         '<w:rPr><w:sz w:val="2"/></w:rPr></w:pPr></w:p></w:tc></w:tr>'
end

local function build_meta_table_xml(rows)
  if #rows == 0 then return nil end
  local parts = { rule_row_xml("top") }
  for _, r in ipairs(rows) do
    table.insert(parts, meta_row_xml(r[1], r[2]))
  end
  table.insert(parts, rule_row_xml("bottom"))
  local xml =
    '<w:tbl><w:tblPr>' ..
    '<w:tblpPr w:vertAnchor="margin" w:horzAnchor="margin"' ..
    ' w:tblpYSpec="bottom" w:tblpXSpec="left"/>' ..
    '<w:tblOverlap w:val="never"/>' ..
    '<w:tblW w:w="9360" w:type="dxa"/>' ..
    '<w:tblLayout w:type="fixed"/>' ..
    '<w:tblCellMar>' ..
    '<w:top w:w="60" w:type="dxa"/><w:left w:w="0" w:type="dxa"/>' ..
    '<w:bottom w:w="60" w:type="dxa"/><w:right w:w="180" w:type="dxa"/>' ..
    '</w:tblCellMar></w:tblPr>' ..
    '<w:tblGrid><w:gridCol w:w="3000"/><w:gridCol w:w="6360"/></w:tblGrid>' ..
    table.concat(parts) ..
    '</w:tbl>'
  return pandoc.RawBlock("openxml", xml)
end

local function page_break()
  return pandoc.RawBlock("openxml",
    '<w:p><w:r><w:br w:type="page"/></w:r></w:p>')
end

-- A4 page size in twips: 11906 x 16838 (portrait) / 16838 x 11906 (landscape).
-- Header/footer references use the *same* rId names as reference.docx's body
-- sectPr (rIdHdr1/rIdHdr2/rIdFtr1/rIdFtr2). Pandoc renumbers those rIds when
-- it writes document.xml.rels and rewrites the same rIds in raw OOXML blocks,
-- so the references stay valid.
local function title_page_section_break()
  return pandoc.RawBlock("openxml",
    '<w:p><w:pPr><w:sectPr>' ..
    '<w:headerReference w:type="default" r:id="rIdHdr1"/>' ..
    '<w:headerReference w:type="first" r:id="rIdHdr2"/>' ..
    '<w:footerReference w:type="default" r:id="rIdFtr1"/>' ..
    '<w:footerReference w:type="first" r:id="rIdFtr2"/>' ..
    '<w:pgSz w:w="11906" w:h="16838"/>' ..
    '<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440"' ..
    ' w:header="708" w:footer="708" w:gutter="0"/>' ..
    '<w:cols w:space="708"/>' ..
    '<w:titlePg/>' ..
    '<w:docGrid w:linePitch="360"/>' ..
    '</w:sectPr></w:pPr></w:p>')
end

local function body_section_break(orient)
  local pg_sz = orient == "landscape"
    and '<w:pgSz w:w="16838" w:h="11906" w:orient="landscape"/>'
    or  '<w:pgSz w:w="11906" w:h="16838"/>'
  return pandoc.RawBlock("openxml",
    '<w:p><w:pPr><w:sectPr>' ..
    '<w:headerReference w:type="default" r:id="rIdHdr1"/>' ..
    '<w:footerReference w:type="default" r:id="rIdFtr1"/>' ..
    pg_sz ..
    '<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440"' ..
    ' w:header="708" w:footer="708" w:gutter="0"/>' ..
    '<w:cols w:space="708"/>' ..
    '<w:docGrid w:linePitch="360"/>' ..
    '</w:sectPr></w:pPr></w:p>')
end

local function toc_block(title, depth)
  return pandoc.RawBlock("openxml",
    '<w:p><w:pPr><w:pStyle w:val="TOCHeading"/></w:pPr>' ..
    '<w:r><w:t xml:space="preserve">' .. esc(title) .. '</w:t></w:r></w:p>' ..
    '<w:p>' ..
    '<w:r><w:fldChar w:fldCharType="begin" w:dirty="true"/></w:r>' ..
    '<w:r><w:instrText xml:space="preserve">TOC \\o &quot;1-' .. depth ..
    '&quot; \\h \\z \\u</w:instrText></w:r>' ..
    '<w:r><w:fldChar w:fldCharType="separate"/></w:r>' ..
    '<w:r><w:t xml:space="preserve">Right-click and choose &quot;Update Field&quot; to populate this table of contents.</w:t></w:r>' ..
    '<w:r><w:fldChar w:fldCharType="end"/></w:r>' ..
    '</w:p>')
end

local function confidential_page(text)
  return pandoc.RawBlock("openxml",
    '<w:p><w:pPr><w:jc w:val="center"/></w:pPr>' ..
    '<w:r><w:rPr><w:b/></w:rPr><w:t xml:space="preserve">' .. esc(text) ..
    '</w:t></w:r></w:p>' ..
    '<w:p><w:pPr><w:sectPr>' ..
    '<w:headerReference w:type="default" r:id="rIdHdr1"/>' ..
    '<w:footerReference w:type="default" r:id="rIdFtr1"/>' ..
    '<w:pgSz w:w="11906" w:h="16838"/>' ..
    '<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440"' ..
    ' w:header="708" w:footer="708" w:gutter="0"/>' ..
    '<w:cols w:space="708"/>' ..
    '<w:vAlign w:val="center"/>' ..
    '<w:docGrid w:linePitch="360"/>' ..
    '</w:sectPr></w:pPr></w:p>')
end

local function empty_para()
  return pandoc.RawBlock("openxml", '<w:p/>')
end

-- ---------------------------------------------------------------------
-- AST walker: landscape sections + numbered captions above content.
--
-- - `::: landscape` divs are wrapped in section breaks so their content
--   renders in landscape (the inversion is because a sectPr in a paragraph
--   defines the section ENDING at that paragraph, not the one starting).
-- - Figures and Tables get sequential numbering and the caption is moved
--   above the content as a paragraph styled "ImageCaption" (figures) or
--   "TableCaption" (tables) -- both styles already live in reference.docx.
-- ---------------------------------------------------------------------

local function caption_inlines(prefix, n, original)
  local out = pandoc.Inlines({
    pandoc.Strong({ pandoc.Str(prefix .. " " .. tostring(n) .. ":") }),
    pandoc.Space(),
  })
  for _, inl in ipairs(original) do out:insert(inl) end
  return out
end

local function captioned_para(style, inlines)
  return pandoc.Div(
    { pandoc.Para(inlines) },
    pandoc.Attr("", {}, { ["custom-style"] = style })
  )
end

local function is_landscape_div(b)
  return b ~= nil and b.t == "Div" and b.classes:includes("landscape")
end

local function is_page_break(b)
  if b == nil or b.t ~= "RawBlock" then return false end
  local fmt = tostring(b.format)
  if fmt == "openxml" then
    return b.text:find('w:br w:type="page"', 1, true) ~= nil
  end
  if fmt == "tex" or fmt == "latex" then
    return b.text:match("^%s*\\newpage%s*$") ~= nil
      or b.text:match("^%s*\\pagebreak%s*$") ~= nil
  end
  return false
end

local function is_blank(b)
  if b == nil then return false end
  if b.t == "Null" then return true end
  return (b.t == "Para" or b.t == "Plain") and #b.content == 0
end

local function next_content(blocks, i)
  while is_blank(blocks[i]) do i = i + 1 end
  return i
end

-- Index of the next content block, stepping over one page break.
local function skip_page_break(blocks, i)
  local j = next_content(blocks, i)
  if is_page_break(blocks[j]) then return next_content(blocks, j + 1) end
  return j
end

local function process_blocks(blocks, counters)
  local out = pandoc.Blocks({})
  local i = next_content(blocks, 1)
  while blocks[i] ~= nil do
    local b = blocks[i]
    if is_page_break(b) and is_landscape_div(blocks[skip_page_break(blocks, i)]) then
      i = next_content(blocks, i + 1)
    elseif is_landscape_div(b) then
      out:insert(body_section_break("portrait"))
      local j = i
      local following = skip_page_break(blocks, j + 1)
      while true do
        for _, inner in ipairs(process_blocks(blocks[j].content, counters)) do
          out:insert(inner)
        end
        if is_landscape_div(blocks[following]) then
          out:insert(page_break())
          j = following
          following = skip_page_break(blocks, j + 1)
        else
          break
        end
      end
      out:insert(body_section_break("landscape"))
      i = following
    elseif b.t == "Figure" then
      counters.fig = counters.fig + 1
      local cap_inlines = b.caption and b.caption.long
        and utils.blocks_to_inlines(b.caption.long) or pandoc.Inlines({})
      out:insert(captioned_para("ImageCaption",
        caption_inlines("Figure", counters.fig, cap_inlines)))
      b.caption = pandoc.Caption()
      out:insert(b)
      i = next_content(blocks, i + 1)
    elseif b.t == "Table" then
      counters.tbl = counters.tbl + 1
      local cap_inlines = b.caption and b.caption.long
        and utils.blocks_to_inlines(b.caption.long) or pandoc.Inlines({})
      out:insert(captioned_para("TableCaption",
        caption_inlines("Table", counters.tbl, cap_inlines)))
      b.caption = pandoc.Caption()
      out:insert(b)
      out:insert(empty_para())
      i = next_content(blocks, i + 1)
    else
      out:insert(b)
      i = next_content(blocks, i + 1)
    end
  end
  return out
end

-- ---------------------------------------------------------------------
-- Metadata-table rows. YAML shape:
--   metadata:
--     - Date: "2026-05-14"
--     - Prepared by:
--         - name: Jane Statistician
--           role: Senior Statistician
-- Row values may be a string, a list of strings, a { name, role } map,
-- or a list of such maps.
-- ---------------------------------------------------------------------

local function row_from_entry(entry)
  if not is_metamap(entry) then return nil end
  for k, v in pairs(entry) do
    if type(k) == "string" then
      local cell = people_to_cell(v)
      if cell then return { k, cell } end
    end
  end
  return nil
end

local function build_meta_rows(m)
  local rows = {}
  if m["metadata"] == nil then return rows end
  for _, entry in ipairs(m["metadata"]) do
    local r = row_from_entry(entry)
    if r then table.insert(rows, r) end
  end
  return rows
end

-- ---------------------------------------------------------------------
-- Main entry point.
-- ---------------------------------------------------------------------

local TITLE_PARAS = {
  { "trial-short-name",   "TitleAcronym"      },
  { "trial-long-name",    "TitleFullName"     },
  { "trial-registration", "TrialRegistration" },
  { "report-type",        "ReportType"        },
  { "report-subtype",     "ReportSubtype"     },
}

function Pandoc(doc)
  local m = doc.meta

  local blocks = pandoc.Blocks({})
  for _, p in ipairs(TITLE_PARAS) do
    local b = styled_para(as_string(m[p[1]]), p[2])
    if b then
      blocks:insert(b)
    elseif p[2] == "ReportSubtype" then
      blocks:insert(report_subtype_anchor())
    end
  end

  local meta_tbl = build_meta_table_xml(build_meta_rows(m))
  if meta_tbl then blocks:insert(meta_tbl) end

  blocks:insert(title_page_section_break())

  local confidential = as_string(m["confidential"])
  if confidential then blocks:insert(confidential_page(confidential)) end

  if as_bool(m["include-toc"]) then
    local depth = tonumber(as_string(m["toc-depth"])) or 3
    local title = as_string(m["toc-title"]) or "Table of Contents"
    blocks:insert(toc_block(title, depth))
    blocks:insert(page_break())
  end

  local counters = { fig = 0, tbl = 0 }
  for _, b in ipairs(process_blocks(doc.blocks, counters)) do
    blocks:insert(b)
  end

  -- Strip Pandoc's built-in metadata keys so it doesn't render its own
  -- title block or author/date paragraph.
  m.title, m.subtitle, m.author, m.date = nil, nil, nil, nil
  m.toc, m["toc-title"] = false, nil

  return pandoc.Pandoc(blocks, m)
end
