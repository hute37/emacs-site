-- -------------------------------------------------------------------------
-- pandoc filter: _md-report_.lua
--
-- @see: _md-report_.tex # maketitle|titlepage
-- @see: _LLM_Template_.md # YAML preamble
-- @see: Makefile
---
-- @see: https://aistudio.google.com/app/prompts?state=%7B%22ids%22:%5B%221npksKeOyqJG2RI1HAYntbWZqSnyrXWtu%22%5D,%22action%22:%22open%22,%22userId%22:%22101040866152128307883%22,%22resourceKeys%22:%7B%7D%7D&usp=sharing
-- @see: https://claude.ai/share/697fb160-e546-414a-8241-325f8b33571e
-- @see: https://chatgpt.com/share/6939a036-8ba8-8012-819c-aac93afad04e
-- @see: https://www.perplexity.ai/search/using-pandoc-to-convert-a-mark-Ob9r4V.rQsucZ69mwuNuiw#0
-- @see: https://chat.deepseek.com/a/chat/s/087ffbc3-196f-4ee5-bf7d-5ae173567462
-- @see: https://claude.ai/share/b28ceb6a-409e-4908-8519-4393d47a706e
-- -------------------------------------------------------------------------

local logging = require("logging")

function Meta(m)
	logging.temp(">>> ", rawget(_G, "FORMAT"), "#/meta:", m)

	if FORMAT ~= 'latex' then
	  return m
	end

        -- 0. Get temp directory from metadata
	local temp_dir = pandoc.utils.stringify(m.tempdir or "/tmp")
	
	-- Use FIXED filename in temp directory
	local temp_file = temp_dir .. "/pandoc-meta-commands.tex"
	
	logging.temp(">>> Using temp file:", temp_file)
        
        local header_lines = {}
        
	-- 1. Handle Keywords
	if m.keywords then
	  local kw_list = {}
	  -- Convert the list of keywords into a comma-separated string
	  for _, item in ipairs(m.keywords) do
	    table.insert(kw_list, pandoc.utils.stringify(item))
	  end
	  local kw_string = table.concat(kw_list, ", ")

	  -- Inject \keywords{...} into the header-includes
	  local kw_cmd = "\\keywords{" .. kw_string .. "}"
          table.insert(header_lines, kw_cmd)
          
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(kw):", kw_cmd)
	end

	-- 2. Handle Abstract
        if m.abstract then
           local abstract_tex = pandoc.write(pandoc.Pandoc(m.abstract), 'latex')
           local abstract_cmd = "\\abstract{" .. abstract_tex .. "}"
           table.insert(header_lines, abstract_cmd)
           m.abstract = nil
           
        end

	-- 3. Handle Category
	if m.category then
	  local cat_string = pandoc.write(pandoc.Pandoc(m.category), 'latex')
	  local cat_cmd = "\\category{" .. cat_string .. "}"
          table.insert(header_lines, cat_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(cat):", cat_cmd)
	end

	-- Organization Name
	if m.orgname then
          local orgname_string = pandoc.utils.stringify(m.orgname)
	  local orgname_cmd = "\\orgname{" .. orgname_string .. "}"
          table.insert(header_lines, orgname_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(orgname):", orgname_cmd)
	end

	-- Organization Site
	if m.orgsite then
          local orgsite_string = pandoc.write(pandoc.Pandoc(m.orgsite), 'latex')
	  local orgsite_cmd = "\\orgsite{" .. orgsite_string .. "}"
          table.insert(header_lines, orgsite_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(orgsite):", orgsite_cmd)
	end

	-- 4. Project Name
	if m.project then
          local proj_string = pandoc.write(pandoc.Pandoc(m.project), 'latex')
	  local proj_cmd = "\\project{" .. proj_string .. "}"
          table.insert(header_lines, proj_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(project):", proj_cmd)
	end

	-- 5. Document Name
	if m.docname then
          local doc_string = pandoc.write(pandoc.Pandoc(m.docname), 'latex')
	  local doc_cmd = "\\docname{" .. doc_string .. "}"
          table.insert(header_lines, doc_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(doc):", doc_cmd)
	end

	-- 6. Base URL
	if m.baseurl then
          local url_string = pandoc.write(pandoc.Pandoc(m.baseurl), 'latex')
	  local url_cmd = "\\projecturl{" .. url_string .. "}"
          table.insert(header_lines, url_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(baseurl):", url_cmd)
	end

	-- 7. Document Directory
	if m.docsdir then
          local dir_string = pandoc.write(pandoc.Pandoc(m.docsdir), 'latex')
	  local dir_cmd = "\\docpath{" .. dir_string .. "}"
          table.insert(header_lines, dir_cmd)
          logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(dir):", dir_cmd)
	end

	-- 8. Document Dir URL
	if 1 == 1 then
           local baseurl_string = ""
           local docsdir_string = ""
           local docname_string = ""
           
           if m.baseurl then
              baseurl_string = pandoc.write(pandoc.Pandoc(m.baseurl), 'latex')  .. "/"
           end
           
           if m.docsdir then
              docsdir_string = pandoc.write(pandoc.Pandoc(m.docsdir), 'latex')  .. "/"
           end
        
           if m.docname then
              docname_string = pandoc.write(pandoc.Pandoc(m.docname), 'latex')
           end
           
           local docsurl_string = baseurl_string .. docsdir_string
           local docsurl_cmd = "\\docsurl{" .. docsurl_string .. "}"
           table.insert(header_lines, docsurl_cmd)
           logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(docsurl):", docsurl_cmd)
           
           local docurl_string = baseurl_string .. docsdir_string .. docname_string
           local docurl_cmd = "\\docurl{" .. docurl_string .. "}"
           table.insert(header_lines, docurl_cmd)
           logging.temp("+++ ", rawget(_G, "FORMAT"), "#/meta(docurl):", docurl_cmd)
	end

        -- z. Write to the unique temp file
        local f, err = io.open(temp_file, "w")
        if f then
           if #header_lines > 0 then
              local header_text = table.concat(header_lines, "\n") .. "\n"
              logging.temp("+++ Header Text:\n{{{\n", header_text, "\n}}}\n")
              f:write(header_text)
           else
              f:write("% No metadata commands\n")
           end
           f:close()
	  logging.temp("+++ Wrote to:", temp_file)
	else
	  logging.warning("!!! Could not write:", temp_file, err)
	end

	logging.temp("<<< ", rawget(_G, "FORMAT"), "#/meta:", m)
	return m
end

