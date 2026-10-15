if not FORMAT:match("docx") then
  return {}
end

local pass1 = {
  Table = function(tbl)
    -- 1. Desenvelopar o contentor exterior do Quarto
    if #tbl.head.rows == 0 and #tbl.colspecs == 1 and tbl.colspecs[1][1] == pandoc.AlignCenter then
      local body = tbl.bodies[1]
      if body and #body.body == 1 and #body.body[1].cells == 1 then
        return body.body[1].cells[1].contents
      end
    end

    -- 2. Processar corpos: linha TOTAL e variação 'vs'
    for _, body in ipairs(tbl.bodies) do
      for _, row in ipairs(body.body) do
        local is_total = false
        local is_var = false
        for _, cell in ipairs(row.cells) do
          local txt = pandoc.utils.stringify(cell)
          if txt:match("^TOTAL") then
            is_total = true
            break
          elseif txt:match("%s+vs%s+") then
            is_var = true
            break
          end
        end
        if is_total then
          for _, cell in ipairs(row.cells) do
            local raw_tc = pandoc.RawBlock(
              "openxml",
              '<w:tcPr><w:tcBorders><w:top w:val="single" w:sz="8" w:space="0" w:color="000000"/></w:tcBorders></w:tcPr>'
            )
            cell.contents:insert(1, raw_tc)
          end
        elseif is_var then
          for _, cell in ipairs(row.cells) do
            local raw_tc = pandoc.RawBlock(
              "openxml",
              '<w:tcPr><w:tcBorders><w:top w:val="single" w:sz="8" w:space="0" w:color="000000"/><w:bottom w:val="single" w:sz="8" w:space="0" w:color="000000"/></w:tcBorders></w:tcPr>'
            )
            cell.contents:insert(1, raw_tc)
          end
        end
      end
    end
    return tbl
  end,

  Para = function(p)
    -- 3. Centrar legendas do Quarto
    for _, inl in ipairs(p.content) do
      if inl.t == "RawInline" and inl.format == "openxml" then
        if inl.text:match("ImageCaption") or inl.text:match("TableCaption") then
          inl.text = inl.text:gsub('<w:jc w:val="left"/>', '<w:jc w:val="center"/>')
        end
      end
    end
    return p
  end,

  BlockQuote = function(bq)
    -- 5. Citações (Blockquotes): itálico, cinzento escuro (#404040), indentação e barra à esquerda
    local out = pandoc.List()
    local num_blocks = #bq.content
    for blk_idx, blk in ipairs(bq.content) do
      if blk.t == "BulletList" then
        local num_items = #blk.content
        for item_idx, item in ipairs(blk.content) do
          local is_last = (blk_idx == num_blocks and item_idx == num_items)
          local spacing = is_last and '<w:spacing w:before="40" w:after="240" w:line="260" w:lineRule="auto"/>'
                                  or '<w:spacing w:before="40" w:after="40" w:line="260" w:lineRule="auto"/>'
          for _, subblk in ipairs(item) do
            if subblk.t == "Para" or subblk.t == "Plain" then
              local raw_p = pandoc.RawInline(
                "openxml",
                '<w:pPr><w:pStyle w:val="BlockText"/><w:pBdr><w:left w:val="single" w:sz="12" w:space="12" w:color="B0B0B0"/></w:pBdr>' .. spacing .. '<w:numPr><w:ilvl w:val="0"/><w:numId w:val="4"/></w:numPr><w:ind w:left="920" w:hanging="360" w:right="360"/><w:rPr><w:i/><w:iCs/><w:color w:val="404040"/></w:rPr></w:pPr>'
              )
              local inlines = pandoc.List({ raw_p })
              for _, inl in ipairs(subblk.content) do
                inlines:insert(inl)
              end
              out:insert(pandoc.Para(inlines))
            else
              out:insert(subblk)
            end
          end
        end
      elseif blk.t == "Para" or blk.t == "Plain" then
        local is_last = (blk_idx == num_blocks)
        local spacing = is_last and '<w:spacing w:before="80" w:after="240" w:line="260" w:lineRule="auto"/>'
                                or '<w:spacing w:before="80" w:after="80" w:line="260" w:lineRule="auto"/>'
        local raw_p = pandoc.RawInline(
          "openxml",
          '<w:pPr><w:pStyle w:val="BlockText"/><w:pBdr><w:left w:val="single" w:sz="12" w:space="12" w:color="B0B0B0"/></w:pBdr>' .. spacing .. '<w:ind w:left="720" w:right="360"/><w:rPr><w:i/><w:iCs/><w:color w:val="404040"/></w:rPr></w:pPr>'
        )
        local inlines = pandoc.List({ raw_p })
        for _, inl in ipairs(blk.content) do
          inlines:insert(inl)
        end
        out:insert(pandoc.Para(inlines))
      else
        out:insert(blk)
      end
    end
    return out
  end
}

local pass2 = {
  BulletList = function(bl)
    -- 4. Listas com marcas (bullet points) normais sem indentação excessiva
    local out = pandoc.List()
    local raw_p = pandoc.RawInline(
      "openxml",
      '<w:pPr><w:pStyle w:val="PargrafodaLista"/><w:numPr><w:ilvl w:val="0"/><w:numId w:val="4"/></w:numPr><w:ind w:left="360" w:hanging="360"/></w:pPr>'
    )
    for _, item in ipairs(bl.content) do
      for _, block in ipairs(item) do
        if block.t == "Para" or block.t == "Plain" then
          local inlines = pandoc.List({ raw_p })
          for _, inl in ipairs(block.content) do
            inlines:insert(inl)
          end
          out:insert(pandoc.Para(inlines))
        else
          out:insert(block)
        end
      end
    end
    return out
  end
}

return { pass1, pass2 }
