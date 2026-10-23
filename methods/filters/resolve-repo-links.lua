-- Pandoc Lua filter to resolve relative paths (../code/, ../data/, ../documents/, etc.)
-- to full GitHub repository URLs when rendering the Quarto book.

local repo_base = "https://github.com/tmlmobilidade/compensacoes-passejovem"
local comp_base = "https://github.com/tmlmobilidade/compensacoes-navegante"
local branch = "main"

local patterns = {
  "^%.%./(code/.*)$",
  "^(code/.*)$",
  "^%.%./(data/.*)$",
  "^(data/.*)$",
  "^%.%./(documents/.*)$",
  "^(documents/.*)$",
  "^%.%./(Exemplos/.*)$",
  "^(Exemplos/.*)$",
  "^%.%./(README%.md.*)$",
  "^(README%.md.*)$",
  "^%.%./(procedure%.md.*)$",
  "^(procedure%.md.*)$"
}

local comp_patterns = {
  "^%.%./%.%./Compensacoes/(.*)$",
  "^%.%./Compensacoes/(.*)$",
  "^Compensacoes/(.*)$"
}

function Link(el)
  local target = el.target
  if not target then return el end

  for _, pat in ipairs(comp_patterns) do
    local match = target:match(pat)
    if match then
      local action = "blob"
      if match:sub(-1) == "/" then
        action = "tree"
      end
      el.target = string.format("%s/%s/%s/%s", comp_base, action, branch, match)
      return el
    end
  end

  for _, pat in ipairs(patterns) do
    local match = target:match(pat)
    if match then
      local action = "blob"
      if match:sub(-1) == "/" then
        action = "tree"
      end
      el.target = string.format("%s/%s/%s/%s", repo_base, action, branch, match)
      break
    end
  end

  return el
end

