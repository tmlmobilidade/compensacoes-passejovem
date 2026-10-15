param([string]$DocxPath)

Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem

if (-not $DocxPath -or -not (Test-Path $DocxPath)) {
    $envTarget = $env:QUARTO_PROJECT_OUTPUT_FILES
    if ($envTarget -and (Test-Path $envTarget)) {
        $files = @(Get-Item $envTarget)
    } else {
        $files = Get-ChildItem -Path "code/*.docx" -Exclude "*template*", "*pandoc*"
    }
} else {
    $files = @(Get-Item $DocxPath)
}

foreach ($f in $files) {
    if ($f.Extension -ne ".docx") { continue }
    try {
        $zip = [System.IO.Compression.ZipFile]::Open($f.FullName, [System.IO.Compression.ZipArchiveMode]::Update)
        $entry = $zip.GetEntry('word/document.xml')
        if (-not $entry) { $zip.Dispose(); continue }
        
        $reader = New-Object System.IO.StreamReader($entry.Open())
        $xml = $reader.ReadToEnd()
        $reader.Close()
        
        $matches = [regex]::Matches($xml, '<w:tbl[ >].*?</w:tbl>')
        $modified = $false
        
        for ($i = $matches.Count - 1; $i -ge 0; $i--) {
            $tbl = $matches[$i].Value
            $tblNew = $tbl
            
            if ($tbl.Contains('Passageiros') -and $tbl.Contains('Receitas') -and $tbl.Contains('jan/25')) {
                # Table 7: TabelaTextoPequeno e margens compactas
                $tblNew = $tbl.Replace('w:val="Compact"', 'w:val="TabelaTextoPequeno"')
                $tightMar = '<w:tblCellMar><w:top w:w="15" w:type="dxa"/><w:left w:w="40" w:type="dxa"/><w:bottom w:w="15" w:type="dxa"/><w:right w:w="40" w:type="dxa"/></w:tblCellMar>'
                if ($tblNew.Contains('<w:tblCellMar>')) {
                    $tblNew = [regex]::Replace($tblNew, '<w:tblCellMar>.*?</w:tblCellMar>', $tightMar)
                } else {
                    $tblNew = [regex]::Replace($tblNew, '(<w:tblLayout [^>]*/>)', "`$1$tightMar")
                }
            } elseif ($tbl.Contains('De:') -and $tbl.Contains('Para:')) {
                # Tabela de cabecalho (De e Para): 2 colunas sem bordas
                $tblNew = $tbl.Replace('w:val="Compact"', 'w:val="BlocoPequeno"')
                $tblNew = $tblNew.Replace('w:val="TabelaTexto"', 'w:val="BlocoPequeno"')
                $tblNew = [regex]::Replace($tblNew, '<w:tr[ >].*?<w:tblHeader/>.*?</w:tr>', '')
                $tblNew = [regex]::Replace($tblNew, '<w:tblStyle [^>]*/>', '')
                $tblNew = [regex]::Replace($tblNew, '<w:tcBorders>.*?</w:tcBorders>', '<w:tcBorders><w:top w:val="none"/><w:left w:val="none"/><w:bottom w:val="none"/><w:right w:val="none"/></w:tcBorders>')
                $noBrd = '<w:tblW w:type="pct" w:w="5000"/><w:tblBorders><w:top w:val="none"/><w:left w:val="none"/><w:bottom w:val="none"/><w:right w:val="none"/><w:insideH w:val="none"/><w:insideV w:val="none"/></w:tblBorders><w:tblLayout w:type="fixed"/><w:tblCellMar><w:top w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:bottom w:w="140" w:type="dxa"/><w:right w:w="120" w:type="dxa"/></w:tblCellMar><w:tblLook w:val="0000"/>'
                $tblNew = [regex]::Replace($tblNew, '<w:tblPr>.*?</w:tblPr>', "<w:tblPr>$noBrd</w:tblPr>")
            } else {
                # Tabelas normais: TabelaTexto (Calibri 8.5 pt)
                $tblNew = $tbl.Replace('w:val="Compact"', 'w:val="TabelaTexto"')
            }

            # Centrar tabelas de conteudo (nao cabecalho De/Para)
            if (-not ($tbl.Contains('De:') -and $tbl.Contains('Para:'))) {
                $tblNew = [regex]::Replace($tblNew, '(<w:tblPr[^>]*>)(.*?)(</w:tblPr>)', {
                    param($tm)
                    $tblPrContent = $tm.Groups[2].Value
                    if ($tblPrContent.Contains('<w:jc ')) {
                        $tblPrContent = [regex]::Replace($tblPrContent, '<w:jc [^>]*/>', '<w:jc w:val="center"/>')
                    } else {
                        $tblPrContent = '<w:jc w:val="center"/>' + $tblPrContent
                    }
                    return $tm.Groups[1].Value + $tblPrContent + $tm.Groups[3].Value
                }, [System.Text.RegularExpressions.RegexOptions]::Singleline)
            }
            
            if ($tblNew -ne $tbl) {
                $xml = $xml.Substring(0, $matches[$i].Index) + $tblNew + $xml.Substring($matches[$i].Index + $matches[$i].Length)
                $modified = $true
            }
        }

        # Centrar paragrafos que contem imagens/desenhos (<w:drawing>)
        $xmlNew = [regex]::Replace($xml, '(<w:p[ >].*?</w:p>)', {
            param($m)
            $p = $m.Groups[1].Value
            if ($p.Contains('<w:drawing>') -or $p.Contains('<w:drawing ')) {
                if ($p.Contains('<w:jc ')) {
                    return [regex]::Replace($p, '<w:jc [^>]*/>', '<w:jc w:val="center"/>')
                } elseif ($p.Contains('<w:pPr>')) {
                    return $p.Replace('<w:pPr>', '<w:pPr><w:jc w:val="center"/>')
                } elseif ($p.Contains('<w:pPr ')) {
                    return [regex]::Replace($p, '(<w:pPr [^>]*>)', '$1<w:jc w:val="center"/>')
                } else {
                    return [regex]::Replace($p, '(<w:p[ >])', '$1<w:pPr><w:jc w:val="center"/></w:pPr>')
                }
            }
            return $p
        }, [System.Text.RegularExpressions.RegexOptions]::Singleline)
        if ($xmlNew -ne $xml) {
            $xml = $xmlNew
            $modified = $true
        }

        # Redimensionar fig-1 para a largura da pagina landscape (~9.5 pol = 8686800 EMU, 3.8 pol = 3474720 EMU)
        $xmlBeforeFig = $xml
        $xml = [regex]::Replace($xml, '(<w:drawing[ >](?:(?!</w:drawing>).)*?fig-1(?:(?!</w:drawing>).)*?</w:drawing>)', {
            param($m)
            return [regex]::Replace($m.Groups[1].Value, 'cx="\d+" cy="\d+"', 'cx="8686800" cy="3474720"')
        }, [System.Text.RegularExpressions.RegexOptions]::Singleline)
        if ($xml -ne $xmlBeforeFig) {
            $modified = $true
        }

        # Espacamento vertical apos tabelas (no primeiro paragrafo a seguir a </w:tbl>)
        $xmlBeforeTblSpace = $xml
        $xml = [regex]::Replace($xml, '(</w:tbl>(?:\s*<w:bookmarkEnd[^>]*>)*\s*(?:<w:bookmarkStart[^>]*>)*\s*<w:p[ >][^>]*?<w:pPr[^>]*>)(.*?)(</w:pPr>)', {
            param($m)
            $pPr = $m.Groups[2].Value
            if ($pPr.Contains('BlocoPequeno')) {
                return $m.Value
            }
            if ($pPr.Contains('TabelaNota')) {
                if ($pPr.Contains('w:after="')) {
                    $pPr = [regex]::Replace($pPr, 'w:after="\d+"', 'w:after="200"')
                } else {
                    $pPr = $pPr + '<w:spacing w:before="60" w:after="200" w:line="240" w:lineRule="auto"/>'
                }
            } else {
                if ($pPr.Contains('w:before="')) {
                    $pPr = [regex]::Replace($pPr, 'w:before="\d+"', 'w:before="240"')
                } else {
                    $pPr = $pPr + '<w:spacing w:before="240" w:after="240" w:line="288" w:lineRule="auto"/>'
                }
            }
            return $m.Groups[1].Value + $pPr + $m.Groups[3].Value
        }, [System.Text.RegularExpressions.RegexOptions]::Singleline)
        if ($xml -ne $xmlBeforeTblSpace) {
            $modified = $true
        }

        # Limpar w:pPr duplicados em paragrafos que receberam pPr customizado via Lua
        if ($xml -match '<w:p[ >][^>]*?<w:pPr>.*?</w:pPr><w:pPr>') {
            $xml = [regex]::Replace($xml, '(<w:p[ >][^>]*?)<w:pPr>.*?</w:pPr><w:pPr>', '$1<w:pPr>')
            $modified = $true
        }
        
        if ($modified) {
            $entry.Delete()
            $newEntry = $zip.CreateEntry('word/document.xml')
            $writer = New-Object System.IO.StreamWriter($newEntry.Open(), [System.Text.Encoding]::UTF8)
            $writer.Write($xml)
            $writer.Close()
            Write-Host ("[OK] Pos-processamento concluido para: " + $f.Name)
        }
        $zip.Dispose()
    } catch {
        Write-Warning ("Aviso ao processar " + $f.Name + ": " + $_.Exception.ToString())
    }
}

