
The output .xlsx is not correct, the values do not match the columns. See compensação, for instance, is not in the right place.
The july should be the reference to use this xlsx v2 template, as it requires to add the Tipo de Documento,	N.º DO DOCUMENTO and Data de nascimento, in the abscence of NIF. These values are only in the new PasseJovem_ValidaçõesLimpas.xlsx, in both sheets, as ID_DOC_NUMBER and ID_DOC_NUMBER_DESC
So, for the previous months, those fields in the output xlsx should be empty.

The "Categoria" variable should be create as:

If passe nome nas "Complemento", -> "B", otherwise "A".

update the procedure.md
