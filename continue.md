The output .xlsx is not correct, the values do not match the columns. See compensação, for instance, is not in the right place.
The july should be the reference to use this xlsx v2 template, as it requires to add the Tipo de Documento,	N.º DO DOCUMENTO and Data de nascimento, in the abscence of NIF. These values are only in the new PasseJovem_ValidaçõesLimpas.xlsx, in both sheets, as ID_DOC_NUMBER and ID_DOC_NUMBER_DESC
So, for the previous months, those fields in the output xlsx should be empty.

It is possible that the results do not match by 1 or 2 (between the output ant the examples). In that case, please export a csv with the row that is not matching, so i can investigate.

update the procedure.md

I am not sure if it is in the rules, but in case that we have a nif with 2 ticket codes, for instance one with 20 validations in Viver Cascais and 3 validations in Navegante metropolitano, we sum them and allocate to Metropoliano (always the one with larger territorial coverage, that costs more)

COnfirm if the rule of having up to 23 years old (included) is always refered to the month in observation. So, for july, we consider that it is valid if a kid turns 24 in the beginning of the month and keeps using it until the last day of that month.