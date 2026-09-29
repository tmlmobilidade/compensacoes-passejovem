
The output .xlsx is not correct, the values do not match the columns. See compensação, for instance, is not in the right place.
The july should be the reference to use this xlsx v2 template, as it requires to add the Tipo de Documento,	N.º DO DOCUMENTO and Data de nascimento, in the abscence of NIF. These values are only in the new PasseJovem_ValidaçõesLimpas.xlsx, in both sheets, as ID_DOC_NUMBER and ID_DOC_NUMBER_DESC
So, for the previous months, those fields in the output xlsx should be empty.

* The "Categoria" variable should be create as:
  If passe nome nas "Complemento", -> "B", otherwise "A".

* [ ]  update the procedure.md
* [ ]  use more dplyr functions, such in filters and so, instead base R (more easier to read for me)

So, again, I was out of tokens in the middle of your last thinking, and I was trying to fix it myself and run some tests. I messed up with code\01_preparar_dados.R L199-226.

But I think i found the missing piece.
We recieve data from people that can actually be older than 23, as long as they are still students. We don't have a way to confirm if they are or not (that is not the point), but we know for sure that someone with 25 years old turned in the previous months, cannot be in the list for compensation.

So, my suggestion is to use a ano-mes rule, and discard (ignore) the day in the rule.
so if, for july 2026, someone was born in june 2001 it is inegible (age = 25), but born in july 2001 is still elibigble (consider age = 24), and then we add a rule to clean the ones with age >=25.
For the kids  with 3 years old, we also need to consider someone who turns 4 in that month (any day), is 4 years age, not 3 (and I think that here lies the largest dicrepancy of our results and my colleague). And then we discard the ones with age = 3.

and with this i think we will fix all.
please fix the code, and the `nok` should be the ones that we discarded with the 25 and 3 of age.
(update also this rule in the procedure).
