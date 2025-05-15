* Encoding: UTF-8.
*test for an interaction between equation type (addition vs. subtraction) and target congruency (congruent vs. incongruent)

temporary.
select if awareness =1.
GLM addclrx addilrx subclrx subilrx
  /WSFACTOR=equation 2 Polynomial congruency 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=equation congruency equation*congruency.

*test for order effects

temporary.
select if awareness =1.
GLM addclrx addilrx subclrx subilrx BY block
  /WSFACTOR=equation 2 Polynomial congruency 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=equation congruency equation*congruency
  /DESIGN=block.

*test for priming effect by equation

temporary.
select if awareness =1.
T-TEST PAIRS=addilrx subilrx WITH addclrx subclrx (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*test for priming effect for excluded participants 

SORT CASES  BY awareness.
SPLIT FILE LAYERED BY awareness.

T-TEST PAIRS=addilrx subilrx WITH addclrx subclrx (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.
